#lang at-exp racket/base

(require json
         racket/contract/base
         racket/format
         racket/function
         racket/list
         racket/match
         xml)

(provide (all-from-out json
                       xml)
         flexpr?
         plural-symbol?
         singular-symbol/c
         (contract-out [current-singular-symbol (parameter/c singular-symbol/c)]
                       [default-singular-symbol singular-symbol/c])
         flexpr->xexpr
         write-flexpr-xml/content
         display-flexpr-xml/content
         flexpr->jsexpr
         write-flexpr-json)

(module+ test
  (require rackunit))


;;; Plural and singular symbols

(define singular-symbol/c (-> symbol? (or/c symbol? #f)))

(define (default-singular-symbol v) ;singular-symbol/c
  (and (symbol? v)
       (let ([s (symbol->string v)])
         (and (eq? #\s (string-ref s (sub1 (string-length s))))
              (string->symbol (substring s 0 (sub1 (string-length s))))))))

(define current-singular-symbol (make-parameter default-singular-symbol))

(define (plural-symbol? v) ;any -> boolean?
  (and ((current-singular-symbol) v) #t))


;;; flexpr? predicate

(define (flexpr? v) ;any -> boolean?
  (match v
    [(? boolean?) #t]
    [(? string?) #t]
    [(? exact-integer?) #t]
    [(? inexact-real?) #t]
    [(and (? hash?) (? hash-eq? ht))
     (for/and ([(k v) (in-hash ht)])
       (match* (k v)
         [((? plural-symbol?) (list (? flexpr?) ...)) #t]
         [((? symbol?)        (? flexpr?)           ) #t]
         [(_                  _                     ) #f]))]
    [_ #f]))

(module+ test
  (define-syntax-rule (check-flexpr? v)     (check-true (flexpr? v)))
  (define-syntax-rule (check-not-flexpr? v) (check-false (flexpr? v)))

  ;; Things that ARE flexprs
  (check-flexpr? (hasheq))
  (check-flexpr? (hasheq 'a 0))
  (check-flexpr? (hasheq 'a 0 'b 1))
  (check-flexpr? (hasheq 'a (hasheq 'b 0)))

  ;; Things that are NOT flexprs
  (check-not-flexpr? 'symbol)
  (check-not-flexpr? #"bytes")

  ;; A list by itself is NOT a flexpr
  (check-not-flexpr? '(1 2 3))
  ;; A list CAN be the value in a hash where its key is plural-symbol?
  (check-flexpr? (hasheq 'results '(1 2 3)))
  (check-flexpr? (hasheq 'results '(1 2 3) 'other 0))
  (check-not-flexpr? (hasheq 'singular '(1 2 3))))


;;; Conversion to xexpr / xml

;; Note: A contract with flexpr? would double traverse, don't need.
(define (flexpr->xexpr v #:root [root 'Response])
  (unless (symbol? root)
    (raise-argument-error 'flexpr->xexpr "symbol" 1 v root))
  (list* root
         (list)
         (let f->x ([v v])
           (match v
             [#t                   (list "true")]
             [#f                   (list "false")]
             [(? string? v)        (list v)]
             [(? exact-integer? v) (list (~a v))]
             [(? inexact-real? v)  (list (~a v))]
             [(and (? hash?) (? hash-eq? ht))
              ;; Important: Output the elements in a order that isn't
              ;; subject to `in-hash` or `hash-keys` order changing across
              ;; various versions of Racket.
              (for/list ([k (in-list (sort (hash-keys ht) symbol<?))])
                (define v (hash-ref ht k))
                (match* (k v)
                  [((? plural-symbol? plural) (? list? vs))
                   (define singular ((current-singular-symbol) plural))
                   (list* plural (list)
                          (for/list ([v (in-list vs)])
                            (list* singular (list)
                                   (f->x v))))]
                  [((? symbol? s) (? list? vs))
                   (raise-arguments-error
                    'flexpr->xexpr
                    "hash table key must be plural-symbol?"
                    "expected" (string->symbol (format "~as" s))
                    "given" s
                    "in" ht)]
                  [((? symbol? k) v) (list* k (list) (f->x v))]
                  [(k _) (raise-argument-error 'flexpr->xexpr "symbol" k)]))]
             [v (raise-argument-error 'flexpr->xexpr "flexpr" v)]))))

(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b)))

(module+ test
  (check-false (flexpr? (hasheq 'item (list 0 1 2))))
  (check-exn #px"hash table key must be plural-symbol?"
             (λ () (flexpr->xexpr (hasheq 'item (list 0 1 2))))))

(define (write-flexpr-xml/content v [out (current-output-port)])
  (void (write-xml/content (xexpr->xml (flexpr->xexpr v))
                           out)))

(define (display-flexpr-xml/content v [out (current-output-port)])
  (void (display-xml/content (xexpr->xml (flexpr->xexpr v))
                             out)))


;;; Conversion to jsexpr / json

(define (flexpr->jsexpr v) ;flexpr? -> jsexpr?
  (unless (flexpr? v)
    (raise-argument-error 'flexpr->jsexpr "flexpr" 0 v))
  v)

(define (write-flexpr-json v
                           [out (current-output-port)]
                           #:null [jsnull (json-null)]
                           #:encode [encode 'control])
  (void (write-json (flexpr->jsexpr v)
                    out
                    #:null jsnull
                    #:encode encode)))


;;; Test same data, both conversions
(module+ test
  (check-equal? (flexpr->xexpr (hasheq))              '(Response ()))
  (check-equal? (flexpr->xexpr (hasheq) #:root 'Root) '(Root ()))

  (check-equal? (flexpr->jsexpr (hasheq)) (hasheq))

  ;; Using default-singular-symbol
  (let ([v (hasheq 'ResponseId 123123
                   'Students
                   (list (hasheq 'FirstName "John"
                                 'LastName "Doe"
                                 'Age 12
                                 'Active #f
                                 'GPA 3.4)
                         (hasheq 'FirstName "Alyssa"
                                 'LastName "Hacker"
                                 'Age 14
                                 'Active #t
                                 'GPA 4.0)))])
    (check-true (flexpr? v))

    (check-equal? (flexpr->xexpr v)
                  '(Response ()
                             (ResponseId () "123123")
                             (Students ()
                                       (Student ()
                                                (Active () "false")
                                                (Age () "12")
                                                (FirstName () "John")
                                                (GPA () "3.4")
                                                (LastName () "Doe"))
                                       (Student ()
                                                (Active () "true")
                                                (Age () "14")
                                                (FirstName () "Alyssa")
                                                (GPA () "4.0")
                                                (LastName () "Hacker")))))

    (let ([out (open-output-string)])
      (write-flexpr-xml/content v out)
      (check-equal? (get-output-string out)
                    "<Response><ResponseId>123123</ResponseId><Students><Student><Active>false</Active><Age>12</Age><FirstName>John</FirstName><GPA>3.4</GPA><LastName>Doe</LastName></Student><Student><Active>true</Active><Age>14</Age><FirstName>Alyssa</FirstName><GPA>4.0</GPA><LastName>Hacker</LastName></Student></Students></Response>"))

    (let ([out (open-output-string)])
      (display-flexpr-xml/content v out)
      (check-equal? (get-output-string out)
                    "\n<Response>\n  <ResponseId>\n    123123\n  </ResponseId>\n  <Students>\n    <Student>\n      <Active>\n        false\n      </Active>\n      <Age>\n        12\n      </Age>\n      <FirstName>\n        John\n      </FirstName>\n      <GPA>\n        3.4\n      </GPA>\n      <LastName>\n        Doe\n      </LastName>\n    </Student>\n    <Student>\n      <Active>\n        true\n      </Active>\n      <Age>\n        14\n      </Age>\n      <FirstName>\n        Alyssa\n      </FirstName>\n      <GPA>\n        4.0\n      </GPA>\n      <LastName>\n        Hacker\n      </LastName>\n    </Student>\n  </Students>\n</Response>"))

    (check-equal? (flexpr->jsexpr v) v)

    (let ([out (open-output-string)])
      (write-flexpr-json v out)
      (check-equal? (get-output-string out)
                    "{\"ResponseId\":123123,\"Students\":[{\"FirstName\":\"John\",\"LastName\":\"Doe\",\"Age\":12,\"Active\":false,\"GPA\":3.4},{\"FirstName\":\"Alyssa\",\"LastName\":\"Hacker\",\"Age\":14,\"Active\":true,\"GPA\":4.0}]}")))

  ;; Using pluralization that needs a custom current-singular-symbol
  (parameterize ([current-singular-symbol
                  (λ (s)
                    (or (and (eq? s 'Werewolves) 'Werewolf)
                        (default-singular-symbol s)))])
    (let ([v (hasheq 'ResponseId 123123
                     'Werewolves
                     (list (hasheq 'FirstName "John"
                                   'LastName "Doe"
                                   'Age 12
                                   'Active #f
                                   'GPA 3.4)
                           (hasheq 'FirstName "Alyssa"
                                   'LastName "Hacker"
                                   'Age 14
                                   'Active #t
                                   'GPA 4.0)))])
      (check-true (flexpr? v))

      (check-equal? (flexpr->xexpr v)
                    '(Response ()
                               (ResponseId () "123123")
                               (Werewolves ()
                                           (Werewolf ()
                                                     (Active () "false")
                                                     (Age () "12")
                                                     (FirstName () "John")
                                                     (GPA () "3.4")
                                                     (LastName () "Doe"))
                                           (Werewolf ()
                                                     (Active () "true")
                                                     (Age () "14")
                                                     (FirstName () "Alyssa")
                                                     (GPA () "4.0")
                                                     (LastName () "Hacker")))))

    (let ([out (open-output-string)])
      (write-flexpr-xml/content v out)
      (check-equal? (get-output-string out)
                    "<Response><ResponseId>123123</ResponseId><Werewolves><Werewolf><Active>false</Active><Age>12</Age><FirstName>John</FirstName><GPA>3.4</GPA><LastName>Doe</LastName></Werewolf><Werewolf><Active>true</Active><Age>14</Age><FirstName>Alyssa</FirstName><GPA>4.0</GPA><LastName>Hacker</LastName></Werewolf></Werewolves></Response>"))

    (let ([out (open-output-string)])
      (display-flexpr-xml/content v out)
      (check-equal? (get-output-string out)
                    "\n<Response>\n  <ResponseId>\n    123123\n  </ResponseId>\n  <Werewolves>\n    <Werewolf>\n      <Active>\n        false\n      </Active>\n      <Age>\n        12\n      </Age>\n      <FirstName>\n        John\n      </FirstName>\n      <GPA>\n        3.4\n      </GPA>\n      <LastName>\n        Doe\n      </LastName>\n    </Werewolf>\n    <Werewolf>\n      <Active>\n        true\n      </Active>\n      <Age>\n        14\n      </Age>\n      <FirstName>\n        Alyssa\n      </FirstName>\n      <GPA>\n        4.0\n      </GPA>\n      <LastName>\n        Hacker\n      </LastName>\n    </Werewolf>\n  </Werewolves>\n</Response>"))

    (check-equal? (flexpr->jsexpr v) v)

    (let ([out (open-output-string)])
      (write-flexpr-json v out)
      (check-equal? (get-output-string out)
                    "{\"ResponseId\":123123,\"Werewolves\":[{\"FirstName\":\"John\",\"LastName\":\"Doe\",\"Age\":12,\"Active\":false,\"GPA\":3.4},{\"FirstName\":\"Alyssa\",\"LastName\":\"Hacker\",\"Age\":14,\"Active\":true,\"GPA\":4.0}]}")))))
