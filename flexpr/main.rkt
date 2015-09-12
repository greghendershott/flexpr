#lang at-exp racket/base

(require json
         racket/format
         racket/function
         racket/list
         racket/match
         xml)

(provide (all-from-out json
                       xml)
         flexpr?
         plural-symbol?
         flexpr->xexpr
         flexpr->jsexpr)

(module+ test
  (require rackunit))


;;; Predicates

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

(define (plural-symbol? v) ;any -> boolean?
  (and (symbol? v)
       (let ([s (symbol->string v)])
         (eq? #\s (string-ref s (sub1 (string-length s)))))))

(define (singular-symbol v) ;plural-symbol? -> symbol?
  (and (plural-symbol? v)
       (let ([s (symbol->string v)])
         (string->symbol (substring s 0 (sub1 (string-length s)))))))


;;; Conversion to xexpr / xml

;; Note: A contract with flexpr? would double traverse, don't need.
(define (flexpr->xexpr v #:root [root 'Response]) ;flexpr? symbol? -> xexpr?
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
              (for/list ([(k v) (in-hash ht)])
                (match* (k v)
                  [((? plural-symbol? plural) (? list? vs))
                   (define singular (singular-symbol plural))
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

(module+ test
  (check-false (flexpr? (hasheq 'item (list 0 1 2))))
  (check-exn #px"hash table key must be plural-symbol?"
             (λ () (flexpr->xexpr (hasheq 'item (list 0 1 2))))))


;;; Conversion to jsexpr / json

(define (flexpr->jsexpr v) ;flexpr? -> jsexpr?
  (unless (flexpr? v)
    (raise-argument-error 'flexpr->jsexpr "flexpr" 0 v))
  v)


;;; Test same data, both conversions
(module+ test
  (check-equal? (flexpr->xexpr (hasheq))              '(Response ()))
  (check-equal? (flexpr->xexpr (hasheq) #:root 'Root) '(Root ()))

  (check-equal? (flexpr->jsexpr (hasheq)) (hasheq))

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
                                                (FirstName () "John")
                                                (LastName () "Doe")
                                                (Age () "12")
                                                (Active () "false")
                                                (GPA () "3.4"))
                                       (Student ()
                                                (FirstName () "Alyssa")
                                                (LastName () "Hacker")
                                                (Age () "14")
                                                (Active () "true")
                                                (GPA () "4.0")))))

    (check-equal? (flexpr->jsexpr v) v)))
