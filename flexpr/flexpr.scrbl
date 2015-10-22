#lang scribble/manual

@(require racket/sandbox
          scribble/eval)

@(define my-eval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'racket
                     #:requires '(flexpr))))

@(require (for-label racket
                     flexpr
                     json
                     xml))

@title{Expressions rendered as both XML and JSON}

@defmodule[flexpr]

@section{Rationale}

A @racket[flexpr?] is like a @racket[xexpr?] or @racket[jsexpr?], but
more flexible. A @racket[flexpr?] can be sensibly converted to either
XML or JSON.

Example use case: Your web service wants to offer the same response
data in the client's choice of XML or JSON (as expressed by the client
in a URI query parameter or @tt{Accept} header). Early in your
response pipeline, define your response as a @racket[flexpr?]. Convert
to the desired format as/when needed.

A small benefit: A @racket[flexpr?] doesn't make you prematurely
represent numbers as strings. They can remain numbers for JSON.
They're converted to strings only for XML. This makes it a bit more
convenient to write @racket[flexpr?]s by hand. Unlike @racket[xexpr?]s
you don't need to use @racket[~a] on non-string values.

So what should a @racket[flexpr?] be?

It's tempting to start with an @racket[xexpr?]. However the general
form of XML is awkward to convert to JSON satisfactorily. An XML
element essentially has two, parallel key/value dictionaries: the
attributes and the child elements. A JSON dict would need to store the
children in some sub-dict with a magic key name like say
@racket["body"] (or equally weirdly, store the attributes in a
sub-dict).

@margin-note{Another reason to prefer child elements is that attribute
values are limited to primitives like strings, numbers, and booleans;
there's no arbitrary nesting.}

Let's back up. Let's stipulate that having a "fork" of two dicts at
the same level is weird. It's weird even for XML. In fact most web
service response XML I've seen doesn't use attributes, just child
elements.

Instead let's start with a @racket[jsexpr?]. A @racket[flexpr?] is a
slightly restricted form of @racket[jsexpr?]:

@racketgrammar*[#:literals (boolean? string? exact-integer? inexact-real? hasheq
                                     plural-symbol? symbol? list)
                [flexpr boolean? string? exact-integer? inexact-real? (hasheq key-val ...)]
                [key-val [symbol? flexpr] [plural-symbol? (list flexpr ...)]]]

Given this definition:

@itemlist[

@item{The conversion to @racket[jsexpr?] is trivial. A @racket[flexpr?]
already is a @racket[jsexpr?].}

@item{The conversion to @racket[xexpr?] is simple, with one bit of
implicit "magic":

@itemlist[

@item{Attribute lists are always empty.}

@item{A @racket[hasheq] becomes a list of @racket[xexpr?]s -- the
key becomes the element tag and the value is the element body. The
list is spliced into a parent element:

@interaction[#:eval my-eval
             (flexpr->xexpr (hasheq 'a "a" 'x 0) #:root 'Response)]
}

@item{

@margin-note{Although I generally dislike implicit "magic", this seems
like the least-worst way to specify data that can be rendered to both
reasonably idiomatic XML and JSON.}

A @racket[list?] is allowed only as the value in a @racket[hasheq] --
and provided its key ends with @racket[#\s] (for example
@racket['items] but not @racket['item]). The plural is the parent
element tag, and the singular is used for each child tag.

@interaction[#:eval my-eval
             (flexpr->xexpr (hasheq 'Items (list 1 2)) #:root 'Response)]

}

]}]


@section{A more realistic example}

@interaction[#:eval my-eval
(define v (hasheq 'ResponseId 123123
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
                                'GPA 4.0))))
(flexpr? v)
(pretty-print (flexpr->xexpr v))
(display-xml/content (xexpr->xml (flexpr->xexpr v)))
(pretty-print (flexpr->jsexpr v))
(write-json (flexpr->jsexpr v))
]


@section{Basics}

@defproc[(flexpr? [v any/c]) boolean?]{ Predicate. }


@defproc[(flexpr->xexpr
          [v flexpr?]
          [#:root root symbol? 'Response]) xexpr?]{

Convert @racket[v] to an @racket[xexpr?], which is enclosed in an
element whose tag is @racket[root].

While traversing @racket[v], @racket[flexpr->xexpr] performs
equivalent tests as @racket[flexpr?], but will provide an error
message specific to the item that failed. For example when a plural
key is required for a @racket[(listof flexpr?)] value, the error
message is:

@interaction[#:eval my-eval
             (flexpr->xexpr (hasheq 'Item (list 0 1)))]

providing a hint that instead you should do:

@interaction[#:eval my-eval
             (flexpr->xexpr (hasheq 'Items (list 0 1)))]
}


@defproc[(flexpr->jsexpr [v flexpr?]) jsexpr?]{

Convert @racket[v] to a @racket[jsexpr?].

Because a @racket[flexpr?] is a subset of a @racket[jsexpr?], this is
approximately the composition of @racket[values] with
@racket[flexpr?].}


@section{Customizing "plural" symbols}

Although the default idea of a "plural symbol" is simply
@racket[default-singular-symbol], you may enhance this by supplying a
different function as the value of the
@racket[current-singular-symbol] parameter.


@defproc[(default-singular-symbol [v symbol?]) (or/c symbol? #f)]{

The default value of the @racket[current-singular-symbol] parameter --
a function that simply converts a symbol ending in @racket[#\s] to a
symbol lacking the @racket[#\s], and returns @racket[#f] for all other
symbols.

@interaction[#:eval my-eval
             (default-singular-symbol 'vampires)
             (default-singular-symbol 'vampire)]

}


@deftogether[(
  @defparam[current-singular-symbol proc singular-symbol/c #:value default-singular-symbol]
  @defthing[singular-symbol/c (symbol? -> (or/c symbol? #f))]
)]{

A parameter whose value is a function that converts plural symbols to
singular, and returns @racket[#f] for all other symbols.

You may customize this to extend or replace
@racket[default-singular-symbol].

@interaction[#:eval my-eval
             (parameterize ([current-singular-symbol
                             (λ (s)
                               (or (and (eq? s 'Werewolves) 'Werewolf)
                                   (default-singular-symbol s)))])
               (pretty-print
                (flexpr->xexpr
                 (hasheq 'Werewolves
                         (list (hasheq 'FirstName "John"
                                       'FangLength 6.4)
                               (hasheq 'FirstName "Alyssa"
                                       'FangLength 5.0))
                         'Vampires
                         (list (hasheq 'FirstName "John"
                                       'FangLength 3.4)
                               (hasheq 'FirstName "Alyssa"
                                       'FangLength 4.0))))))]
}


@defproc[(plural-symbol? [v any/c]) boolean?]{

A predicate that is effectively just:

@racketblock[(and ((current-singular-symbol) v) #t)]

}
