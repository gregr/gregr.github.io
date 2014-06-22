#lang racket
(provide
  writing
  writing-names
  writing-pages
  )

(require
  "common.rkt"
  "static-site.rkt"
  )

(define nav
  `(nav ((id "nav-local"))))

(define writing
  (content
    "Writing"
    nav
    `(article
       (h1 ((class "content-title")) "Misguided Writing")
       (section ((class "summary"))
                (p "This is where you will find my attempts at meaningful writing.")
                (p ,(node-ref 'test))
                )
       )))

(define (read-all inp)
  (let loop ((forms '()))
    (let ((form (read inp)))
      (if (eq? form eof)
        (cons 'begin (reverse forms))
        (loop (cons form forms))))))
(define writing-namespace
  (let ((local-ns (current-namespace)))
    (parameterize ((current-namespace (make-base-namespace)))
      (namespace-attach-module local-ns "common.rkt")
      (namespace-attach-module local-ns "static-site.rkt")
      (namespace-require "common.rkt")
      (namespace-require "static-site.rkt")
      (current-namespace))))

(define writing-entries
  '(
    (test "Test Article")
    ))

(define writing-names (map car writing-entries))
(define writing-descs (map cadr writing-entries))
(define writing-paths
  (for/list ((name writing-names))
    (string-append "writing/" (symbol->string name) ".html.rkt")))
(define writing-xexprs
  (for/list ((path writing-paths))
    (eval (call-with-input-file path read-all) writing-namespace)))
(define writing-pages
  (for/list ((name writing-names) (desc writing-descs) (xexpr writing-xexprs))
    (list name desc xexpr)))
