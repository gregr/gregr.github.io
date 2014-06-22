#lang racket
(provide writing)

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
                )
       )))
