#lang racket
(require
  "static-site.rkt"
  )
(require
  "main.css.rkt"
  )
(require
  "about.html.rkt"
  "writing.html.rkt"
  )

(define-site "."
  ; path structure
  `(
    about
    (writing
      ))

  ; page definitions
  `(
    (about "About" ,about)
    (writing "Writing" ,writing)
   ))
