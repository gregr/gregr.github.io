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
      ,@writing-names
      ))

  ; page definitions
  `(
    (about ,@about)
    (writing ,@writing)
    ,@writing-pages
   ))
