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

(write-xml-file "writing.xml" writing-feed)

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
