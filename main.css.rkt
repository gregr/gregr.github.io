#lang racket
(require
  "static-site.rkt"
  )

(define nav-content-width "1160px")
(define nav-width "280px")

(define-css "main.css"
  ("body"
   (hash
     "font-family" "'Lucida Grande', Verdana, Helvetica, sans-serif"
     "font-size" "80%"
     "line-height" "120%"
     ))

  ("nav"
   (hash
     "margin-bottom" "6em"
     ))

  ("section"
   (hash
     "margin-top" "2em"
     "margin-bottom" "2em"
     ))

  ("p"
   (hash
     "margin-top" "0.5em"
     "margin-bottom" "0.5em"
     ))

  (".summary>p"
   (hash
     "margin-top" "1em"
     "margin-bottom" "1em"
     ))

  (".content-title"
   (hash
     "margin-top" "2em"
     "font-size" "1.7em"
     ))

  ("a:link, a:visited"
   (hash
     "text-decoration" "none"
     "color" "#226699"
     ))
  ("a:hover"
   (hash
     "text-decoration" "underline"
     "color" "#333333"
     ))

  ("#content"
   (hash
     "width" nav-content-width
     "margin" "auto"
     ))

  ("#nav-panel"
   (hash
     "float" "left"
     "width" nav-width
     ))
  ("#nav-panel a:link, #nav-panel a:visited"
   (hash
     "text-decoration" "none"
     "color" "#666666"
     ))
  ("#content-main"
   (hash
     "margin-left" nav-width
     "margin-right" nav-width
     ))

  ("nav ul"
   (hash
     "list-style-type" "none"
     ))
  ("nav li"
   (hash
     "margin-bottom" "1em"
     ))

  (".date"
   (hash
     "font-style" "italic"
     ))
  (".date-end:before"
   (hash
     "content" "\" - \""))

  (".code-block"
   (hash
     "margin-left" "1em"
     ))

  (".multipart-series li"
   (hash
     "display" "inline"
     "padding" "10px"))

  (".personal-project-list"
   (hash
     "list-style-type" "none"
     ))
  (".personal-project-list>li"
   (hash
     "margin-bottom" "2em"
     ))
  (".personal-project-name"
   (hash
     "display" "inline"
     "margin-right" "1em"
   ))
  (".personal-project .date-range"
   (hash
     "display" "inline"
   ))

  (".employment-list"
   (hash
     "list-style-type" "none"
     ))
  (".employment-list>li"
   (hash
     "margin-bottom" "2em"
     ))
  (".employment-employer"
   (hash
     "display" "inline"
     "font-weight" "bold"
     ))
  (".employment-location"
   (hash
     "display" "inline"
     "font-weight" "normal"
     ))
  (".employment-title:before"
   (hash
     "content" "\"\\A\""
     "white-space" "pre"
     ))
  (".employment-title"
   (hash
     "display" "inline"
     "margin-right" "1em"
     ))
  (".employment>.date-range"
   (hash
     "display" "inline"
     "margin-bottom" "1em"
     ))
  (".employment-location:before"
   (hash
     "content" "\" - \""))

  (".education-list"
   (hash
     "list-style-type" "none"
     ))
  (".education-list>li"
   (hash
     "margin-bottom" "2em"
     ))
  (".education-name"
   (hash
     "display" "inline"
     "margin-right" "1em"
     ))
  (".education>.date-range"
   (hash
     "display" "inline"
     ))
  (".education>.date"
   (hash
     "display" "inline"
     ))
  )
