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
     "color" "#333333"
     ))
  ("#nav-panel ul ul a:link, #nav-panel ul ul a:visited"
   (hash
     "color" "#666666"
     ))
  ("#content-main"
   (hash
     "margin-left" nav-width
     "margin-right" nav-width
     ))

  ("#nav-local"
   (hash
     "margin-right" "1em"
     ))
  ("nav ul"
   (hash
     "margin" "0"
     "padding-left" "1em"
     "list-style-type" "none"
     ))
  ("nav li"
   (hash
     "margin-top" "1em"
     "margin-bottom" "1em"
     ))

  (".date"
   (hash
     "font-style" "italic"
     ))
  (".date-end:before"
   (hash
     "content" "\" - \""))

  (".code-frag"
   (hash
     "padding" "0.2em"
     "font-weight" "bold"
     "color" "brown"
     "background-color" "beige"
     ))

  (".code-block"
   (hash
     "padding" "1em"
     "color" "brown"
     "background-color" "beige"
     ))

  (".multipart-series li"
   (hash
     "display" "inline"
     "padding" "10px"))

  (".publication-list"
   (hash
     "list-style-type" "none"
     ))
  (".publication-list>li"
   (hash
     "margin-bottom" "2em"
     ))
  (".publication-name"
   (hash
     "display" "inline"
     "margin-right" "1em"
   ))
  (".publication .date-range"
   (hash
     "display" "inline"
   ))

  (".talk-list"
   (hash
     "list-style-type" "none"
     ))
  (".talk-list>li"
   (hash
     "margin-bottom" "2em"
     ))
  (".talk-name"
   (hash
     "display" "inline"
     "margin-right" "1em"
   ))
  (".talk .date-range"
   (hash
     "display" "inline"
   ))

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

  (".workflow .state"
   (hash
     "border-style" "dashed"
     "border-width" "1px"
     "border-color" "red"
     ))


  ;; pandoc syntax highlighting

  (".pandoc code" (hash "white-space" "pre"))
  (".pandoc pre, pandoc code" (hash "color" "brown" "background-color" "beige"))
  (".pandoc code > span.co" (hash "color" "grey" "font-style" "italic"))
  (".pandoc code > span.kw" (hash "color" "#0050aa" "font-weight" "bold"))
  (".pandoc code > span.st" (hash "color" "#049b0a"))
  (".pandoc code > span.dt" (hash "text-decoration" "underline"))
  (".pandoc code > span.dv" (hash "color" "#44aa43"))
  (".pandoc code > span.bn" (hash "color" "#44aa43"))
  (".pandoc code > span.fl" (hash "color" "#44aa43"))
  (".pandoc code > span.ch" (hash "color" "#049b0a"))
  (".pandoc code > span.al" (hash "color" "#ffff00"))
  (".pandoc code > span.fu" (hash "color" "#ff9358" "font-weight" "bold"))
  (".pandoc code > span.er" (hash "font-weight" "bold"))
  ;table.sourceCode, tr.sourceCode, td.lineNumbers, td.sourceCode { ;margin: 0; padding: 0; vertical-align: baseline; border: none; }
  ;table.sourceCode { width: 100%; line-height: 100%; background-color: #2a211c; color: #bdae9d; }
  ;td.lineNumbers { text-align: right; padding-right: 4px; padding-left: 4px; background-color: #2a211c; color: #bdae9d; border-right: 1px solid #bdae9d; }
  ;td.sourceCode { padding-left: 5px; }
  )
