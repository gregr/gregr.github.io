#lang racket
(provide
  (all-defined-out)
  )

(require
  "static-site.rkt"
  )

(define-anchors
  (github "https://github.com/gregr")
  (weiqi "https://en.wikipedia.org/wiki/Go_(game)")

  (beat-cap "http://nathanmarz.com/blog/how-to-beat-the-cap-theorem.html")
  (consistent-hash "http://www.tomkleinpeter.com/2008/03/17/programmers-toolbox-part-3-consistent-hashing/")
  (raft-paxos "http://www.reddit.com/comments/1c1rjx")
  (props-as-types "http://homepages.inf.ed.ac.uk/wadler/papers/propositions-as-types/propositions-as-types.pdf")
  (relational-prog "https://www.youtube.com/watch?v=5Q9x16uIsKA")
  (role-of-PL-study "http://www.cs.indiana.edu/~dfried/mex.pdf")

  (100-percent-solutions "http://www.ccs.neu.edu/home/shivers/papers/sre.txt")
  (postgres "https://en.wikipedia.org/wiki/PostgreSQL")
  (redis "https://en.wikipedia.org/wiki/Redis")
  (haskell "https://en.wikipedia.org/wiki/Haskell_(programming_language)")
  (racket "https://en.wikipedia.org/wiki/Racket_(programming_language)")
  (python "https://en.wikipedia.org/wiki/Python_(programming_language)")
  (git "https://en.wikipedia.org/wiki/Git_(software)")

  (c++-fqa "http://yosefk.com/c++fqa/")
  (wat-talk "https://www.destroyallsoftware.com/talks/wat")
  (php-fractally-bad "http://me.veekun.com/blog/2012/04/09/php-a-fractal-of-bad-design/")
  (mysql-choose-something-else "http://grimoire.ca/mysql/choose-something-else")

  (magic-ink "http://worrydream.com/MagicInk/")
  (drawing-dynamic "http://worrydream.com/DrawingDynamicVisualizationsTalkAddendum/")
  (learnable-prog "http://worrydream.com/LearnableProgramming/")
  (future-of-prog "http://worrydream.com/dbx/")

  (SYJMrF "https://en.wikipedia.org/wiki/Surely_You're_Joking,_Mr._Feynman!")
  (GEB "https://en.wikipedia.org/wiki/G%C3%B6del,_Escher,_Bach")
  (logicomix "https://en.wikipedia.org/wiki/Logicomix")
  (prag-prog "https://en.wikipedia.org/wiki/The_Pragmatic_Programmer")
  (peopleware "https://en.wikipedia.org/wiki/Peopleware:_Productive_Projects_and_Teams")
  (sicp "https://mitpress.mit.edu/sicp/")
  (algo-design-manual "http://www.algorist.com/")
  (okasaki "http://www.amazon.ca/Purely-Functional-Structures-Chris-Okasaki/dp/0521663504")
  (prog-pearls "http://www.cs.bell-labs.com/cm/cs/pearls/")
  )

(define (anchor href description)
  `(a ((href ,href)) ,description))
(define (anchor-target name)
  `(a ((name ,name))))
(define (date-single d)
  `(div ((class "date")) ,d))
(define (date-range start end)
  `(div ((class "date-range"))
        (span ((class "date")) ,start)
        (span ((class "date date-end")) ,end)))

(define head
  `(head
     (title "Greg Rosenblatt - About")
     (meta ((charset "utf-8")))
     (meta ((name "author") (content "Greg Rosenblatt")))
     (meta ((name "description") (content "Personal site of Greg Rosenblatt")))
     (link ((rel "stylesheet") (href "main.css")))
     (script ((src "//ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js")))
     (script ((src "main.js")))
     ))
(define nav-main
  `(nav ((id "nav-main"))
        (ul
          (li ,(node-ref 'about))
          )))

(define (content nav-local body)
  `(html
     ,head
     (body
       (div ((id "content"))
            (div ((id "nav-panel"))
                 ,nav-local
                 )
            (div ((id "content-main")) ,body)
            ))))
