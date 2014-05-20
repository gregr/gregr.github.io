#lang racket
(require xml)

(define-syntax (for/foldm stx)
  (syntax-case stx ()
    ((_ accs ((pattern seq) ...) body)
     (with-syntax (((element ...)
                    (generate-temporaries #'((pattern seq) ...))))
      #'(for/fold accs
                ((element seq) ...)
          (match* (element ...)
            ((pattern ...) body)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; file generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-dirs path-parts)
  (let loop ((base 'same) (parts path-parts))
    (unless (empty? parts)
      (let ((next (build-path base (car parts))))
        (unless (directory-exists? next)
          (make-directory next))
        (loop next (cdr parts))))))

(define (xexpr->pretty-string xexpr)
  (call-with-output-string
    (curry display-xml/content (xexpr->xml xexpr))))
(define (xexpr->html-string xexpr)
  (string-append "<!DOCTYPE html>" (xexpr->string xexpr)))

(define (write-file path content)
  (define-values (directory filename is-root) (split-path path))
  (when (path? directory) (make-dirs (explode-path directory)))
  (call-with-output-file path #:exists 'replace
    (curry display content)))

(define (write-html-file path xexpr)
  (write-file path (xexpr->html-string xexpr)))

(define (css-style->string style)
  (let ((parts
          (for/foldm ((parts '()))
                     (((cons field value) (dict->list style)))
            (cons (format "  ~a: ~a;\n" field value) parts))))
    (string-append "{\n" (apply string-append (reverse parts)) "}\n")))

(define (css->string css)
  (let ((parts
          (for/foldm ((parts '()))
                     (((cons selector style) css))
            (cons (string-append selector " " (css-style->string style)) parts))))
    (apply string-append (reverse parts))))

(define (write-css-file path css)
  (write-file path (css->string css)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CSS description
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-css stx)
  (syntax-case stx ()
    ((_ path (selector style) ...)
     #`(write-css-file path
         (list
           (cons selector style) ...)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; site description
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reachable graph roots)
  (let loop ((reached (set))
             (roots (list->set (sequence->list roots))))
    (if (set-empty? roots) reached
      (let* ((reached (set-union reached roots))
             (nexts (map (curry dict-ref graph) (set->list roots)))
             (next (apply set-union nexts))
             (next (set-subtract next reached)))
        (loop reached next)))))

(struct node-ref (name data) #:transparent)
(define (tree-lift-refs tree)
  (match tree
    ((node-ref name data) (list (set name) data))
    ((? list?)
     (match-let (((list names trees)
                  (apply (curry map list) (map tree-lift-refs tree))))
       (list (apply set-union names) trees)))
    (_ (list (set) tree))))

(define (path-tree->name=>path+node-ref descs ptree)
  (define (attr-add attrs name base filename)
    (when (dict-has-key? attrs name)
      (error (format "path tree contains duplicate entries for: ~a" name)))
    (let* ((path (apply build-path (reverse (cons filename base))))
           (href (string-append "/" (path->string path)))
           (desc (dict-ref descs name))
           (link `(a ((href ,href)) ,desc)))
      (dict-set attrs name (list path (node-ref name link)))))
  (define (synthesize-fold attrs base children)
    (for/fold ((attrs attrs))
              ((child children))
      (synthesize attrs base child)))
  (define (synthesize attrs base child)
    (match child
      ((cons subdir children)
        (let* ((base (cons (symbol->string subdir) base))
               (attrs (attr-add attrs subdir base "index.html")))
          (synthesize-fold attrs base children)))
      (_
        (attr-add attrs child base
          (string-append (symbol->string child) ".html")))))
  (let* ((attrs (synthesize-fold (hash) '() (cdr ptree)))
         (attrs (attr-add attrs (car ptree) '() "index.html")))
    attrs))

(require (for-syntax racket/dict racket/function))
(define-for-syntax (build-site stx path-tree exprs pages)
  (let* ((pages (map syntax->list (syntax->list pages)))
         (page-idents (map car pages))
         (page-names (map syntax->datum page-idents))
         (page-descs (map syntax->datum (map cadr pages)))
         (page-bodies (map caddr pages))
         (descs (make-immutable-hash (map cons page-names page-descs)))
         (ptree (syntax->datum path-tree))
         (page-root (car ptree)))
    (let* ((names (dict-keys descs))
          (idents (map (curry datum->syntax stx) names)))
      (datum->syntax stx
        (append
          (list #'begin)
          (syntax->list #`(
            (define n=>p+nr (path-tree->name=>path+node-ref
                              (make-immutable-hash '#,(dict->list descs))
                              '#,ptree))
            (define n++p+nr (dict->list n=>p+nr))
            (define name=>path
              (make-immutable-hash
                (map (match-lambda ((cons name (list path nr)) (cons name path)))
                    n++p+nr)))
            (match-define
              (list #,@idents)
              (map (compose1 cadr (curry dict-ref n=>p+nr)) '#,names))))
          (syntax->list exprs)
          (list
            #`(let* ((page-reachables+xexpr
                       (map tree-lift-refs (list #,@page-bodies)))
                     (page-reachables (map car page-reachables+xexpr))
                     (page-graph (make-immutable-hash
                                   (map cons '#,page-names page-reachables)))
                     (pages-reached (reachable page-graph (list '#,page-root)))
                     (pages-not-reached (set-subtract (list->set '#,page-names)
                                                      pages-reached))
                     (page-xexprs (map cadr page-reachables+xexpr))
                     (page-paths (map (curry dict-ref name=>path)
                                      '#,page-names)))
                (unless (set-empty? pages-not-reached)
                  (error (format "unreachable pages: ~a"
                                 (set->list pages-not-reached))))
                (for ((path page-paths) (xexpr page-xexprs))
                     (write-html-file path xexpr)))))))))

(define-syntax (define-site stx)
  (syntax-case stx ()
    ((_ path-tree exprs page ...)
     (build-site stx #'path-tree #'exprs #'(page ...)))))

(define-for-syntax (identifier-prefixed prefix ident)
  (datum->syntax
    ident
    (string->symbol
      (string-append prefix (symbol->string (syntax->datum ident))))))

(define-syntax (define-anchors stx)
  (syntax-case stx ()
    ((_ (name url) ...)
     (with-syntax (((anchor-name ...)
                    (map (curry identifier-prefixed "anchor-")
                         (syntax->list #'(name ...)))))
      #'(begin
          (define (anchor-name desc)
            `(a ((href ,url)) ,desc)) ...)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; an actual site
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define nav-content-width "900px")
(define nav-width "150px")

(define-css "main.css"
  ("body"
   (hash
     "font-family" "'Lucida Grande', Verdana, Helvetica, sans-serif"
     "font-size" "80%"
     "line-height" "120%"
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

  ("#nav-main"
   (hash
     "position" "fixed"
     "float" "left"
     "width" nav-width))
  ("#content-main"
   (hash
     "margin-left" nav-width
     "margin-right" nav-width
     ))

  (".date"
   (hash
     "font-style" "italic"
     ))
  (".date-end:before"
   (hash
     "content" "\" - \""))

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

(define-site
  ; path structure
  (about)

  ; utilities
  (
   (define head
     `(head
        (title "Greg's Metareflection")
        (meta ((charset "utf-8")))
        (meta ((name "author") (content "Greg Rosenblatt")))
        (meta ((name "description") (content "Personal page of Greg Rosenblatt")))
        (link ((rel "stylesheet") (href "main.css")))))
   (define nav
     `(nav ((id "nav-main"))
        (ul
          (li ,about))))
   (define (content body)
     `(html
        ,head
        (body
          (div ((id "content"))
            ;,nav
            (div ((id "content-main")) ,body)
            ))))
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
   (define (personal-project name start end . details)
     `(div ((class "personal-project"))
        (h3 ((class "personal-project-name")) ,name)
        ,(date-range start end)
        (div ((class "personal-project-details")) ,@details)))
   (define (employment employer location title start end . details)
     `(div ((class "employment"))
        (h3 ((class "employment-employer")) ,employer)
        (div ((class "employment-location")) ,location)
        (h4 ((class "employment-title")) ,title)
        ,(date-range start end)
        (div ((class "employment-details")) ,@details)))
   (define (education name date . details)
     `(div ((class "education"))
        (h3 ((class "education-name")) ,name)
        ,date
        (div ((class "education-details")) ,@details)))

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
   ))

  ; page definitions
  (about "About"
    (content
      `(article
         (h1 ((class "content-title")) "Gregory L. Rosenblatt")
         (section
           (div ,(anchor "mailto:greg.weiqi@gmail.com" "greg.weiqi@gmail.com"))
           (div "GitHub: " ,(anchor-github "gregr"))
           )
         (section ((class "summary"))
           ,(anchor-target "summary")
           (h2 "Summary")
           (p "I currently live in Toronto, Ontario, working as a Software Engineer.")
           (p
             "My "
             ,(anchor-beat-cap "focus")
             " is "
             ,(anchor-consistent-hash "more")
             " "
             ,(anchor-raft-paxos "on")
             " "
             ,(anchor-relational-prog "general")
             " "
             ,(anchor-props-as-types "ideas")
             " than on specific technologies.  Though, I do like to choose "
             ,(anchor-postgres "which")
             " "
             ,(anchor-redis "technologies")
             " to work with, "
             ,(anchor-racket "preferring")
             " "
             ,(anchor-haskell "well-designed")
             " "
             ,(anchor-python "languages")
             ", "
             ,(anchor-git "tools")
             " and "
             ,(anchor-100-percent-solutions "100% solutions")
             ".  Unfortunately I have "
             ,(anchor-c++-fqa "plenty")
             " of "
             ,(anchor-wat-talk "experience")
             " with "
             ,(anchor-php-fractally-bad "poorly-designed")
             " "
             ,(anchor-mysql-choose-something-else "technology")
             ".")
           (p
             "I enjoy being "
             ,(anchor-peopleware "managed")
             " well.")
           (p
             "My approach to problem solving emphasizes "
             ,(anchor-prag-prog "tool-building and automation")
             ".  This includes code generation, transformation and other "
             ,(anchor-role-of-PL-study "language-oriented")
             " techniques.  When solving mysteries, I understand and reason about code before jumping into a debugger.")
           (p
             "A major goal of mine is to become better at "
             ,(anchor-magic-ink "designing")
             " for "
             ,(anchor-drawing-dynamic "human")
             " "
             ,(anchor-learnable-prog "beings")
             ".  The "
             ,(anchor-future-of-prog "past")
             " inspires me.")
           (p
             "I play "
             ,(anchor-weiqi "Go/Weiqi/Baduk")
             " at the 5 dan level."
             ))
         (section
           ,(anchor-target "personal-projects")
           (h2 "Notable personal projects")
           (p "The following projects were the product of significant effort made as I grew up as a programmer.")
           (ul ((class "personal-project-list"))
             (li
               ,(personal-project
                  (anchor "https://github.com/gregr/chive" "The Chive Programming Language")
                  "2009" "2010"
                  `(p "This was my first serious attempt at a full programming language implementation.  It features a scheme-like hygienic macro system based on syntactic closures.")
                  `(p "It was put aside when I started working at Facebook.")
                  ))
             (li
               ,(personal-project
                  (anchor "https://github.com/gregr/uriel" "Uriel")
                  "2005" "2006"
                  `(p "a tile-based multiplayer game framework")
                  `(p "Several amusing games were made with this.  They are lost.")
                  ))
             (li
               ,(personal-project
                  (anchor "https://github.com/gregr/starscape" "Starscape")
                  "1999" "2005"
                  `(p "a 3D game programming system, including a GUI library")
                  ))
             (li
               ,(personal-project
                  "Creating a Scripting System in C++"
                  "2002" "2003"
                  `(p "This is an article series I wrote for " ,(anchor "http://www.gamedev.net/" "gamedev.net") " in five parts.")
                  `(ul ((class "multipart-series"))
                     (li ,(anchor "http://www.gamedev.net/page/resources/_/technical/game-programming/creating-a-scripting-system-in-c-part-i-an-i-r1633" "I"))
                     (li ,(anchor "http://www.gamedev.net/page/resources/_/technical/game-programming/creating-a-scripting-system-in-c-part-ii-dat-r1686" "II"))
                     (li ,(anchor "http://www.gamedev.net/page/resources/_/technical/game-programming/creating-a-scripting-system-in-c-part-iii-dy-r1788" "III"))
                     (li ,(anchor "http://www.gamedev.net/page/resources/_/technical/game-programming/creating-a-scripting-system-in-c-part-iv-the-r1803" "IV"))
                     (li ,(anchor "http://www.gamedev.net/page/resources/_/technical/game-programming/creating-a-scripting-system-in-c-part-v-func-r1877" "V")))
                  ))))
         (section
           ,(anchor-target "work-experience")
           (h2 "Professional experience")
           (ul ((class "employment-list"))
             (li
               ,(employment "Tulip Retail" "Toronto, ON"
                            "Software Engineer"
                            "October 2013" "Present"
                  `(p "Part of the founding team originally from Well.ca.")
                  `(p "Worked on both retail platform and individual client projects.")
                  `(ul
                     (li "Re-design of data model and platform in terms of a service-oriented architecture")
                     (li "Framework for building hypermedia APIs")
                     (li "Data ingestion and processing library")
                     (li "Continuous image import system")
                     )))
             (li
               ,(employment "Well.ca" "Kitchener, ON"
                            "Software Engineer"
                            "August 2012" "October 2013"
                  `(p "Refactored and redesigned the warehouse management system")
                  `(ul
                     (li "Separation of WMS into an independent service providing a web API")
                     (li "Design and performance analysis of alternative communication protocols")
                     (li "Internal tools and automation for development and deployment")
                     )))
             (li
               ,(employment "Facebook" "Palo Alto, CA"
                            "Software Engineer"
                            "January 2011" "June 2012"
                  `(p "Developed and maintained internal development tools and infrastructure")
                  `(ul
                     (li "Distributed continuous test run infrastructure")
                     (li "Automatic test failure blame assignment and task creation")
                     (li "Phabricator")
                     (li "Task management system")
                     (li "Dev-server allocation and management system")
                     (li "Real-time asynchronous job tier")
                     )))
             (li
               ,(employment "Ellington Management Group" "Old Greenwich, CT"
                            "Quantitative Developer"
                            "May 2006" "August 2009"
                  `(p "Developed and maintained various modelling and trading systems.")
                  `(ul
                     (li "Distributed computing infrastructure")
                     (li "Cross-language services and remote procedure call library")
                     (li "Quantitative model implementations")
                     (li "Real-time market data streaming and aggregation")
                     (li "Automated trading and order management system")
                     )))
             ))
         (section
           ,(anchor-target "education")
           (h2 "Education")
           (ul ((class "education-list"))
             (li
               ,(education
                  "Rochester Institute of Technology"
                  (date-range "1999" "2004")
                  `(p "Bachelor of Science in Mechanical Engineering")
                  `(p "Concentration in Aerospace")
                  ))
             (li
               ,(education
                  "Stanford Online Courses for AI and Machine Learning"
                  (date-single "Fall 2011")
                  `(p "Statement of Accomplishment")
                  `(p "This was the original online offering of these two classes from which "
                      ,(anchor "https://www.coursera.org/" "coursera")
                      " and "
                      ,(anchor "https://www.udacity.com/" "udacity")
                      " were spun off.")))))
         (section
           ,(anchor-target "recommended-reading")
           (h2 "Some reading I've enjoyed and recommend")
           (ul
             (li ,(anchor-SYJMrF "Surely You're Joking, Mr. Feynman!"))
             (li ,(anchor-GEB "Gödel, Escher, Bach"))
             (li ,(anchor-logicomix "Logicomix"))
             (li ,(anchor-prag-prog "The Pragmatic Programmer"))
             (li ,(anchor-peopleware "Peopleware"))
             (li ,(anchor-sicp "Structure and Interpretation of Computer Programs"))
             (li ,(anchor-algo-design-manual "The Algorithm Design Manual"))
             (li ,(anchor-okasaki "Purely Functional Data Structures"))
             (li ,(anchor-prog-pearls "Programming Pearls"))
             ))
         )))
  )
