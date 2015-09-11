#lang racket
(provide about)

(require
  "common.rkt"
  "static-site.rkt"
  )

(match-define (list ref-personal _)
  (anchors `(
    (github "gregr at GitHub" "https://github.com/gregr")
    )))

(match-define (list ref-misc anchor-list-misc)
  (anchors `(
    (weiqi "Go/Weiqi/Baduk" "https://en.wikipedia.org/wiki/Go_(game)")
    )))

(match-define (list ref-tech anchor-list-tech)
  (anchors `(
    (100-percent-solutions "100% and 80% solutions" "http://www.ccs.neu.edu/home/shivers/papers/sre.txt")
    (postgres "PostgreSQL" "https://en.wikipedia.org/wiki/PostgreSQL")
    (redis "Redis" "https://en.wikipedia.org/wiki/Redis")
    (haskell "Haskell" "https://en.wikipedia.org/wiki/Haskell_(programming_language)")
    (racket "Racket" "https://en.wikipedia.org/wiki/Racket_(programming_language)")
    (python "Python" "https://en.wikipedia.org/wiki/Python_(programming_language)")
    (git "Git" "https://en.wikipedia.org/wiki/Git_(software)")
    )))

(match-define (list ref-tech-poor anchor-list-tech-poor)
  (anchors `(
    (c++-fqa "C++ FQA" "http://yosefk.com/c++fqa/")
    (wat-talk "Wat" "https://www.destroyallsoftware.com/talks/wat")
    (php-fractally-bad "PHP: a fractal of bad design" "http://me.veekun.com/blog/2012/04/09/php-a-fractal-of-bad-design/")
    (mysql-choose-something-else "Do Not Pass This Way Again" "http://grimoire.ca/mysql/choose-something-else")
    )))

(match-define (list ref-design anchor-list-design)
  (anchors `(
    (magic-ink "Magic Ink" "http://worrydream.com/MagicInk/")
    (drawing-dynamic "Additional Notes on \"Drawing Dynamic Visualizations\"" "http://worrydream.com/DrawingDynamicVisualizationsTalkAddendum/")
    (learnable-prog "Learnable Programming" "http://worrydream.com/LearnableProgramming/")
    (future-of-prog "References for \"The Future of Programming\"" "http://worrydream.com/dbx/")
    )))

(match-define (list ref-reading anchor-list-reading)
  (anchors `(
    (SYJMrF "Surely You're Joking, Mr. Feynman!" "https://en.wikipedia.org/wiki/Surely_You're_Joking,_Mr._Feynman!")
    (GEB "Gödel, Escher, Bach" "https://en.wikipedia.org/wiki/G%C3%B6del,_Escher,_Bach")
    (logicomix "Logicomix" "https://en.wikipedia.org/wiki/Logicomix")
    (prag-prog "The Pragmatic Programmer" "https://en.wikipedia.org/wiki/The_Pragmatic_Programmer")
    (peopleware "Peopleware" "https://en.wikipedia.org/wiki/Peopleware:_Productive_Projects_and_Teams")
    (sicp "Structure and Interpretation of Computer Programs" "https://mitpress.mit.edu/sicp/")
    (algo-design-manual "The Algorithm Design Manual" "http://www.algorist.com/")
    (okasaki "Purely Functional Data Structures" "http://www.amazon.ca/Purely-Functional-Structures-Chris-Okasaki/dp/0521663504")
    (prog-pearls "Programming Pearls" "http://www.cs.bell-labs.com/cm/cs/pearls/")
    )))

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

(define nav-about
  (nav-local `(
    ("summary"             "Summary")
    ("personal-projects"   "Personal Projects")
    ("work-experience"     "Professional Experience")
    ("education"           "Education")
    ("recommended-reading" "Recommended Reading")
  )))

(define about
  (list
    "About"
    (content
      "About"
      nav-about
      `(article
         (h1 ((class "content-title")) "Gregory L. Rosenblatt")
         (section
           (div ,(anchor "mailto:greg.weiqi@gmail.com" "greg.weiqi@gmail.com"))
           (div "GitHub: " ,(ref-personal 'github "gregr"))
           )
         (section ((class "summary"))
                  ,(anchor-target "summary")
                  (h2 "Summary")
                  (p "I currently live in Toronto, Ontario, working as a Software Engineer.")
                  (p
                    "My "
                    ,(ref-arch 'beat-cap "focus")
                    " is "
                    ,(ref-arch 'consistent-hash "more")
                    " "
                    ,(ref-arch 'raft-paxos "on")
                    " "
                    ,(ref-plt 'relational-prog "general")
                    " "
                    ,(ref-plt 'props-as-types "ideas")
                    " than on specific technologies.  Though, I do like to choose "
                    ,(ref-tech 'postgres "which")
                    " "
                    ,(ref-tech 'redis "technologies")
                    " to work with, "
                    ,(ref-tech 'racket "preferring")
                    " "
                    ,(ref-tech 'haskell "well-designed")
                    " "
                    ,(ref-tech 'python "languages")
                    ", "
                    ,(ref-tech 'git "tools")
                    " and "
                    ,(ref-tech '100-percent-solutions "100% solutions")
                    ".  Unfortunately I have "
                    ,(ref-tech-poor 'c++-fqa "plenty")
                    " of "
                    ,(ref-tech-poor 'wat-talk "experience")
                    " with "
                    ,(ref-tech-poor 'php-fractally-bad "poorly-designed")
                    " "
                    ,(ref-tech-poor 'mysql-choose-something-else "technology")
                    ".")
                  (p
                    "I enjoy being "
                    ,(ref-reading 'peopleware "managed")
                    " well.")
                  (p
                    "My approach to problem solving emphasizes "
                    ,(ref-reading 'prag-prog "tool-building and automation")
                    ".  This includes code generation, transformation and other "
                    ,(ref-plt 'role-of-PL-study "language-oriented")
                    " techniques.  When solving mysteries, I understand and reason about code before jumping into a debugger.")
                  (p
                    "A major goal of mine is to become better at "
                    ,(ref-design 'magic-ink "designing")
                    " for "
                    ,(ref-design 'drawing-dynamic "human")
                    " "
                    ,(ref-design 'learnable-prog "beings")
                    ".  The "
                    ,(ref-design 'future-of-prog "past")
                    " inspires me.")
                  (p
                    "I play "
                    ,(ref-misc 'weiqi "Go/Weiqi/Baduk")
                    " at the 5 dan level."
                    ))
         (section
           ,(anchor-target "personal-projects")
           (h2 "Notable personal projects")
           (p "The following projects were the product of significant effort made as I grew up as a programmer.")
           (ul ((class "personal-project-list"))
               (li
                 ,(personal-project
                    (anchor "https://github.com/gregr/racket-misc" "Racket-Misc")
                    "2014" "present"
                    `(p "This is a library of miscellaneous utilities for the " ,(ref-tech 'racket "Racket")
                        " programming language.  It features records, cursors ("
                        ,(ref-dsalgo 'zipper "zippers")
                        " and lenses), unrestricted "
                        ,(ref-plt 'yield-mainstream-delim-cont "generators")
                        ", "
                        ,(ref-plt 'eff-lang "algebraic effects")
                        ", gratuitous experiments in syntactic sugar, and more.")
                    ))
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
                 ,(employment "500px" "Toronto, ON"
                              "Platform Engineer"
                              "July 2014" "February 2015"
                              `(p "Improved platform stability and performance")
                              `(ul
                                 (li "Kafka-based data pipeline prototype")
                                 (li "Refactoring of Rails application to produce microservices written in Go")
                                 (li "Internal libraries/tools that simplify creation of new Go services")
                                 )))
               (li
                 ,(employment "Tulip Retail" "Toronto, ON"
                              "Software Engineer"
                              "October 2013" "June 2014"
                              `(p "Part of the founding team originally from Well.ca")
                              `(p "Worked on both retail platform and individual client projects")
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
                              `(p "Developed and maintained various modelling and trading systems")
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
           ,(anchor-reference anchor-list-reading)
           )
         )
      (list (atom-feed-link))
      )))
