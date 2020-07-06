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

(define (publication name authors . details)
  `(div ((class "publication"))
        (h3 ((class "publication-name")) ,name)
        (div ((class "publication-authors")) ,authors)
        (div ((class "publication-details")) ,@details)))
(define (talk name authors . details)
  `(div ((class "talk"))
        (h3 ((class "talk-name")) ,name)
        (div ((class "talk-authors")) ,authors)
        (div ((class "talk-details")) ,@details)))
(define (personal-project name start end . details)
  `(div ((class "personal-project"))
        (h3 ((class "personal-project-name")) ,name)
        ,(date-range start end)
        (div ((class "personal-project-details")) ,@details)))
(define (employment employer location roles . details)
  `(div ((class "employment"))
        (h3 ((class "employment-employer")) ,employer)
        (div ((class "employment-location")) ,location)
        ,@(append* (map (lambda (r)
                          (match-define (list title start end) r)
                          `((h4 ((class "employment-title")) ,title)
                            ,(date-range start end))) roles))
        (div ((class "employment-details")) ,@details)))
(define (education name date . details)
  `(div ((class "education"))
        (h3 ((class "education-name")) ,name)
        ,date
        (div ((class "education-details")) ,@details)))

(define nav-about
  (nav-local `(
    ("summary"             "Summary")
    ("research-interests"  "Research interests")
    ("publications"        "Publications")
    ("talks"               "Talks")
    ("work-experience"     "Professional")
    ("non-work-experience" "Extraprofessional")
    ("education"           "Education")
    ("personal-projects"   "Old projects")
    ("recommended-reading" "Reading")
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
                  (p "I work for the " (a ((target "_blank") (href "https://www.uab.edu/medicine/pmi/"))
                                          "Hugh Kaul Precision Medicine Institute")
                     " at the University of Alabama at Birmingham."
                     "  I build "
                     ,(anchor "https://github.com/webyrd/mediKanren" "biomedical reasoning tools")
                     " using relational programming techniques."
                     "  I live in Toronto, Ontario.")
                  (p
                    "My approach to problem solving emphasizes "
                    ,(ref-reading 'prag-prog "tool-building and automation")
                    ".  This includes generating and transforming code, and other "
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
           ,(anchor-target "research-interests")
           (h2 "Research interests")
           (ul
             (li "Relational (constraint logic) programming")
             (li "Old-school symbolic artificial intelligence")
             (li "Program synthesis")
             (li "Computational reflection")
             (li "Supercompilation")
             (li "Multi-stage programming")
             (li "Semi-automated theorem proving")
             ))

         (section
           ,(anchor-target "publications")
           (h2 "Publications")
           (ul ((class "publication-list"))
               (li
                 ,(publication
                    (anchor "https://icfp20.sigplan.org/details/minikanren-2020-papers/10/mediKanren-A-System-for-Bio-medical-Reasoning"
                            "mediKanren: A System for Bio-medical Reasoning")
                    "Michael Patton, Gregory Rosenblatt, William E. Byrd, Matthew Might"
                    `(p "To appear in: miniKanren Workshop, 2020")))
               (li
                 ,(publication
                    (anchor "https://icfp20.sigplan.org/details/minikanren-2020-papers/1/A-Relational-Interpreter-for-Synthesizing-JavaScript"
                            "A Relational Interpreter for Synthesizing JavaScript")
                    "Artem Chirkov, Gregory Rosenblatt, Matthew Might, Lisa Zhang"
                    `(p "To appear in: miniKanren Workshop, 2020")))
               (li
                 ,(publication (anchor "https://www.coalg.org/tease-lp/2020/accelerating-program-synthesis-in-minikanren/"
                                       "Accelerating Program Synthesis in miniKanren")
                               "Robert Zinkov, Michael Ballantyne, Gregory L. Rosenblatt and William E. Byrd"
                               `(p "Workshop on Trends, Extensions, Applications and Semantics of Logic Programming, 2020")))
               (li
                 ,(publication (anchor "http://minikanren.org/workshop/2019/minikanren19-final2.pdf"
                                       "First-order miniKanren representation: Great for tooling and search")
                               "Gregory Rosenblatt, Lisa Zhang, William E. Byrd, Matthew Might"
                               `(p "miniKanren Workshop, 2019")
                               `(p "[" ,(anchor "https://github.com/gregr/first-order-miniKanren" "Implementation and examples") "]")))
               (li
                 ,(publication (anchor "http://papers.nips.cc/paper/7445-neural-guided-constraint-logic-programming-for-program-synthesis"
                                       "Neural Guided Constraint Logic Programming for Program Synthesis")
                               "Lisa Zhang, Gregory Rosenblatt, Ethan Fetaya, Renjie Liao, William E. Byrd, Matthew Might, Raquel Urtasun, Richard Zemel"
                               `(p "Neural Information Processing Systems (NeurIPS), 2018")
                               "[" (anchor "https://arxiv.org/abs/1809.02840"            "arXiv") "] "
                               "[" (anchor "http://lisazhang.ca/NIPS2018_CLP_poster.pdf" "Poster") "] "
                               "[" (anchor "https://github.com/xuexue/neuralkanren"      "Code") "] "
                               "[" (anchor "https://openreview.net/forum?id=HJIHtIJvz"   "Workshop") "]"))
               (li
                 ,(publication (anchor "https://dl.acm.org/citation.cfm?id=3110252"
                                       "A unified approach to solving seven programming problems (functional pearl)")
                               "William E. Byrd, Michael Ballantyne, Gregory Rosenblatt, Matthew Might"
                               `(p "Proceedings of the ACM on Programming Languages.  Volume 1, Issue ICFP, September 2017. Article No. 8")
                               `(p "[" ,(anchor "https://github.com/gregr/icfp2017-artifact-auas7pp" "Reusable artifact") "]")
                               `(p "We present seven programming challenges in Racket, and an elegant, unified approach to solving them using constraint logic programming in miniKanren.")))))

         (section
           ,(anchor-target "talks")
           (h2 "Talks")
           (ul ((class "talk-list"))
               (li
                 ,(talk (anchor "https://www.youtube.com/watch?v=er_lLvkklsk" "Barliman: trying the halting problem backwards, blindfolded")
                        "with William E. Byrd"
                        `(p "[" ,(anchor "http://2016.clojure-conj.org/barliman/" "Clojure/conj 2016") "] "
                            "[" ,(anchor "https://github.com/webyrd/Barliman" "Code") "]")
                        `(p "Barliman is a prototype smart editor capable of program synthesis: given part of a program and a set of tests to pass, Barliman attempts to complete the program for you.")))))

         (section
           ,(anchor-target "work-experience")
           (h2 "Professional experience")
           (ul ((class "employment-list"))
               (li
                 ,(employment "University of Alabama at Birmingham" "Birmingham, AL"
                              '(("Scientist"
                                 "February 2020" "present")
                                ("Bioinformatician"
                                 "September 2017" "February 2020"))
                              `(p "Applying computer science to build medical reasoning tools")
                              `(ul
                                 (li "mediKanren user interface")
                                 (li "Relational programming system extended with graph database and search")
                                 (li "Automated ingestion and reformatting of biological graph data sources"))))
               (li
                 ,(employment "500px" "Toronto, ON"
                              '(("Platform Engineer"
                                 "July 2014" "February 2015"))
                              `(p "Improved platform stability and performance")
                              `(ul
                                 (li "Kafka-based data pipeline prototype")
                                 (li "Refactoring of Rails application to produce microservices written in Go")
                                 (li "Internal libraries/tools that simplify creation of new Go services")
                                 )))
               (li
                 ,(employment "Tulip Retail" "Toronto, ON"
                              '(("Software Engineer"
                                 "October 2013" "June 2014"))
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
                              '(("Software Engineer"
                                 "August 2012" "October 2013"))
                              `(p "Refactored and redesigned the warehouse management system")
                              `(ul
                                 (li "Separation of WMS into an independent service providing a web API")
                                 (li "Design and performance analysis of alternative communication protocols")
                                 (li "Internal tools and automation for development and deployment")
                                 )))
               (li
                 ,(employment "Facebook" "Palo Alto, CA"
                              '(("Software Engineer"
                                 "January 2011" "June 2012"))
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
                              '(("Quantitative Developer"
                                 "May 2006" "August 2009"))
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
           ,(anchor-target "non-work-experience")
           (h2 "Extraprofessional experience")
           (ul ((class "employment-list"))
               (li
                 ,(employment "Funemployment" "Toronto, ON"
                              '(("Independent Researcher"
                                 "February 2015" "September 2017"))
                              `(p "Drawing the future into the present")
                              `(ul
                                 (li "Joint work with William E. Byrd on " ,(anchor "https://github.com/webyrd/Barliman" "Barliman") ", a prototype smart editor capable of real-time program synthesis")
                                 (li "Described " ,(anchor "/hypaeit.html" "Hyperprograms") " for reconciling program comprehensibility and efficiency")
                                 (li "Sketched an " ,(anchor "https://github.com/gregr/demo-livingnet" "alternative to the web")))
                              `(p "Taking the programming out of programming")
                              `(ul
                                 (li "Making the programming a better place")
                                 (li "Fighting the social injustice of programming-inequality")
                                 (li "Increasing the minimum programming to fight poverty")
                                 (li "Providing affordable programming to keep people off the streets")
                                 (li "Putting programming on the table for you and your family")
                                 (li "Cutting programming emissions to slow climate change")
                                 (li "Looking for a cure to programming")
                                 (li "Programming is the leading cause of programming")
                                 (li "1 out of every 1 programmers suffers from programming, but many are in denial")
                                 (li "Staging programming interventions")
                                 (li "Just say 'no' to programming"))))
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
           ,(anchor-target "personal-projects")
           (h2 "Older personal projects")
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
           ,(anchor-target "recommended-reading")
           (h2 "Some reading I've enjoyed and recommend")
           ,(anchor-reference anchor-list-reading)
           )
         )
      (list (atom-feed-link))
      )))
