#lang racket
(provide about)

(require
  "common.rkt"
  "static-site.rkt"
  )

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
  `(nav ((id "nav-local"))
        (ul
          (li ,(anchor "#summary" "Summary"))
          (li ,(anchor "#personal-projects" "Personal Projects"))
          (li ,(anchor "#work-experience" "Professional Experience"))
          (li ,(anchor "#education" "Education"))
          (li ,(anchor "#recommended-reading" "Recommended Reading"))
          )))

(define about
  (content
    "About"
    nav-about
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
                            "October 2013" "June 2014"
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
