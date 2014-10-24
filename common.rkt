#lang racket
(provide
  (all-defined-out)
  )

(require
  "static-site.rkt"
  gregr-misc/match
  racket/runtime-path
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

(define (anchors anchor-specs)
  (match-let
    (((list anchor-dict rev-anchors)
      (for/fold/match (((list anchor-dict rev-anchors) (list (hash) (list))))
                      (((list key main-desc href) anchor-specs))
        (let ((make-anchor (curry anchor href)))
          (list
            (dict-set anchor-dict key make-anchor)
            (cons (make-anchor main-desc) rev-anchors))))))
    (list (lambda (key desc) ((dict-ref anchor-dict key) desc))
          (reverse rev-anchors))))
(define (anchor-reference anchors)
  `(ul ,@(map (lambda (anchor) `(li ,anchor) ) anchors)))

(match-define (list ref-plt anchor-list-plt)
  (anchors `(
    (relational-prog "miniKanren" "https://www.youtube.com/watch?v=5Q9x16uIsKA")
    (role-of-PL-study "The Role of the Study of Programming Languages" "http://www.cs.indiana.edu/~dfried/mex.pdf")
    (props-as-types "Propositions as Types" "http://homepages.inf.ed.ac.uk/wadler/papers/propositions-as-types/propositions-as-types.pdf")
    (object-capability-model "Object-capability model" "https://en.wikipedia.org/wiki/Object-capability_model")
    (erights "E Programming Language" "http://erights.org")
    )))

(match-define (list ref-arch anchor-list-arch)
  (anchors `(
    (fault-tolerance-high-volume "Fault Tolerance in a High Volume, Distributed System" "http://techblog.netflix.com/2012/02/fault-tolerance-in-high-volume.html")
    (log-unifying-data-abstraction "The Log: What every software engineer should know about real-time data's unifying abstraction" "http://engineering.linkedin.com/distributed-systems/log-what-every-software-engineer-should-know-about-real-time-datas-unifying")
    (beat-cap "How to beat the CAP theorem" "http://nathanmarz.com/blog/how-to-beat-the-cap-theorem.html")
    (consistent-hash "Consistent Hashing" "http://www.tomkleinpeter.com/2008/03/17/programmers-toolbox-part-3-consistent-hashing/")
    (raft-paxos "Raft: A more understandable consensus algorithm that is equivalent to Paxos" "http://www.reddit.com/comments/1c1rjx")
    (das-boundaries "Boundaries" "https://www.destroyallsoftware.com/talks/boundaries")
    )))

(define (nav-local-target target)
  (define (entry tag desc . subentries)
    `(li (p ,(anchor (string-append "#" tag) desc)) ,@subentries))
  (match target
    ((list tag desc)
     (entry tag desc))
    ((list tag desc subtargets)
     (entry tag desc `(ul ,@(map nav-local-target subtargets))))))
(define (nav-local targets)
  `(nav ((id "nav-local"))
        (ul ,@(map nav-local-target targets))))

(define-runtime-path local-directory ".")
(define (code-block source)
  `(pre ((class "code-block")) (code ,source)))
(define (code-file path)
  (code-block (call-with-input-file path port->string)))
(define (local-code-file subpath)
  (code-file (build-path local-directory subpath)))

(define (head sub-title)
  `(head
     (title ,(string-append "Greg Rosenblatt - " sub-title))
     (meta ((charset "utf-8")))
     (meta ((name "author") (content "Greg Rosenblatt")))
     (meta ((name "description") (content "Personal site of Greg Rosenblatt")))
     (link ((rel "stylesheet") (href "/main.css")))
     (script ((src "//ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js")))
     (script ((src "/main.js")))
     ))
(define nav-main
  `(nav ((id "nav-main"))
        (ul
          (li ,(node-ref 'about))
          (li ,(node-ref 'writing))
          )))

(define (content sub-title nav-local body)
  `(html
     ,(head sub-title)
     (body
       (div ((id "content"))
            (div ((id "nav-panel"))
                 ,nav-main
                 ,nav-local
                 )
            (div ((id "content-main")) ,body)
            ))))
