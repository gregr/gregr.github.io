(define title "Reinvert control with delimited continuations")
(match-define (list ref anchor-list)
  (anchors `(
    (inversion-of-control "Inversion of control" "https://en.wikipedia.org/wiki/Inversion_of_control")
    (composable-cont-tutorial "Composable continuations tutorial" "http://community.schemewiki.org/?composable-continuations-tutorial")
    (quick-dirty-reinvert-ctrl "Quick and dirty reinversion of control" "http://blog.sigfpe.com/2011/10/quick-and-dirty-reinversion-of-control.html")
    (delim-cont-in-scala "Delimited continuations explained (in Scala)" "http://dcsobral.blogspot.ca/2009/07/delimited-continuations-explained-in.html")
    (racket-cont "The Racket Reference: Continuations" "http://docs.racket-lang.org/reference/cont.html")
    )))
(define (local-code filename)
  (local-code-file
    (build-path "writing/reinvert-control-delim-cont/" filename)))
(list title
  (writing-content
    title
    (nav-local '())
    `(section
       ,(date-range "July 25, 2015" "July 26, 2015"))
    `(section
       ((class "summary"))
       (p "While discussing alternatives to javascript in the browser, a friend lamented that even if we were to replace javascript with a better language, we would still not escape \"callback hell\" if we were forced to keep the event loop model.  The problem is that the event loop " ,(ref 'inversion-of-control "inverts control") ".")
       (p "But given a language with delimited continuation operators, it turns out that you can reinvert control.  This article uses the " ,(code-frag "shift") " and " ,(code-frag "reset") " operators in " ,(ref 'racket-cont "racket") " to demonstrate a technique possible in languages such as " ,(ref 'composable-cont-tutorial "scheme") ", " ,(ref 'quick-dirty-reinvert-ctrl "haskell") " and " ,(ref 'delim-cont-in-scala "scala") "."))
    `(section
       (h2 "Event loops and asynchronous operations")
       (code "event-loop.rkt:")
       ,(local-code "event-loop.rkt")
       (p "This module simulates an event loop by spawning worker threads to process asynchronous operations.")
       )
    `(section
       (h2 "Invoke asynchronous ops while passing callbacks")
       (code "user-with-callbacks.rkt:")
       ,(local-code "user-with-callbacks.rkt")
       (p "To invoke asynchronous operations, we pass two first-class functions representing how to proceed when the operation either succeeds or fails.  The callbacks can be seen as manually-lifted first-class continuations.  Taken to the extreme, it can be more difficult to read/write programs in such a style.")
       )
    `(section
       (h2 "Reinverting control")
       (code "event-loop-direct.rkt:")
       ,(local-code "event-loop-direct.rkt")
       (p "We can use the " ,(code-frag "shift") " operator to grab the continuation to which we want to pass the result of an asynchronous operation when invoked in direct-style.  Asynchronous operations transformed in this way can be provided by libraries without any cooperation from the event-loop implementation, demonstrating the technique's general applicability.")
       )
    `(section
       (h2 "Direct-style invocation")
       (code "user-direct.rkt:")
       ,(local-code "user-direct.rkt")
       (p "Finally, we use a " ,(code-frag "reset") " block to indicate the extent of sequentially-executed code associated with a series of " ,(code-frag "async-op-direct") " invocations.")
       (code "user-direct-concurrency.rkt:")
       ,(local-code "user-direct-concurrency.rkt")
       (p "Invocations can be performed concurrently by wrapping them in separate " ,(code-frag "reset") " blocks.  These blocks may be embedded in other blocks to control the timing of invocations, satisfying dependencies on earlier asynchronous results.")
       )))
