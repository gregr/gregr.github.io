(define title "Staying sane while writing software")
(list title
  (writing-content
    title
    `(section
       ,(date-single "June 22, 2014")
       )
    `(section
       (p "This is a collection of principles and techniques that lead to useful, quality software while preserving the sanity of the programmers writing it.  Mental health and project success go hand-in-hand.")
       (p "Not all of the principles mentioned here can hold in all cases.  Some may conflict in a particular setting.  But where possible, applying these ideas seems to lead to the most satisfying results.")
       (p "Currently this is just an outline.")
       )
    `(section
       (h2 "Attributes of desirable software")
       (h3 "What most people (should) want")
       (ul
         (li "features!")
         (li "privacy")
         (li "security")
         (li "robustness")
         (li "performance"
             (ul
               (li "responsiveness")
               (li "low resource consumption")
               ))
         )
       (h3 "What programmers also want")
       (ul
         (li "repeatable behavior")
         (li "incremental effects")
         (li "local reasoning")
         (li "easy rearrangement")
         (li "generalization")
         (li "customization")
         )
       )
    `(section
       (h2 "Qualities that help satisfy these desires")
       (h3 "Relatively abstract")
       (ul
         (li "idempotence")
         (li "monotonicity")
         (li "commutativity")
         (li "orthogonality")
         (li "composability")
         )
       (h3 "Somewhat more concrete")
       (ul
         (li "principle of least authority")
         (li "redundancy")
         (li "tolerance")
         (li "isolation")
         (li "efficiency")
         (li "scalability")
         )
       )
    `(section
       (h2 "Some techniques that can provide these properties")
       (h3 "Broad concepts")
       (ul
         (li "modular design")
         (li "decentralized authority"
             (ul
               (li "distribution of resources, labor and responsibility")))
         (li "granular delegation of authority"
             (ul
               (li "as in the "
                   ,(anchor-object-capability-model "object-capability model"))
               ))
         (li "clear, incorruptible sources of truth")
         )
       (h3 "More specific architectural choices")
       (ul
         (li "purely functional cores with imperative shells"
             (ul
               (li ,(anchor-das-boundaries "Boundaries"))
               ))
         (li ,(anchor-object-capability-model "object capabilities")
             (ul
               (li ,(anchor-erights "as in the E Programming Language"))
               ))
         (li "caching")
         (li "replication")
         (li "append-only databases"
             (ul
               (li "event sourcing")
               (li ,(anchor-log-unifying-data-abstraction "the log as a unifying data abstraction"))
               (li ,(anchor-beat-cap "Beating the CAP Theorem"))
               ))
         (li "consistent or rendezvous hashing")
         (li "SOA (in the microservices sense)"
             (ul
               (li "smart endpoints, dumb pipes")
               (li ,(anchor-fault-tolerance-high-volume "service failure resilience"))
               ))
         (li "REST (in the hypermedia-driven sense)")
         )
       )
    ))