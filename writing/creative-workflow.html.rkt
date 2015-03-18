(define title "Reflecting on creative workflow")
(list title
  (writing-content
    title
    (nav-local '())
    `(section
       ,(date-range "March 17, 2015" "March 18, 2015"))
    `(section
       ((class "summary"))
       (p "Some of my friends have asked for advice on how to make progress with their projects.  Specifically, they have trouble moving from a conceptualization phase to a programming phase.  High-level conceptualization and programming are not difficult for them when taken in isolation.  But the gap between these activities seems wide as the high-level concepts are interpreted as large, daunting tasks, causing them to feel overwhelmed.  Even if they had the energy, it's not even clear to them what they should be programming.  If only they had somebody to tell them exactly what to program.  Fortunately, there is a way for them to tell themselves.")
       (p "What seems to be missing is an organizational phase for breaking concepts down into clear, implementable pieces.  Most importantly, thoughts/findings should be written down in some way while operating in this phase in order to reduce the cognitive load of later phases.  Such a process lowers the activation energy required for getting started with the actual programming.  This planning process can be performed gradually, and so has low activation energy requirements itself, completing a cycle requiring only low energy transitions.  I suspect this phase is harder to find and develop because it feels more like work.")
       (p "Some companies incorporate a similar organizational phase into a formal process involving planning documents, formal specifications and project/task management tools.  While you can do these things, for my personal work I use simple text to quickly dump thoughts, take notes and sketch TODO lists without imposing unwanted structure.")
       (p "One problem with typical formal processes is that the structure they impose increases activation energy requirements.  Because of this, even when forced to operate within such a structure, I still work within my own informal organizational process for efficiency.  I then translate to the formal process in batches to communicate with everyone else.")
       (p "One problem with typical formal processes is that the structure they impose increases activation energy requirements.  Fitting the structure becomes an additional problem to solve, adding friction.  Friction kills unborn ideas, so I write freely before structuring.  When forced to operate within a formal process, I still work within my own informal process for efficiency.  I then translate to the formal process in batches to communicate with everyone else.")
       (p "Below is one view of a creative workflow.  I am not entirely sure what the mental degree labels I have chosen should really mean.  They probably have something to do with the type of focus being applied.  Consider this to be a rough sketch.")
       (p "I should use something better than a table to visualize this.  This is actually quite terrible.")
       )
    `(section
       (table ((class "workflow"))
         (tr
           (th "Mental Degree:")
           (th "High")
           (th "Medium")
           (th "[F]Low")
           )
         (tr
           (td ((class "row-header")) "Work")
           (td)
           (td ((class "state"))
               (p "Organize")
               (ul
                 (li "analyze")
                 (li "investigate")
                 (li "plan")
                 (li "break goals down into actionable, low activation energy steps")
                 )
               (p "produces: procedures, facts"))
           (td)
           )
         (tr
           (td ((class "row-header")) "Fun Work")
           (td ((class "state"))
               (p "Explore")
               (ul
                 (li "generate and elaborate ideas")
                 (li "perform thought experiments")
                 (li "find direction"))
               (p "produces: goals, context"))
           (td ((style "text-align: center")) "Proceed clockwise.")
           (td ((class "state"))
               (p "Act")
               (ul
                 (li "follow procedures")
                 (li "implement, refine, test, and debug artifacts")
                 )
               (p "produces: artifacts"))
           )
         (tr
           (td ((class "row-header")) "Fun")
           (td)
           (td ((class "state"))
               (p "React")
               (ul
                 (li "play with artifacts")
                 (li "reflect on experiences")
                 )
               (p "produces: experiences, feedback"))
           (td)
           )
         ))
    ))
