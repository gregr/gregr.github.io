# Have your program and eat it too: eliminating tradeoffs between comprehensibility and efficiency

*February 11, 2016 - November 14, 2016*

Gregory Rosenblatt


## Abstract

We describe hyperprograms and their supporting tools as an attempt to achieve both comprehensibility and efficiency without compromise.  A hyperprogram consists of multiple semantically-equivalent programs along with smooth transitions between them.  Each program within a hyperprogram provides a medium for focusing on some facet of comprehensibility and efficiency without being needlessly disrupted by actions taken with its sibling programs.


## Introduction

"Weak human + machine + better process was superior to a strong computer alone and, more remarkably, superior to a strong human + machine + inferior process." -- Garry Kasparov [[1](#1)]

### Problem

We would like our programs to be both comprehensible and efficient, but these two qualities seem to be at odds.  Is it possible to maximize both?  Maximizing efficiency requires precise allocation of computational resources.  Maximizing comprehensibility requires focusing only on the essential concerns of the problem being solved, making use of abstractions that hide the inessential details of how computations are carried out.  But resource allocation is one such inessential detail that is left implicit, making a loss of efficiency seem unavoidable.

Optimizing compilers can automatically mitigate some of this loss in efficiency, allowing programs to continue using abstractions without paying as much in performance.  However, such automation is not one-size-fits-all, with competing factors such as the resource consumption, effectiveness, and predictability of the optimizer.  Due to the large search space, as problem complexity grows, identifying optimization opportunities becomes disproportionately expensive and unreliable.  Small changes to a program may lead to significant, unpredictable changes in performance.

Having access to runtime data, just-in-time compilers cope with a smaller search space than ahead-of-time compilers, which would normally enable a better effectiveness to analysis ratio.  But acting at runtime, they are also more resource constrained, often only having time to perform simple optimizations.  Being dependent on predictable runtime data flow and hotspots, they are also less predictable regarding whether, when, and how optimizations manifest.

Problem-specific knowledge can help tame the optimization search space before runtime.  Though specialized optimizers exist for some domains, in general the responsibility for applying problem-specific knowledge falls to the programmer [[18](#18)].  Armed with the right priorities and appropriate profiling information, the programmer can focus on just the problematic portions of a program, manually tuning them to dramatically improve performance.  And because these improvements are explicitly encoded, they will not unpredictably vanish.  Unfortunately, the conflict is still present.  Performance tuning may be algorithmic, employing more efficient computational strategies, but these strategies are often more complex than the straightforward processes they replace.  Tuning may also involve specialization, which replaces expensive, general-purpose abstractions with problem-specific computational management, but this management is the distraction we wanted to avoid.  For programs written in high level languages, specialization may even involve rewriting critical portions in a lower level language, where computational management becomes even more explicit.  While they may achieve great efficiency, complex algorithms and specialization greatly reduce comprehensibility, compromising maintainability.

In terms of the comprehensibility and efficiency tradeoff, general-purpose automation and problem-specific manual tuning each seem to be strong where the other is weak.  Is there some way to reconcile the two approaches to get the best of both?


### Hyperprograms

It may be possible to simultaneously maximize both comprehensibility and efficiency of programs using tools that maintain multiple, semantically-equivalent views of the same computation.  These views are themselves programs expressing this computation.  We refer to a set of such programs, along with evidence of their semantic equivalence, a hyperprogram.  As an example, one subset of these programs could consolidate meaning in a high level, maximally comprehensible manner, while another subset could elaborate computational details for efficiency.  The evidence of equivalence enables tooling for smooth transition between all these programs, allowing the programmer to navigate between the most relevant computational views for the task at hand.

Hyperprograms support factoring the task of programming into separate reasoning activities, allowing each activity to proceed without impeding the others unnecessarily.  Taken together, the programs of a hyperprogram can constitute the programmer's mental model of that computation.  Each program emphasizes qualities useful for a particular reasoning task.  The highest level program will correspond most closely to the desired outcome or meaning, ideally formulated as a specification or an intuitive representation.  Other programs will likely highlight different facets of a computational strategy, with the lowest levels mapping directly to machine implementations.

A hypothetical process for creating new hyperprograms may first involve either direct manipulation [[42](#42)] or symbolic specification in a declarative programming language.  If performance is not acceptable, hotspots would be identified through profiling, then elaborated in one or more new programs.  This elaboration would likely begin with algorithmic and data representation improvements in a high level language.  If the improvements expressible in this high level language are not sufficient, the programmer may descend to a lower level for more control.

This formulation of programming produces predictable, observable transformation artifacts as additional programs.  Unlike the ephemeral output of typical automated tools, programs are produced and ultimately controlled by the activity of programming.  They will not vanish or change violently without explanation.  When changes in meaning do occur, dependent programs will be updated and any conflicting programmer decisions will be highlighted.  Aided by the evidence of equivalence, robustness can be achieved through programmable tactics and goal-oriented search techniques, decreasing the occurrence of conflicts compared to a fully manual effort.

Seen this way, hyperprograms provide access to a spectrum of transformation activities between the fully automated and the fully manual.  If desired, they can still be used to support the extreme cases without additional effort, as the degenerate case of a hyperprogram is just a normal program.


### Proposal

To explore hyperprogramming, we propose a minimal set of tools including:

* a small, extensible programming language for exploring various paradigms [[2](#2), [3](#3)]
* an intermediate notation for layered languages [[4](#4)]
* a logical framework for equational reasoning with this notation [[9](#9), [11](#11)]
* meaning-preserving, compositional program transformations within and across layers for symbolic exploration
* programmable tactics for robust, problem-specific driving of these transformations
* goal-oriented, interactive transformation search [[12](#12), [23](#23)]
* transformation-aware version control for hyperprograms and their programs
* provenance-tracking and visualization to indicate related subterms across programs [[30](#30)]

With these tools, we can build a hyperprogramming editor.  Such an editor would support operations for creating new linked programs that, despite possible differences in formulation, always have the same behavior.  Changes to one program would be reflected in the other.  Aside from basic editing to define and change hyperprogram behavior, the editor would provide access to interactive and programmable transformation commands guaranteed to not change hyperprogram behavior, allowing programs to diverge in interesting ways while maintaining equivalence.

If these tools prove useful, a more complete set could be worth developing, including:

* a set of abstract machine models supporting compilation to/from various programming languages
* cost semantics for abstract profiling [[31](#31), [32](#32), [33](#33), [34](#34)]
* type systems, property verification, abstract debugging and other static analyses defined using the logical framework
* program synthesis for editing assistance, test generation and inferring examples to automatically document programs [[36](#36), [35](#35)]
* extending version control to a database and distribution system for collaborative program composition
* an editor supporting extremely high-level programming via non-symbolic program elements [[44](#44)] and direct manipulation [[39](#39), [40](#40), [41](#41), [43](#43)]


## Tools With the Right Affordances

To conduct our exploration, we need tools appropriate for hyperprogramming.  We now describe a minimal set of tools that should suffice.


### Extensible language

Because our tooling must support a wide variety of uses, some of which may not have been anticipated, it is important to build on a flexible foundation, capable of providing multiple forms of programming and metaprogramming.  Embedded interpreters provide the natural extensibility we need to be this flexible.  For this reason, we start with a simple, homoiconic language.  Homoiconicity enables convenient metalinguistic abstraction [[2](#2)], making it less disruptive to invoke our embedded interpreters.


### Programming paradigms and layered structure

Peter Van Roy categorizes programming paradigms [[3](#3)], describing their relationships in terms of the concepts they consist of and discussing their properties, such as observable nondeterminism.  Interesting concepts include deterministic concurrency and constraints.  A definitive language is described as having a layered structure [[4](#4)], each incorporating some combination of the following concepts: functional programming, deterministic concurrency, message-passing concurrency and shared-state concurrency.  To be useful, our tools must such concepts.

To work with multiple paradigms manageably, we will layer our concepts.  Concepts introducing similar complexity are grouped in in the same layer, with subsequent layers indicating increased expressiveness at the cost of more difficult reasoning.  In order of increasing expressiveness, the layers we've chosen include:

* functional programming: simple data, first-class procedures
* deterministic concurrency: single-writer dataflow, laziness, constraints, logical monotonicity [[5](#5), [6](#6)]
* nondeterminism: asynchrony, message-passing concurrency, probabilistic choice [[7](#7)]
* sequential control: delimited continuations, coroutines, implicit context

The core functional layer is chosen to be small, strict, pure, and untyped for the following reasons:

* small and strict: to describe deterministic computational processes with simple, sequential operational semantics
* pure: for strong compositional reasoning
* untyped: for versatility and simpler metaprogramming

We introduce subsequent concepts only as needed.  Language-level support for a concept is needed when introducing the concept would otherwise require nonlocal program transformations (page 9 of [[4](#4)]).  To only pay for additional complexity when necessary, concept support may be added compositionally by locally invoking an interpreter with the appropriate semantics.


### Intermediate notation and logical framework for equational reasoning

To leverage existing work and yield a simple logical framework, we base our intermediate notation on the call-by-value lambda calculus.  To support the functional programming layer, this notation includes bits, pairs, and single-argument procedures.  Depending on the use case, additional concepts may be supported either by functional modelling or by extending the notation with impure terms.  Functional modelling allows the most re-use of existing machinery but may not support as precise a level of reasoning as a dedicated extension.  An example of improved reasoning through better notation are the impure variable binders [[10](#10)] of John Shutt.  In order to study an abstraction based on Fexprs [[9](#9)], Shutt develops several vau-calculi, assessing the term equivalences they can express.  The impure vau-calculi developed are well-behaved, introducing control effects and mutable state without losing Church-Rosser-ness of the step relation.  Unlike lambdas, state and control variable binders bubble up ahead of escaping references, dynamically maintaining lexical scope.  This technique seems useful for reasoning about effects in general and seems difficult with functional modelling alone.

To represent, produce, and manipulate evidence of equivalence, we need tools for equational reasoning.  A minimal logical framework based on the operational semantics of our intermediate notation should suffice.  Rather than introduce the complexities of a multi-tiered reasoning system involving higher order type theory, we choose a computational logic similar to ACL2 [[12](#12)] and Milawa [[11](#11)].  Milawa is a self-verifying theorem prover that puts an ACL2-like system on firmer ground by bootstrapping it from a simple proof checker, using a reflection rule [[14](#14)] to install progressively more sophisticated proof checkers that are proved faithful to the original.  Its logic describes term equality axioms similar to the step relation of an operational semantics.  We prefer this style of logic because it leverages the operational intuitions developed while learning how to program, and seems to complement our use of metalinguistic abstraction.  We believe such a logic:

* corresponds to common programmer mental models for evaluation, composition and generalization
* presents a shorter learning curve than higher order type theory
* is capable of expressing various type systems and analysis frameworks, supporting pluralism


### Meaning-preserving transformation, programmable tactics, and goal-oriented search

Equationally reasoning about programs requires the ability to transform them while preserving their meaning.  A transformation is meaning-preserving if it only replaces a term with an operationally-equivalent one.  The simplest of these transformations are based on the small step relation of our logic, allowing transition in either direction.  Such transformations can be thought of as being justified by extremely simple equivalence proofs.  Complex transformations are enabled by complex equivalence proofs, possibly involving term induction.

There is also a lateral direction to consider when traversing computational models.  Generally, we have three basic directions corresponding to different programmer intentions and activities:

* up: consolidation of meaning; refactoring
* down: elaboration; optimization
* lateral: reinterpretation; compilation

Direct, manual use of basic transformations is a relatively low-level mode of operation, useful for precise symbolic exploration and fine-tuning of a computational process.  A more typical mode of operation involves programmer-definable tactics that compose the basic transformations to carry out more sophisticated rewritings, allowing the programmer to focus at the right level of abstraction for a given problem.

The simplest tactics behave like basic transformations that replace terms of larger granularity.  For instance, when manipulating computations occurring in an embedded domain-specific language, it makes more sense for the default step-like transformation a programmer reaches for to correspond to the step relation of the embedded language rather than that of the host language.  In contrast, when the intention is to specialize the invocation of an embedded language interpreter, a compilation activity, host language transformations become relevant again.  However, rather than manually applying the host language's step relation, it would be more effective to use tactics designed specifically for compilation.

While programmed tactics should be an effective way to automate transformations, it may in general be more intuitive to develop rewrite rules by providing goals to a search system in the style of ACL2 [[12](#12)].  ACL2 is a theorem prover based on a computational logic.  Rather than programmable tactics, the preferred ACL2 proof method [[13](#13)] involves an intuitive, interactive, recursive goal-oriented search.  The user specifies a goal and interprets failure feedback to choose lemmas to prove as subgoals.  In doing so, the user builds up a library of rewrite rules capable of automatically proving the original goal.

By providing an ACL2-inspired search system, users can remain focused on high-level problems, only delving into low-level details when the system needs new suggestions.  The system becomes more effective as the library of rewrite rules is gradually built up, naturally developing capabilities similar to a domain-specific solver.  Aside from being more intuitive, goal-oriented search should also be more robust to small changes in program semantics as the user only specifies the source and target of a goal rather than the path to achieving it.  Once a sufficient rewrite rule library is developed, it may also be possible to obtain some forms of program verification for free.  By encoding program properties as predicates in the programming language, verification conditions may be asserted with rewrite rules.  For instance, program 'x' has property 'P' if 'P("x")' ('P' applied to the quoted program 'x') is equivalent to the boolean value 'True': if the search system succeeds in such a rewrite, the property holds. [[16](#16)]

ACL2 employs term-rewriting strategies that happen to resemble supercompilation [[23](#23)], suggesting another source of existing work we can leverage.  Supercompilation [[21](#21), [22](#22), [23](#23)] is a powerful whole-program approach to optimization that, while showing great potential, has had trouble scaling to the general case.  Issues include intense compile-time resource usage and unnecessarily large increases in program size [[25](#25)].  These issues stem from an inability to focus efforts on genuinely important parts of a program.  By limiting the scope of its application to hotspots, a programmer may be able to guide a supercompiler well enough to mitigate these disadvantages.  We hope to experiment with supercompilation both as a directly-applied tactic and for proving term equivalences [[23](#23)] as part of a goal-oriented search implementation.


### Version control and provenance

Rather than textual diffs, histories of semantic actions such as edits and transformations are more useful representations of program changes.  Since they carry programmer intent, semantic actions allow tools that work with multiple versions of a program to more intelligently compare, merge, and undo changes.

Additionally, each program of a hyperprogram should be stored with enough information to conveniently derive provenance.  When transforming a program, we want to be able to correlate the transformed subterms of our resulting program with their sources in the original.  A simple way to achieve this is to tag terms with unique annotations, then track the flow of these annotations during transformation.  By highlighting terms with shared annotations in the original and final programs, we provide linked representations [[41](#41)] for enhanced reasoning.


## Extended tools

If our initial experiment proves successful, we anticipate needing an extended set of tools to completely eliminate tradeoffs between comprehensibility and efficiency.  We briefly describe existing work we could leverage or draw inspiration from in developing these tools.


### Cost semantics

Guy Blelloch defines a cost semantics for the call-by-value lambda calculus [[31](#31)] which is capable of expressing available parallelism without requiring a machine model.  Continuations of this work go on to explore space profiling [[32](#32)] and the impact of memory hierarchy on efficiency [[33](#33)].  These techniques should be capable of measuring the effectiveness of program elaborations even before targeting concrete machines.


### Synthesis

QuickCheck [[35](#35)] is capable of formulating and automatically testing program properties specified as Haskell functions by providing randomly-generated input.  Generation can be adjusted on a case-by-case basis to ensure that sufficiently interesting inputs are being tested.  Generators may be transformed and composed to produce new generators of arbitrary complexity.  This approach should be effective in other settings, such as automating documentation of a function by providing examples of its use.  Example invocations of a complex function may be inferred by composing generators defined for the elements of its implementation.

William Byrd demonstrates relational interpreters [[36](#36)] capable of generating programs that evaluate to a specified result.  Though tractability is a concern, a relational interpreter may be a useful component to a programming assistant, able to generate subterms with desired properties.


### Direct manipulation and linked representation

Bret Victor demonstrates [[39](#39), [40](#40), [41](#41)] that direct manipulation [[42](#42)] of a medium can be significantly more effective than indirect, symbolic programming of it.  One example [[40](#40)] transforms drawing into a medium for highly comprehensible dynamic graphical programming.  Another set of demonstrations [[41](#41)] illustrate working with direct representations of various systems.  All the techniques on display are relevant, particularly multiple representations, and links for explaining provenance and other relationships.

SKETCH-N-SKETCH [[43](#43)] is a tool for programming with two computational views: one symbolic, the other for graphical direct manipulation.  Meaning is preserved by reconciling updates in one view with corresponding updates in the other using triggers and trace-based synthesis.

David M. Barbour discusses an idea for embedding interactive, non-symbolic value representations [[44](#44)] within a symbolic program.  It lists examples including representations for sound, images, animations, 3D models, spreadsheets and other elaborate structures.

Before a set of tools for hyperprogramming can really be considered complete it should incorporate direct manipulation views as programs.  It should be possible to extend support to any medium representable by the hardware.


## Building Bridges

For our work to be relevant we need to produce a self-applicable system capable of interacting with real hardware, then apply this sytem to existing programs and languages.  A long-term plan for achieving relevance follows.


### Bootstrapping

We would like our tools to be self-applicable so that their implementations may also benefit from hyperprogramming.  Ideally this would include self-applicability in the sense of Futamura projections [[17](#17)].

Obtaining self-applicable versions of our tools requires us to somehow implement them with our example programming language.  For this language to even materialize in the first place we must first produce bootstrap versions of some of the tools, including an interpreter.

To derive the most benefit from our efforts, we closely match our functional syntax to the intermediate notation, even implementing the functional layer primitives using the intermediate notation itself.  Primitives in hand, we complete our bootstrap interpreter with a simple functional syntax parser, assembler, and evaluator for the intermediate notation.  The rest of the functional layer is bootstrapped using its own syntactic extension facilities.

Subsequent layers are initially implemented as interpreters in the functional layer.  Communication outside of the functional model can be achieved by connecting it to a simple input/output trampoline that implements the desired effects.  Effect invocation takes the form of a functional computation returning a pair containing the encoding for an effect request and a continuation to pass the response to.  Given the response, the continuation will proceed with the remainder of the program, at least until the next effect request.  The trampoline responds to effect requests until the program returns a final result, tagged as such.

Ultimately we wish to eliminate interpretive overhead and leverage hardware support for parallelism, storage, etc.  To do so, we build machine models that allow us to reason about and target concrete machines for compilation.


### Plugging into the ecosystem

Practical usefulness of our tools depends on them being applicable to existing programming languages.  They must be plugged into the system to provide them first class status.  As languages are plugged into the system, the emerging topology resembles a hub and spokes; aside from providing access to our tools, adding an edge to a new language also implies an ability to translate between that language and any other language in the topology.

Abstract models for each language of interest can be gradually constructed.  Models do not have to be complete before they can be useful: they do not have to constitute full language specifications, or even support the full language.  To model the language subset of interest, we implement its interpreter in a language already accessible to our tools.  Since the interpreter provides meaning, we can derive bi-directional compilation between the language and our system.  This does not immediately yield optimal reasoning tools for the new entrant, but does remove significant barriers to developing them.

We recognize that integration is a significant undertaking, but since abstract models do not have to be complete to be useful, providing language support is not an all-or-nothing endeavor.  We are hopeful that a minimal system demonstrating convincing results will produce enough interest to attract help.  And as help arrives and more languages are introduced and available for interoperation, adding the next language becomes increasingly compelling.  Interested programming language communities will also be the most qualified to build the models they will need.

Aside from basic language models, there is also plenty of room for communities to develop and share tactics and goal-oriented search libraries.  By focusing on their areas of expertise, everyone can benefit from having the best cross-domain reasoning tools available for the least amount of effort.


### Experimentation and assessment

A meaningful experiment should assess the effectiveness of hyperprogramming across a representative set of programming activities.  We now describe such a set of activities and corresponding success criteria.


#### Implementing specifications

Given a very high level program (perhaps described in constraint logic), is it straightforward to derive a realistic implementation?  To be realistic, an implementation must employ reasonable algorithms.  For instance, embedding of expensive operations from the specification language, such as search, would be limited to situations where they were called for.  Cases where these features were only used for concise communication of intent would be specialized away.


#### Problem-specific optimization

Is it natural to take the solution to a general problem and optimize it for a constrained version of the same problem?  It should be possible to express well-known domain-specific optimizations, such as algebraic simplification and specializing matrix multiplication to known matrix sizes.  The Shonan challenge [[29](#29)] could provide good test material.


#### General-purpose optimization

Can we express common transformations performed by optimizing compilers?  Examples include dead code elimination, common subexpression elimination, constant propagation, unboxing, inlining, loop optimization, and deforestation.


#### Compilation to machine targets

Given a machine model to target, is it possible to generate reasonable code for that machine?  This would include reasonable instruction selection and register allocation.  Ideally it would also be possible to perform machine-dependent optimizations, with consideration to memory hierarchy and leveraging available parallelism.


#### Refactoring

Is it possible to perform typical refactoring transformations?  This includes renaming, relocating fragments, and generalizing patterns to produce meaningful abstractions.  It should also be possible to provide automation that is at least as good as what existing IDEs provide.

Going to the extreme, this activity also encompasses inferring specifications for existing programs.  It should be possible for a programmer to clarify a program's meaning by lifting portions of it to a higher level language.


#### Response to change

Are programs robust in response to hyperprogram modifications?  Are behavior-preserving algorithm substitutions and data representation adjustments needlessly disruptive?  Conflicts will certainly arise when a hyperprogram is updated with different behavior.  Successful tools will shield the programmer from unimportant conflicts that do not deserve attention.


## Conclusion

Hyperprogramming makes use of a different kind of infrastructure than that which is currently used.  Before we can properly investigate the effectiveness of hyperprogramming we must invest in new tools and repurpose some existing ones.  Lack of appropriate infrastructure would have inhibited exploration of this approach in the past.  In fact, the idea may have eluded awareness completely due to the absence of tool affordances that even suggest it.

Once we have the proper tools, we will be able to test whether hyperprograms can, in practice, support both comprehensibility and efficiency for the same computation better than fully automated and fully manual efforts.  We may finally be able to have our cake and eat it too.


## References

<a name="1"></a>[1] Garry Kasparov.  The Chess Master and the Computer.
[http://www.nybooks.com/articles/2010/02/11/the-chess-master-and-the-computer/](http://www.nybooks.com/articles/2010/02/11/the-chess-master-and-the-computer/)

<a name="2"></a>[2] Harold Abelson, Gerald J. Sussman.  Structure and Interpretation of Computer Programs, 2nd edition.
MIT Press, Cambridge, MA, USA, (ISBN 0262011530), 1996.

<a name="3"></a>[3] Peter Van Roy.  Programming Paradigms for Dummies: What Every Programmer Should Know.
G. Assayag and A. Gerzso (eds.) New Computational Paradigms for Computer Music, IRCAM/Delatour, France, 2009.

<a name="4"></a>[4] Peter Van Roy.  Convergence in Language Design: A Case of Lightning Striking Four Times in the Same Place.
8th International Symposium on Functional and Logic Programming (FLOPS 2006), Fuji Sosono, Japan, Springer LNCS 3945, April 2006, pp. 2-12.

<a name="5"></a>[5] Lindsey Kuper, Ryan R. Newton.  LVars: Lattice-based Data Structures for Deterministic Parallelism.
Proceedings of the 2nd ACM SIGPLAN workshop on Functional high-performance computing, Boston, Massachusetts, USA, September 23-23, 2013.

<a name="6"></a>[6] CALM: consistency as logical monotonicity.
[http://bloom-lang.net/calm/](http://bloom-lang.net/calm/)

<a name="7"></a>[7] Noah D. Goodman, Joshua B. Tenenbaum. Probabilistic Models of Cognition.
[https://probmods.org/](https://probmods.org/)

<a name="8"></a>[8] Hanne Riis Nielson, Flemming Nielson.  Semantics with Applications: A Formal Introduction.
Wiley Professional Computing, (240 pages, ISBN 0 471 92980 8), Wiley, 1992.

<a name="9"></a>[9] John N. Shutt.  Fexprs as the basis of Lisp function application or $vau: the ultimate abstraction.
PhD thesis, Worcester Polytechnic Institute, September 2010.

<a name="10"></a>[10] John N Shutt.  Continuations and term-rewriting calculi.
[http://fexpr.blogspot.ca/2014/03/continuations-and-term-rewriting-calculi.html](http://fexpr.blogspot.ca/2014/03/continuations-and-term-rewriting-calculi.html)

<a name="11"></a>[11] Jared Davis. A Self-Verifying Theorem Prover.
PhD dissertation. The University of Texas at Austin. December, 2009

<a name="12"></a>[12] Matt Kaufmann, J Strother Moore.  ACL2: A Computational Logic for Applicative Common Lisp.
[https://www.cs.utexas.edu/users/moore/acl2/](https://www.cs.utexas.edu/users/moore/acl2/)

<a name="13"></a>[13] Matt Kaufmann, J Strother Moore.  A Brief ACL2 Tutorial.
[https://www.cs.utexas.edu/users/kaufmann/tutorial/rev3.html](https://www.cs.utexas.edu/users/kaufmann/tutorial/rev3.html)

<a name="14"></a>[14] John Harrison.  Metatheory and reflection in theorem proving: A survey and critique.
Technical Report CRC-053, SRI Cambridge, Millers Yard, Cambridge, UK, 1995.

<a name="15"></a>[15] Adam Chlipala et al.  Bedrock, a Coq library for verified low-level programming.
[http://plv.csail.mit.edu/bedrock/](http://plv.csail.mit.edu/bedrock/)

<a name="16"></a>[16] Yin Wang.  Propositions as programs.
[https://yinwang0.wordpress.com/2012/04/26/propositions-as-programs/](https://yinwang0.wordpress.com/2012/04/26/propositions-as-programs/)

<a name="17"></a>[17] Dan Piponi.  The Three Projections of Doctor Futamura.
[http://blog.sigfpe.com/2009/05/three-projections-of-doctor-futamura.html](http://blog.sigfpe.com/2009/05/three-projections-of-doctor-futamura.html)

<a name="18"></a>[18] Daniel J. Bernstein.  The death of optimizing compilers.
[http://cr.yp.to/talks/2015.04.16/slides-djb-20150416-a4.pdf](http://cr.yp.to/talks/2015.04.16/slides-djb-20150416-a4.pdf)

<a name="19"></a>[19] Stephen Weeks.  Whole-program compilation in MLton.
Proceedings of the 2006 workshop on ML, p.1-1, September 16-16, 2006, Portland, Oregon, USA.

<a name="20"></a>[20] Urban Boquist.  Code Optimisation Techniques for Lazy Functional Languages.
PhD thesis, Chalmers University of Technology, Sweden, 1999.

<a name="21"></a>[21] Max Bolingbroke.  Call-by-need supercompilation.
PhD thesis, Computer Laboratory, University of Cambridge, 2013.

<a name="22"></a>[22] Peter A. Jonsson, Johan Nordlander.  Taming code explosion in supercompilation.
Proceedings of the 20th ACM SIGPLAN workshop on Partial evaluation and program manipulation, Austin, Texas, USA, January 24-25, 2011.

<a name="23"></a>[23] Ilya Klyuchnikov, Sergei Romanenko.  Towards higher-level supercompilation.
Second International Workshop on Metacomputation in Russia, pages 82-101, Ailamazyan University of Pereslavl, 2010.

<a name="24"></a>[24] Neil Mitchell.  Comment: The state and future of supercompilation in GHC.
[https://www.reddit.com/r/haskell/comments/2s97d0/the\_state\_and\_future\_of\_supercompilation\_in\_ghc/cnngoxt](https://www.reddit.com/r/haskell/comments/2s97d0/the\_state\_and\_future\_of\_supercompilation\_in\_ghc/cnngoxt)

<a name="25"></a>[25] Simon Peyton Jones.  Comment: The state and future of supercompilation in GHC.
[https://www.reddit.com/r/haskell/comments/2s97d0/the\_state\_and\_future\_of\_supercompilation\_in\_ghc/cnx2saj](https://www.reddit.com/r/haskell/comments/2s97d0/the\_state\_and\_future\_of\_supercompilation\_in\_ghc/cnx2saj)

<a name="26"></a>[26] Andrew W. Appel, Trevor Jim.  Shrinking lambda expressions in linear time.
Journal of Functional Programming, v.7 n.5, p.515-540, September 1997

<a name="27"></a>[27] Andrew Taylor.  PARMA-bridging the performance gap between imperative and logic programming.
Journal of Logic Programming, 29(1-3), 1996.

<a name="28"></a>[28] Peter Lodewijk Van Roy.  Can Logic Programming Execute as Fast as Imperative Programming?
PhD dissertation, Technical Report UCB/CSD 90/600, Computer Science Division, UC Berkeley, December 1990.

<a name="29"></a>[29] Oleg Kiselyov.  Modular, convenient, assured domain-specific optimizations.
[http://okmij.org/ftp/meta-programming/Shonan1.html](http://okmij.org/ftp/meta-programming/Shonan1.html)

<a name="30"></a>[30] Roly Perera, Umut A. Acar, James Cheney and Paul Blain Levy.  Functional Programs That Explain Their Work.
ICFP 2012: 17th ACM SIGPLAN International Conference on Functional Programming, Copenhagen, Denmark, 2012.

<a name="31"></a>[31] Guy E. Blelloch, John Greiner.  Parallelism in Sequential Functional Languages.
Proceedings of the Symposium on Functional Programming and Computer Architecture, pages 226-237. June 1995.

<a name="32"></a>[32] Daniel Spoonhower, Guy E. Blelloch, Robert Harper and Phillip B. Gibbons.  Space profiling for parallel functional programs.
Journal of Functional Programming. 20 (5-6), 2011.

<a name="33"></a>[33] Guy E. Blelloch, Robert Harper.  Cache and I/O efficent functional algorithms.
POPL '13 Proceedings of the 40th annual ACM SIGPLAN-SIGACT symposium on Principles of programming languages, Pages 39-50. January 2013.

<a name="34"></a>[34] Guy E. Blelloch.  Cost Models based on the Lambda-Calculus or The Church Calculus the Other Turing Machine.
[https://www.cs.cmu.edu/~guyb/papers/lambdaInria.pdf](https://www.cs.cmu.edu/~guyb/papers/lambdaInria.pdf)

<a name="35"></a>[35] Koen Claessen, John Hughes.  QuickCheck: a lightweight tool for random testing of Haskell programs.
Proceedings of the fifth ACM SIGPLAN international conference on Functional programming, p.268-279, September 2000.

<a name="36"></a>[36] William E. Byrd, Eric Holk, Daniel P. Friedman.  miniKanren, Live and Untagged: Quine Generation via Relational Interpreters (Programming Pearl).
2012 Workshop on Scheme and Functional Programming, September 2012.

<a name="37"></a>[37] Bret Victor.  Learnable Programming.
[http://worrydream.com/LearnableProgramming/](http://worrydream.com/LearnableProgramming/)

<a name="38"></a>[38] Bret Victor.  Magic Ink.
[http://worrydream.com/MagicInk/](http://worrydream.com/MagicInk/)

<a name="39"></a>[39] Bret Victor.  Inventing on Principle.
[http://vimeo.com/36579366](http://vimeo.com/36579366)

<a name="40"></a>[40] Bret Victor.  Drawing Dynamic Visualizations.
[http://worrydream.com/DrawingDynamicVisualizationsTalkAddendum/](http://worrydream.com/DrawingDynamicVisualizationsTalkAddendum/)

<a name="41"></a>[41] Bret Victor.  Media for Thinking the Unthinkable.
[http://worrydream.com/MediaForThinkingTheUnthinkable/](http://worrydream.com/MediaForThinkingTheUnthinkable/)

<a name="42"></a>[42] Ben Shneiderman.  Direct Manipulation: A Step Beyond Programming Languages.
Computer, August 1983.

<a name="43"></a>[43] Ravi Chugh, Brian Hempel, Mitchell Spradlin, Jacob Albers.  Programmatic and Direct Manipulation, Together at Last.
arXiv:1507.02988v2 [cs.PL] 20, Nov 2015

<a name="44"></a>[44] David M. Barbour.  Embedded Literal Objects.
[https://awelonblue.wordpress.com/2014/07/22/embedded-literal-objects/](https://awelonblue.wordpress.com/2014/07/22/embedded-literal-objects/)

<a name="45"></a>[45] Jonathan Edwards.  Subtext programming language.
[http://www.subtext-lang.org/](http://www.subtext-lang.org/)

<a name="46"></a>[46] Gilad Bracha.  Debugging Visual Metaphors.
[http://gbracha.blogspot.ca/2008/07/debugging-visual-metaphors.html](http://gbracha.blogspot.ca/2008/07/debugging-visual-metaphors.html)

<a name="47"></a>[47] Gilad Bracha.  Debug Mode is the Only Mode.
[http://gbracha.blogspot.ca/2012/11/debug-mode-is-only-mode.html](http://gbracha.blogspot.ca/2012/11/debug-mode-is-only-mode.html)
