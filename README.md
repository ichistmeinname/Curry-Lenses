# Lenses for Curry

## Ideas for approaching this topic

* combinator DSL - (3.-11.4.)
    * modify monadic approach [2] to use nondeterminism - (3.-8.4.)
    * test examples from paper - (9.4.)
    * test examples with nondeterminism - (9.4.)
    * test examples from SPJ's presentation - (10.4.)
    * figure out how to use Spicey [7] - (11.4.)
    * test examples for web programming with Spicey - (11.4.)

* formlenses - (14.-23.4.)
    * read [6] - (14.-16.4.)
    * adapt formlens idea for WUI/Spicey framework - (16.-17.4.)
    * use formlenses for blog example of Spicey framework (22.-23.4)
  
* definition of `put` and generating corresponding `get` [1]  - (24. - 30.4)
    * naive implementation + basic combinators - (24.4.)
    * examples: is it natural to define well-behaved put-based lenses? -
    (25.-26.4.)
        * test examples from paper - (25.4.)
        * test examples from SPJ's presentation - (25.4.)
        * test examples for web programming with Spicey - (26.4.)
  
    * efficient implementation
    * check requirements for `put` (static analysis (approx.)) [4]

* definition of `get` and generating `put` + choosing _best_
  `get` function [3]

* practical examples: WUI [5, 6]

## Sources

[1] http://research.nii.ac.jp/~hu/pub/grace12_08.pdf - 
"Putback" is the Essence of Bidirectional Programming

[2] http://www.cs.cornell.edu/~hpacheco/publications/pepm14.pdf - Monadic Combinators for "Putback" Style Bidirectional Programming

[3] http://www.cs.cornell.edu/~hpacheco/publications/curry-slides.pdf - Functional Logic Semantic Bidirectionalization for Free!

[4] http://www.cs.cornell.edu/~hpacheco/publications/fm14.pdf - Validity Checking of Putback Transformations in Bidirectional Programming

[5] http://www.informatik.uni-kiel.de/~mh/publications/papers/PPDP06.pdf - Type-Oriented Construction of Web User Interfaces

[6] http://homepages.inf.ed.ac.uk/jcheney/publications/drafts/bx13-workshop-final.pdf -
Lenses for Web Data 

[7] http://www.informatik.uni-kiel.de/~mh/papers/TPLP14.pdf - An ER-based Framework for Declarative Web Programming
