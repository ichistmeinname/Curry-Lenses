# Lenses for Curry

## Ideas for approaching this topic

* combinator DSL (3.-11.4.)
  * modify monadic approach [2] to use nondeterminism (3.-8.4.)
  * test examples from paper (9.4.)
  * test examples with nondeterminism (9.4.)
  * test examples from SPJ's presentation (10.4.)
  * test examples for web programming with Spicey (11.4.)

* definition of `put` and generating `get` [1] (14.4. - 23.4)
  * naive implementation + basic combinators (14.4.)
  * examples: is it natural to define well-behaved put-based lenses? (15.-17.4.)
    * test examples from paper (15.4.)
    * test examples from SPJ's presentation (16.4.)
    * test examples for web programming with Spicey (17.4.)
  
  * efficient implementation
  * check requirements for `put` (static analysis (approx.)) [4]

* definition of `get` and generating `put` + choosing _best_
  `get` function [3]

* practical examples: WUI [5, 6]

## Sources

[1] http://research.nii.ac.jp/~hu/pub/grace12_08.pdf

[2] http://www.cs.cornell.edu/~hpacheco/publications/pepm14.pdf

[3] http://www.cs.cornell.edu/~hpacheco/publications/curry-slides.pdf

[4] http://www.cs.cornell.edu/~hpacheco/publications/fm14.pdf

[5] http://www.informatik.uni-kiel.de/~mh/publications/papers/PPDP06.pdf

[6] http://homepages.inf.ed.ac.uk/slindley/papers/drformlens-draft-june2012.pdf
