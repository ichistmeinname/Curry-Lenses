# Lenses for Curry

## Ideas for approaching this topic

    * definition of `put` and generating`get` [1]
        * efficient implementation
        * check requirements for `put` (static analysis (approx.)) [4]

    * definition of `get` and generating `put` + choosing _best_
      `get` function [3]

    * combinator DSL
        * modify monadic approach [2] to use nondeterminism

    * practical examples: WUI [5, 6]

[1] http://research.nii.ac.jp/~hu/pub/grace12_08.pdf

[2] http://www.cs.cornell.edu/~hpacheco/publications/pepm14.pdf

[3] http://www.cs.cornell.edu/~hpacheco/publications/curry-slides.pdf

[4] http://www.cs.cornell.edu/~hpacheco/publications/fm14.pdf

[5] http://www.informatik.uni-kiel.de/~mh/publications/papers/PPDP06.pdf

[6] http://homepages.inf.ed.ac.uk/slindley/papers/drformlens-draft-june2012.pdf