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
  
* definition of `put` and generating corresponding `get` [1]  - (24. - 23.05)
    * naive implementation + basic combinators - (24.4.)
    * examples: is it natural to define well-behaved put-based lenses? -
    (25.4.)
        * test examples of Fischer's et al paper - (25.4.)
        * test examples of SPJ's presentation - (25.4.)
        * test examples for web programming with Spicey - (25.4.)
        * test examples of Voiglaender's paper [8] (30.4)
          * examples caused non-terminating search for `get`, see
            notes in `curry-lens.md` (1.5., 5.5)
        * even more examples (6.-8.5.,14.-16.5)
        * implementation of better `quotRemNat` in `Binary.curry`
            (6.5.)

    * efficient implementation
    * check requirements for `put` (static analysis (approx.)) [4]
      (28.-29.04), see notes in `curry-lens.md`
        * study [4] (28.-29.04)
        * try to implement transformation to get inverse mapping
          (`rPut`) (28.-29.04) -> fiddly work, go back to test
          examples in order to see how invalid lenses look like and to
          search for `get`-definitions that cannot be generated with
          the built-in search mechanism

* Test framework (12.5;19.5-21.5)
    * reanimate "Easy Check" [9] (12.5)
    * implement tests for _GetPut_, _PutGet_ and _PutPut_ law (19.5.)
    * generate test environment for all lens definitions within a
      module (20.5.)
    * run tests for all examples (21.5.)
    * implement tests for _PutDet_, _PutStab_ (21.5.)

* definition of `get` and generating `put` + choosing _best_
  `get` function [3]

* practical examples
    * WUI [5, 6] (16.-17.4.)
    * simple parser/printer (21.-23.5.)
    * regular expressions with strings (21.-23.5.)
    * XML (21.-23.5.)


* add .bib file (12.5.)

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

[8]
http://www.iai.uni-bonn.de/~jv/papers/BidirectionalizationForFree.pdf -
Bidirectionalization for Free!

[9]
http://www-ps.informatik.uni-kiel.de/~sebf/data/pub/flops08.pdf
EasyCheck - Test Data for Free
