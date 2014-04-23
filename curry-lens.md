### Monadic Combinators for _Putback_ Style Bidirectional Programming

* get functions in general not injective: many possible corresponding
      put functions exist to form a well-behaved lens
* _PUTINJ_: `put s` is injective for any source `s`, i.e., $s' \in put
  s v \wedge s' \in put s v' \Rightarrow v = v'$
* _PUTTWICE_: $s' \in put s v \Rightarrow s' = put s' v$


## Validy Checking of _Putback_ Transformation in Bidirectional Programming

* requires putback functions to be affine and in treeless form, that
  is, each view variable is used at most once and no
  intermediate data structures are used in definitions
* this class of functions has similiarities to tree transducers
* assumes only total functions
* hybrid compositional approach, but focus on designing language to
  specify various primitive putback functions over algebraic data structures
* validity of putback transformations - A put is valid, if there
  exists a get such that GETPUT and PUTGET are satisfied
* Uniqueness of get - Given a put function, there exists at mose one
  get function that forms a well-behaved BX
* _PUTDETERMINATION_: $\forall s,s',v,v' . put s v = put s' v'
  \rightarrow v = v'$
* _PUTSTABILITY_: $\forall s . \exists v . put s v = s$
* validity = A put function is valif if and only if it satisfies the
  _PUTDETERMINATION_ and _PUTSTABILITY_ properties


### Spicey

* labeling bug: if a corresponding label is missing, a
  non-determinism error occurs
* high-potential library to include lenses for getter, setter and projection


## WUI

* `readQTerm (showQTerm ())` fails with parse error
* difference between `wJoinTuple` and `wPair`?

## Formlenses

* `lmap :: Lens a b -> f b -> f a`, where `f` is a `FormLens`
* use `WUILenses` instead of `WUI` in blog example of Spicey
