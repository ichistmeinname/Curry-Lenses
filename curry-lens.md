### Monadic Combinators for _Putback_ Style Bidirectional Programming

* get functions in general not injective: many possible corresponding
      put functions exist to form a well-behaved lens
* _PUTINJ_: `put s` is injective for any source `s`, i.e., $s' \in put
  s v \wedge s' \in put s v' \Rightarrow v = v'$
* _PUTTWICE_: $s' \in put s v \Rightarrow s' = put s' v$


### Spicey

* labeling bug: if a corresponding label is missing, a
  non-deterministism error occurs
* high-potential library to include lenses for getter, setter and projection


## Formlenses

* `lmap :: Lens a b -> f b -> f a`
