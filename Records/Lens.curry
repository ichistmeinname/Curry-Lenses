module Lens where

type Lens a b = (a -> b, a -> b -> a)

get :: Lens a b -> a -> b
get = fst

put :: Lens a b -> a -> b -> a
put = snd

(<.>) :: Lens a b -> Lens b c -> Lens a c
(<.>) lAB lBC = (get lBC . get lAB, \a -> put lAB a . put lBC (get lAB a))