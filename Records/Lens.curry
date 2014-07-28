module Lens where

type Lens a b = (a -> b, a -> b -> a)

get :: Lens a b -> a -> b
get = fst

set :: Lens a b -> a -> b -> a
set = snd