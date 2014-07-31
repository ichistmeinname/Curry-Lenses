module Lens where

infixl 1 .&
infixr 4 .>, .=

type Lens a b = (a -> b, a -> b -> a)

get :: Lens a b -> a -> b
get = fst

put :: Lens a b -> a -> b -> a
put = snd

(<.>) :: Lens a b -> Lens b c -> Lens a c
(<.>) lAB lBC = (get lBC . get lAB, \a -> put lAB a . put lBC (get lAB a))

(.>) :: a -> Lens a b -> b
source .> lens = get lens source

(.=) :: Lens a b -> b -> a -> a
(.=) lens view source = put lens source view

(.&) :: a -> (a -> b) -> b
v .& f = f v