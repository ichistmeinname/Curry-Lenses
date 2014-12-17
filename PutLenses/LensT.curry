{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module LensT (
    put, get
  , mapLens, (<.>)
  , Lens
  )
where

type Lens source view = source -> view -> source

put :: Lens source view -> source -> view -> source
put lens s v = lens s v

get :: Eq source => Lens source view -> source -> view
get lens s | put lens s v == s = v
 where v free

(<.>) :: (Eq a, Eq b) => Lens a b -> Lens b c -> Lens a c
(<.>) l1 l2 sA vC = put l2 (get l1 sA) vC `bind` put l1 sA

bind :: a -> (a -> b) -> b
bind valA fAB = fAB valA

mapLens :: Lens a b -> Lens [a] [b]
mapLens lensF (x:xs) (y:ys) = lensF x y : mapLens lensF xs ys
mapLens lensF []     []     = []

-- mapLens lensF xs ys = zipWith