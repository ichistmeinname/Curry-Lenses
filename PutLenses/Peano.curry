module Peano where

import qualified Integer as I ( even, odd )

data Peano = Z | S Peano

peanoToInt :: Peano -> Int
peanoToInt Z     = 0
peanoToInt (S p) = 1 + peanoToInt p

intToPeano :: Int -> Peano
intToPeano val = case val of
  0    -> Z
  val' -> if val' < 0 then failed
                      else S (intToPeano (val' -1))

even :: Peano -> Bool
even p = I.even $ peanoToInt p

odd :: Peano -> Bool
odd p = not $ even p

length :: [a] -> Peano
length = foldr (const S) Z

drop :: Peano -> [a] -> [a]
drop Z     xs     = xs
drop (S n) (x:xs) = drop n xs
drop (S n) []     = []

data Neg = N

sub :: Peano -> Peano -> Either Neg Peano
sub n Z         = Right n
sub Z (S n)     = Left N
sub (S n) (S m) = sub n m

div :: Peano -> Peano -> Peano
div n Z     = error "div: division by zero"
div Z (S n) = Z
div n@(S _) m@(S _) = fst $ divMod n m
 where
  divMod x y = case sub x y of
                    Left _ -> (Z, y)
                    Right m' -> let (q,r) = divMod m' y
                                in (S q, r)