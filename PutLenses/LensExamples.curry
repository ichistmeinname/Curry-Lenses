module LensExamples where

import Integer ( even, odd )
import Maybe ( fromJust )
import List ( nub )
import Lens
import qualified BinaryList as BL
import Binary
import qualified Peano as P

type Month   = Int
type Day     = Int
type First = String
type Address = String

data Date = Date Month Day
data Person = Person First Address

dateLens :: Lens Date (Month,Day)
dateLens _ (m,d) = Date m d

monthLens :: Lens Date Month
monthLens (Date _ d) m = Date m d

dayLens :: Lens Date Day
dayLens (Date m _) d = Date m d

addressLens :: Lens Person Address
addressLens (Person n _) a = Person n a

nameLens :: Lens Person First
nameLens (Person _ a) n = Person n a

falsePut :: Lens Int Bool
falsePut s _ = s


----- Examples from "Bidirectionalization for Free"

halve :: [a] -> [a]
halve xs = take (length xs `div` 2) xs

putHalve :: [a] -> [a] -> [a]
putHalve xs xs' | length xs' == n = xs' ++ drop n xs
                | otherwise        = failed
 where
  -- n' = intToPeano n
  n  = length xs `div` 2

putHalveBinaryList :: BL.BinaryList a -> BL.BinaryList a -> BL.BinaryList a
putHalveBinaryList xs xs' | BL.length xs' == n = xs' BL.++ BL.drop n xs
 where
  n = BL.length xs `divInteger` Pos (O IHi)

test :: [a] -> Bool
test xs' | length xs' == 1 = True
         | otherwise        = False

data Tree a = Leaf a | Node (Tree a) (Tree a)

flatten :: Tree a -> [a]
flatten (Leaf e)     = [e]
flatten (Node t1 t2) = flatten t1 ++ flatten t2

putFlatten :: Tree a -> [a] -> Tree a
putFlatten s v = case go s v of
                      (t,[]) -> t
                      _      -> failed
 where
  go (Leaf _)     (b:bs) = (Leaf b, bs)
  go (Node s1 s2) bs     = (Node t1 t2, ds)
   where (t1,cs) = go s1 bs
         (t2,ds) = go s2 cs

rmdups :: [a] -> [a]
rmdups = nub

putRmdups :: [a] -> [a] -> [a]
putRmdups s v
  | length v == length s' && v == nub v = map (fromJust . flip lookup (zip s' v)) s
  | otherwise = failed
 where
  s' = nub s

putRmdups' :: [a] -> [a] -> [a]
putRmdups' s v
  | nub v == v && P.length v == P.length s' = map (fromJust . flip lookup (zip s' v)) s
  | otherwise = failed
 where
  s' = nub s

----- Examples from "Putback is the essence of bidirecitional programming"

getFirst (x,_) = x

putFirst (_,y) z = (z,y)

putFirstCount (n,c) m | n==m      = (m,c)
                      | otherwise = (m,c+1)

putFirstDiff (x,y) z = (z,z+y-x)

data PersonData = PersonData Name City

data Name = Hugo | Sebastian | Zhenjiang
data City = Kiel | Tokyo | Braga

city :: Lens PersonData City
city (PersonData n _) c = PersonData n c

name :: Lens PersonData Name
name (PersonData _ c) n = PersonData n c

people = [hugo, sebastian, zhenjiang ]
hugo = PersonData Hugo Braga
sebastian = PersonData Sebastian Tokyo
zhenjiang = PersonData Zhenjiang Tokyo

isFrom :: City -> PersonData -> Bool
isFrom c p = c == get' city p

mergePeople :: [PersonData] -> [PersonData] -> [PersonData]
mergePeople old new = merge (sorted old) (sorted new)
 where
  merge [ ] ps                = ps
  merge (p : ps) [ ]          = p : ps
  merge (p : ps) (q : qs)
   | get name p < get' name q  = p : merge ps (q : qs)
   | get name p == get' name q = q : merge ps qs
   | get name p > get' name q  = q : merge (p : ps) qs
  sorted [ ]      = [ ]
  sorted (p : ps) = ascending p ps
  ascending p []               = [p]
  ascending p (q : qs)
    |  get name p < get' name q = p : ascending q qs

peopleFrom :: City -> Lens [PersonData] [PersonData]
peopleFrom c source view =
  let elsewhere = filter (not . isFrom c) source
  in mergePeople elsewhere (map ensureCity view)
 where
  ensureCity q | isFrom c q = q

peopleFromTo :: City -> City -> Lens [PersonData] [PersonData]
peopleFromTo from to source view =
  let moved = map move source
  in peopleFrom from moved view
 where
  move p | get city p == from = put city p to
         | otherwise          = p

----- Examples from "Validity Check"
data Elem a = A a | B a

putAs [ ] [ ] = [ ]
putAs (ss@[])    (v:vs)   = A v : putAs ss vs
putAs (A _ : ss) (vs@[ ]) = putAs ss vs
putAs (A _ : ss) (v:vs)   = A v : putAs ss vs
putAs (B b : ss) vs       = B b : putAs ss vs