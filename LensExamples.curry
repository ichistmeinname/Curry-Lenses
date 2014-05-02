module LensExamples where

import Integer ( even, odd )
import Maybe ( fromJust )
import List ( nub )
import Float ((*.), (/.), (+.), (-.))
import Monadic

----- Height examples

putHeight1 :: Lens (Int,Int) Int
putHeight1 = keepFst

putHeight2 :: Lens (Int,Int) Int
putHeight2 = addFst (\p v ->
  maybe failed
        (uncurry (putSquare v))
        p)
 where
  putSquare v s1 s2 | s1 == s2  = v
                    | otherwise = failed

putHeight3 :: Int -> Lens (Int,Int) Int
putHeight3 i = addFst (\p v ->
  maybe failed
        (uncurry (putChange v))
        p)
 where
  putChange v s1 s2 | v == s2   = s1
                    | otherwise = i

----- Date examples

type Month   = Int
type Day     = Int
data Date = Date Month Day

monthLens :: Lens Date Month
monthLens = dateLens <.> keepSnd

dayLens :: Lens Date Day
dayLens = dateLens <.> keepFst

dateLens :: Lens Date (Month,Day)
dateLens = isoLens inn out
 where
  inn (m, d) = Date m d
  out (Date m d) = (m,d)

----- Temperature examples

-- type Temp = { fahrenheit :: Float }
data Temp = Temp Float

centigrade :: Lens Temp Float
centigrade = isoLens inn out
 where
  inn :: Float -> Temp
  inn celsius = Temp (cToF celsius)
  out :: Temp -> Float
  out (Temp temp) = fToC temp

cToF :: Float -> Float
cToF c = c *. 1.8 +. 32
fToC :: Float -> Float
fToC f = (f -. 32) *. (5/.9)

----- Time examples

-- type Time = { hour :: Int, min :: Int }
data Time = Time Int Int

inTime :: Lens Time Int
inTime = isoLens innT (\(Time hour min) -> hour * 60 + min)

mins :: Lens Time Int
mins = isoLens innT (\(Time _ min) -> min)

innT :: Int -> Time
innT m = Time (m `quot` 60) (m `mod` 60)

----- Person examples

data Person = Person Name City
type Name = String
type City = String

inPerson :: Lens Person (Name,City)
inPerson = isoLens inn out
 where
  inn (n,c)        = Person n c
  out (Person n c) = (n,c)

outPerson :: Lens (Name, City) Person
outPerson = isoLens out inn
 where
  inn (n,c)        = Person n c
  out (Person n c) = (n,c)

people  = [bastian, lennart, julia]
bastian = Person "Bastian" "Gaarden"
lennart = Person "Lennart" "Kronshagen"
julia   = Person "Julia"   "Schreventeich"
-- julia   = Person "Julia"   "Kiel"

nameOrCity :: Lens Person String
nameOrCity = name ? city

name :: Lens Person Name
name = inPerson <.> keepSnd

city :: Lens Person City
city = inPerson <.> keepFst

inout :: Lens Person Person
inout = inPerson <.> outPerson

peopleNames :: City -> Lens [Person] [Name]
peopleNames c = mapLens (inPerson <.> addSnd cityOf)
 where
  cityOf s _ = maybe c snd s

-----

-- embed an element in a list
embedAt :: Int -> Lens [a] a
embedAt i = if i == 0 then unhead
                      else untail <.> embedAt (i-1)

-- falsePut :: Lens Int Bool

----- Examples from "Bidirectionalization for Free"

halve :: [a] -> [a]
halve xs = take (length xs `div` 2) xs

putHalve :: [a] -> [a] -> [a]
putHalve xs xs' | length xs' == n = xs' ++ drop n xs
                | otherwise       = failed
 where
  n = length xs `div` 2

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
  | v == nub v && length v == length s' = map (fromJust . flip lookup (zip s' v)) s
  | otherwise = failed
 where
  s' = nub s

----- Examples from "Validity Check"

data Elem a = A a | B a

putAs [ ] [ ] = [ ]
putAs (ss@[])    (v:vs)   = A v : putAs ss vs
putAs (A _ : ss) (vs@[ ]) = putAs ss vs
putAs (A _ : ss) (v:vs)   = A v : putAs ss vs
putAs (B b : ss) vs       = B b : putAs ss vs