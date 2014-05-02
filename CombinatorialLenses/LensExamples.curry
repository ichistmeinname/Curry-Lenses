module LensExamples where

import Integer ( even, odd )
import Maybe ( fromJust )
import List ( nub )
import Float ((*.), (/.), (+.), (-.))
import Lens

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
type First = String
type Address = String

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
nameOrCity = nameLens ? cityLens

nameLens :: Lens Person First
nameLens = inPerson <.> keepSnd

addressLens :: Lens Person Address
addressLens = inPerson <.> keepFst

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

----- Examples from "Putback is the Essence of Bidirectional Programming

-- getFirst (x,_) = x

-- putFirst (_,y) z = (z,y)

-- putFirstCount (n,c) m | n==m      = (m,c)
--                       | otherwise = (m,c+1)

-- putFirstDiff (x,y) z = (z,z+y-x)

data PersonData = PersonData Name City

data Name = Hugo | Sebastian | Zhenjiang
data City = Kiel | Tokyo | Braga

-- city :: Lens PersonData City
-- city (PersonData n _) c = PersonData n c

-- name :: Lens PersonData Name
-- name (PersonData _ c) n = PersonData n c

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

-- peopleFrom :: City -> Lens [PersonData] [PersonData]
-- peopleFrom c source view =
--   let elsewhere = filter (not . isFrom c) source
--   in mergePeople elsewhere (map ensureCity view)
--  where
--   ensureCity q | isFrom c q = q

-- peopleFromTo :: City -> City -> Lens [PersonData] [PersonData]
-- peopleFromTo from to source view =
--   let moved = map move source
--   in peopleFrom from moved view
--  where
--   move p | get city p == from = put city p to
--          | otherwise          = p

----- Examples from "Validity Check"

-- data Elem a = A a | B a

-- putAs [ ] [ ] = [ ]
-- putAs (ss@[])    (v:vs)   = A v : putAs ss vs
-- putAs (A _ : ss) (vs@[ ]) = putAs ss vs
-- putAs (A _ : ss) (v:vs)   = A v : putAs ss vs
-- putAs (B b : ss) vs       = B b : putAs ss vs