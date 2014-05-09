module LensExamples where

import Integer ( even, odd )
import Maybe ( fromJust )
import List ( nub, intercalate, last )
import Lens
import qualified BinaryList as BL
import Binary
import qualified Peano as P

----- Data type projections

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

-- invalid put!
falsePut :: Lens Int Bool
falsePut s _ = s

data Time = Time BinInt BinInt

time :: BinInt -> BinInt -> Time
time h m = putHours (putMins (Time Zero Zero) m) h

putHours :: Lens Time BinInt
putHours t@(Time _ m) h = Time h m

-- a smarter projection
putMins :: Lens Time BinInt
putMins t            Zero    = Time Zero Zero
putMins t@(Time _ _) (Pos m) = Time q r
 where
  (q,r) = m `quotRemNat'` (O (O (I (I (I IHi)))))


----- Examples from "Bidirectionalization for Free"

halve :: [a] -> [a]
halve xs = take (length xs `div` 2) xs

putHalve :: [a] -> [a] -> [a]
putHalve xs xs' | length xs' == n = xs' ++ drop n xs
                | otherwise        = failed
 where
  n  = length xs `div` 2

putHalveNat :: [a] -> [a] -> [a]
putHalveNat xs xs' | lengthN xs' == n = xs' ++ drop' n xs
 where
  n = div2 (lengthN xs)
  drop' :: Nat -> [a] -> [a]
  drop' IHi   []     = []
  drop' (I _) []     = []
  drop' (O _) []     = []
  drop' IHI   (_:xs) = xs
  drop' (I n) (_:xs) = drop' (pred (I n)) xs
  drop' (O n) (_:xs) = drop' (pred (O n)) xs

lengthN :: [a] -> Nat
lengthN [_]      = IHi
lengthN (_:y:ys) = succ (lengthN (y:ys))

putHalveBinaryList :: BL.BinaryList a -> BL.BinaryList a -> BL.BinaryList a
putHalveBinaryList xs xs' | BL.length xs' == n = xs' BL.++ BL.drop n xs
 where
  n = BL.length xs `divInteger` Pos (O IHi)

putHalvePeano :: [a] -> [a] -> [a]
putHalvePeano xs xs' | P.length xs' == n = xs' ++ P.drop n xs
 where
  n = P.length xs `P.div` (P.S (P.S P.Z))

data Tree a = Leaf a | Node (Tree a) (Tree a)

flatten :: Tree a -> [a]
flatten (Leaf e)     = [e]
flatten (Node t1 t2) = flatten t1 ++ flatten t2

putflatten :: Tree a -> [a] -> Tree a
putflatten s v = case go s v of
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


----- "data-base" examples

data UPerson = P Prof | S Student
data Prof = Prof String String
data Student = Student String String

putStudents :: Lens [UPerson] [Student]
putStudents []         []         = []
putStudents (S s : ps) (s' : sts) = S s' : putStudents ps sts
putStudents (P p : ps) sts        = P p : putStudents ps sts

testUPersons = [ P (Prof "Huch" "Computer Science")
               , S (Student "Dylus" "Computer Sciene")
               , S (Student "Cordes" "Chemistry")]

testStudents = [Student "Beck" "Linguistics", Student "Danilenko" "Mathematics"]


----- Lists

putHead :: [a] -> a -> [a]
putHead []     y = [y]
putHead (x:xs) y = y:xs

putTail :: [a] -> [a] -> [a]
putTail []     ys = ys
putTail (x:xs) ys = x:ys

putReverse :: [a] -> [a] -> [a]
putReverse [] []         = []
putReverse (x:xs) (y:ys) = putReverse xs ys ++ [y]

putLength :: Lens [a] Int
putLength [] n     | n <  0    = failed
                   | n == 0    = []
                   | n >  0    = failed
putLength (x:xs) n | n <  0    = failed
                   | n == 0    = []
                   | otherwise = x : putLength xs (n-1)

putZip :: Lens ([a],[b]) [(a,b)]
putZip _           []           = ([],[])
putZip (xs,ys)    ((v1,v2):vs)   = let (xs',ys') = putZip (xs,ys) vs
                                   in (v1:xs',v2:ys')

-- a more restrictive version of `putZip`, fails if the view list is
-- longer than the shortest list of the source tuple 
putZip' :: Lens ([a],[b]) [(a,b)]
putZip' _           []           = ([],[])
putZip' (x:xs,y:ys) ((v1,v2):vs) = let (xs',ys') = putZip' (xs,ys) vs
                                   in (v1:xs',v2:ys')

putAlt :: Lens [a] [a]
putAlt []       []     = []
putAlt [x]      []     = [x]
putAlt [x]      (z:zs) = [x,z]
putAlt (x:y:xs) (z:zs) = x:z: putAlt xs zs

putAppend :: Lens ([a],[a]) [a]
putAppend ([],_)     zs     = ([],zs)
putAppend (x:xs,ys) (z:zs) = let (xs',ys') = putAppend (xs,ys) zs
                             in (z:xs',ys')


putLookup :: a -> Lens [(a,b)] (Maybe b)
putLookup _ []       Nothing                = []
putLookup k (kv:kvs) Nothing  | k /= fst kv = kv : putLookup k kvs Nothing
                              | otherwise   = putLookup k kvs Nothing
putLookup k []       (Just v)               = [(k,v)]
putLookup k (kv:kvs) (Just v) | k == fst kv = (k,v):kvs
                              | otherwise   = kv : putLookup k kvs (Just v)

----- Trees

putSumTree :: Lens (Tree Int) Int
putSumTree (Leaf _)   n = Leaf n
putSumTree (Node l r) n = Node (putSumTree l n')
                               (putSumTree r (n-n'))
 where
  n' = n `div` 2

binIntTree = Node (Node (Leaf (Pos IHi))
                        (Leaf (Pos (O IHi))))
                  (Leaf (Pos (I IHi)))

putSumTreeShift :: Lens (Tree BinInt) BinInt
putSumTreeShift (Leaf _)   n         = Leaf n
putSumTreeShift (Node l r) Zero      =
  Node (putSumTreeShift l Zero)
       (putSumTreeShift r Zero)
putSumTreeShift (Node l r) m@(Pos _) =
  Node (putSumTreeShift l n')
       (putSumTreeShift r (m -# n'))
 where
  n' = shiftLeft m

putSumTreeDivNat :: Lens (Tree BinInt) BinInt
putSumTreeDivNat (Leaf _)   n         = Leaf n
putSumTreeDivNat (Node l r) Zero      =
  Node (putSumTreeDivNat l Zero)
       (putSumTreeDivNat r Zero)
putSumTreeDivNat (Node l r) m@(Pos n) =
  Node (putSumTreeDivNat l n')
       (putSumTreeDivNat r (m -# n'))
 where
  n' = divNat n (O IHi)

putAt :: Int -> Lens [a] a
putAt n []     v | n < 0     = failed
                 | n == 0    = [v]
                 | otherwise = []
putAt n (x:xs) v | n < 0     = failed
                 | n == 0    = v:xs
                 | otherwise = x : putAt (n-1) xs v

putLines :: Lens String [String]
putLines x xs | any ('\n' `elem`) xs = failed
              | otherwise            = intercalate "\n" xs

putDiv :: Int -> Lens Int Int
putDiv x 0 z = failed
putDiv x y z | r > 0 && r < y && x == y*z + r = y where r free

putInit :: Lens [a] [a]
putInit []       []     = failed
putInit [x]      []     = [x]
putInit (x:y:xs) (x:ys) = x : putInit (y:xs) ys