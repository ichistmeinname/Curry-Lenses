module LensExamples where

import Integer ( even, odd )
import Maybe ( fromJust )
import List ( nub )
import PutLenses

type Month   = Int
type Day     = Int
type Key     = Int
type First   = String
type Last    = String
type Address = String

data Date = Date Month Day
data Person = Person Key First Last Address

dateLens :: Lens Date (Month,Day)
dateLens _ (m,d) = Date m d

monthLens :: Lens Date Month
monthLens (Date _ d) m = Date m d

dayLens :: Lens Date Day
dayLens (Date m _) d = Date m d

-- dateLens :: Lens Date (Int,Int)
-- dateLens = isoLens inn out
--  where
--   inn (m, d) = Date m d
--   out (Date m d) = (m,d)

addressLens :: Lens Person Address
addressLens (Person k f l _) address = Person k f l address

-- addressLens :: Lens Person String
-- addressLens = isoLens inn out <.> keepFst
--  where
--   inn ((key, first, last), address)   = Person key first last address
--   out (Person key first last address) = ((key, first, last), address)

-- embed an element in a list
-- embedAt :: Int -> Lens [a] a
-- embedAt i = if i == 0 then unhead
--                       else untail <.> embedAt (i-1)

falsePut :: Lens Int Bool
falsePut s _ = s


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
---------------- Examples from Bidirectionalization for Free -------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

halve :: [a] -> [a]
halve xs = take (length xs `div` 2) xs

putHalve :: [a] -> [a] -> [a]
putHalve xs xs' | length xs' == n = xs' ++ drop n xs
                | otherwise       = failed
 where
  n = length xs `div` 2

-- (Rule [1,2] (Let [(3,Comb FuncCall ("Prelude","div") [Comb FuncCall ("Prelude","length") [Var 1],Lit (Intc  2)])] (Case  Rigid (Comb FuncCall ("Prelude","==") [Comb FuncCall ("Prelude","length") [Var 2],Var 3]) [Branch (Pattern ("Prelude","True") []) (Comb FuncCall ("Prelude","++") [Var 2,Comb FuncCall ("Prelude","drop") [Var 3,Var 1]]),Branch (Pattern ("Prelude","False") []) (Case  Rigid (Comb FuncCall ("Prelude","otherwise") []) [Branch (Pattern ("Prelude","True") []) (Comb FuncCall ("Prelude","failed") []),Branch (Pattern ("Prelude","False") []) (Comb FuncCall ("Prelude","failed") [])])])))


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

-- PutGet: put s (get s) = s
-- GetPut: get (put s v) = v
-- PutDetermination: forall s,s',v,v'. put s v = put s' v' => v = v'
-- PutStability: forall s. exists v. put s v = s
putRmdups :: [a] -> [a] -> [a]
putRmdups s v
  | v == nub v && length v == length s' = map (fromJust . flip lookup (zip s' v)) s
  | otherwise = failed
 where
  s' = nub s


--------------------------------------------------------------------------------

evens :: [Int] -> [Int]
evens xs = filter even xs

putEvens :: [Int] -> [Int] -> [Int]
putEvens (x:xs) ys | any odd ys                  = failed
                   | even x && x `elem` ys       = x : putEvens xs ys
                   | even x && not (x `elem` ys) = putEvens xs ys
                   | otherwise                   = x : putEvens xs ys
putEvens [] ys                                   = []

--------------------------------
data Elem a = A a | B a

putAs [ ] [ ] = [ ]
putAs (ss@[])    (v:vs)   = A v : putAs ss vs
putAs (A _ : ss) (vs@[ ]) = putAs ss vs
putAs (A _ : ss) (v:vs)   = A v : putAs ss vs
putAs (B b : ss) vs       = B b : putAs ss vs