--- Implements additional traversals on search trees.
---
--- @author Sebastian Fischer <sebf@informatik.uni-kiel.de>
--- @version August 2007
---
module SearchTreeTraversal (

  depthDiag, rndDepthDiag, levelDiag, rndLevelDiag, rndLevelDiagFlat,

  -- these should be standard library functions of List and Random modules:
  diagonal, shuffle

  ) where

import SearchTree
import Integer ( abs )
import Random

split = nextInt

--- diagonalized depth first search.
---
--- @param t search tree
--- @return enumeration of values in given search tree
---
depthDiag :: SearchTree a -> [a]
depthDiag t = [ x | Value x <- dfsDiag t ]

dfsDiag :: SearchTree a -> [SearchTree a]
-- dfsDiag Suspend      = []
dfsDiag (Fail _)     = []
dfsDiag t@(Value _)  = [t]
dfsDiag t@(Or t1 t2) = t : diagonal (map dfsDiag [t1,t2])


--- randomized variant of diagonalized depth first search.
---
--- @param t search tree
--- @return enumeration of values in given search tree
---
rndDepthDiag :: Int -> SearchTree a -> [a]
rndDepthDiag rnd t = [ x | Value x <- rndDfsDiag rnd t ]

rndDfsDiag :: Int -> SearchTree a -> [SearchTree a]
-- rndDfsDiag _   Suspend      = []
rndDfsDiag _   (Fail _)     = []
rndDfsDiag _   t@(Value _)  = [t]
rndDfsDiag rnd t@(Or t1 t2) =
  t : diagonal (zipWith rndDfsDiag rs (shuffle r [t1,t2]))
 where
  r:rs = split rnd


--- diagonalization of devels.
---
--- @param t search tree
--- @return enumeration of values in given search tree
---
levelDiag :: SearchTree a -> [a]
levelDiag t = [ x | Value x <- diagonal (levels [t]) ]

levels :: [SearchTree a] -> [[SearchTree a]]
levels ts | null ts   = []
          | otherwise = ts : levels [ u | Or u1 u2 <- ts, u <- [u1,u2] ]


--- randomized diagonalization of levels. 
---
--- @param t search tree
--- @return enumeration of values in given search tree
---
rndLevelDiag :: Int -> SearchTree a -> [a]
rndLevelDiag rnd t = [ x | Value x <- diagonal (rndLevels rnd [t]) ]

rndLevels :: Int -> [SearchTree a] -> [[SearchTree a]]
rndLevels rnd ts
  | null ts = []
  | otherwise 
    = ts : rndLevels r (concat (zipWith shuffle rs [ [u1,u2] | Or u1 u2 <- ts ]))
 where
  r:rs = split rnd

--- randomized diagonalization of levels with flatening. 

rndLevelDiagFlat :: Int -> Int -> SearchTree a -> [a]
rndLevelDiagFlat d rnd t = 
  concat $ transpose (zipWith rndLevelDiag rs (flatRep d [t]))
 where
  rs = split rnd

flat :: SearchTree a -> [SearchTree a]
flat t@(Value _) = [t]
flat (Fail _)    = [] -- pretend Fail ~ Or []
flat (Or t1 t2)  = [t1,t2]

flatRep :: Int -> [SearchTree a] -> [SearchTree a]
flatRep n ts 
  | n==0      = ts
  | otherwise = flatRep (n-1) (concatMap flat ts)

-- auxiliary functions

--- list diagonalization. 
--- Fairly merges (possibly infinite) list of (possibly infinite) lists.
---
--- @param ls lists of lists
--- @return fair enumeration of all elements of inner lists of given lists
---
diagonal :: [[a]] -> [a]
diagonal = concat . foldr diags []
 where
  diags []     ys = ys
  diags (x:xs) ys = [x] : merge xs ys
  -- diags xs@(_:_) ys = take 5 xs : merge (drop 5 xs) ys

  merge []       ys     = ys
  merge xs@(_:_) []     = map (:[]) xs
  merge (x:xs)   (y:ys) = (x:y) : merge xs ys


--- Computes a random permutation of the given list.
---
--- @param rnd random seed
--- @param l lists to shuffle
--- @return shuffled list
---
shuffle :: Int -> [a] -> [a]
shuffle rnd l = shuffleWithLen (nextInt rnd) (length l) l

shuffleWithLen :: [Int] -> Int -> [a] -> [a]
shuffleWithLen (r:rs) len xs
  | len == 0  = []
  | otherwise = z : shuffleWithLen rs (len-1) (ys++zs)
 where
  (ys,z:zs) = splitAt (abs r `mod` len) xs


transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : xss) = transpose xss
transpose ((x:xs) : xss)
  = (x : [h | (h:_) <- xss]) : transpose (xs : [t | (_:t) <- xss])
