module EasyCheck (

  -- test specification:
  Test, (==>), for, 

  test, is, isAlways, isEventually, prop, uniquely, always, eventually, 
  failing, successful, deterministic, (-=-), (#), (<~>), (~>), (<~),

  -- test annotations
  label, trivial, classify, collect, collectAs, 

  -- test functions
  easyCheck, easyCheck1, easyCheck2, easyCheck3, easyCheck4, easyCheck5,
  verboseCheck, verboseCheck1, verboseCheck2, verboseCheck3, verboseCheck4,
  verboseCheck5,

  valuesOf, Result(..), result,

  easyCheck', easyCheck1', easyCheck2', easyCheck3', easyCheck4', easyCheck5'

  ) where

import List       ( nub, group, intersperse, (\\) )
import Sort       ( leqList, leqString, mergeSort )
import Random     ( nextInt )
import SearchTree ( someSearchTree )

import SearchTreeTraversal

infix  4 `isSameSet`, `isSubsetOf`
infix  1 `is`, `isAlways`, `isEventually`, -=-, #, <~>, ~>, <~, `trivial`
infixr 0 ==>


data Test = Test Result [String] [String]

data Result = Undef | Ok | Falsified [String] | Ambigious [Bool] [String]

type Prop = [Test]

notest :: Test
notest = Test Undef [] []

result :: Test -> Result
result (Test r _ _) = r

setResult :: Result -> Test -> Test
setResult res (Test _ s a) = Test res a s

args, stamp :: Test -> [String]
args  (Test _ a _) = a
stamp (Test _ _ s) = s

updArgs, updStamp :: ([String] -> [String]) -> Test -> Test
updArgs  upd (Test r a s) = Test r (upd a) s
updStamp upd (Test r a s) = Test r a (upd s)

-- Test Specification

test :: a -> ([a] -> Bool) -> Prop
test x f = [setResult res notest]
 where
  xs  = valuesOf x
  res = case valuesOf (f xs) of
          [True]  -> Ok
          [False] -> Falsified (map show xs)
          bs      -> Ambigious bs (map show xs)

is, isAlways, isEventually :: a -> (a -> Bool) -> Prop
is x f = test x (\xs -> case xs of [y] -> f y; _ -> False)
isAlways x  = test x . all
isEventually x = test x . any

prop, uniquely, always, eventually :: Bool -> Prop
prop       = uniquely
uniquely   = (`is`id)
always     = (`isAlways`id)
eventually = (`isEventually`id)

failing, successful, deterministic :: _ -> Prop
failing x = test x null
successful x = test x (not . null)
deterministic x = x `is` const True

(-=-) :: a -> a -> Prop
x -=- y = (x,y) `is` uncurry (==)

(#) :: _ -> Int -> Prop
x # n = test x ((n==) . length . nub)

(<~>), (~>), (<~) :: a -> a -> Prop
x <~> y = test x (isSameSet (valuesOf y))
x  ~> y = test x (isSubsetOf (valuesOf y))
x <~  y = test x (`isSubsetOf` (valuesOf y))

isSameSet, isSubsetOf, subset :: [a] -> [a] -> Bool
xs `isSameSet` ys = xs' `subset` ys' && ys' `subset` xs'
 where xs' = nub xs; ys' = nub ys
xs `isSubsetOf` ys = nub xs `subset` ys
xs `subset` ys = null (xs\\ys)

(==>) :: Bool -> Prop -> Prop
True  ==> p = p
False ==> _ = [notest]

forAll :: (b -> Prop) -> a -> (a -> b) -> Prop
forAll c x f
  = diagonal [[ updArgs (show y:) t | t <- c (f y) ] | y <- valuesOf x ]

for :: a -> (a -> Prop) -> Prop
for = forAll id

-- Test Annotations

label :: String -> Prop -> Prop
label = map . updStamp . (:)

classify :: Bool -> String -> Prop -> Prop
classify True  name = label name
classify False _    = id

trivial :: Bool -> Prop -> Prop
trivial = (`classify`"trivial")

collect :: a -> Prop -> Prop
collect = label . show

collectAs :: String -> a -> Prop -> Prop
collectAs name = label . ((name++": ")++) . show

-- Test Functions

data Config = Config Int Int (Int -> [String] -> String)

maxTest, maxFail :: Config -> Int
maxTest (Config n _ _) = n
maxFail (Config _ n _) = n

every :: Config -> Int -> [String] -> String
every (Config _ _ f) = f

setEvery :: (Int -> [String] -> String) -> Config -> Config
setEvery f (Config n m _) = Config n m f

easy :: Config
easy = Config 1000 10000
        (\n _ -> let s = ' ':show (n+1) in s ++ [ chr 8 | _ <- s ])

verbose :: Config
verbose = setEvery (\n xs -> show n ++ ":\n" ++ unlines xs) easy

easyCheck, verboseCheck :: Prop -> IO ()
easyCheck    = check easy
verboseCheck = check verbose

suc :: (a -> Prop) -> (b -> a) -> Prop
suc n = forAll n unknown

easyCheck1 :: (_ -> Prop) -> IO ()
easyCheck1 = easyCheck . suc id

easyCheck2 :: (_ -> _ -> Prop) -> IO ()
easyCheck2 = easyCheck . suc (suc id)

easyCheck3 :: (_ -> _ -> _ -> Prop) -> IO ()
easyCheck3 = easyCheck . suc (suc (suc id))

easyCheck4 :: (_ -> _ -> _ -> _ -> Prop) -> IO ()
easyCheck4 = easyCheck . suc (suc (suc (suc id)))

easyCheck5 :: (_ -> _ -> _ -> _ -> _ -> Prop) -> IO ()
easyCheck5 = easyCheck . suc (suc (suc (suc (suc id))))

verboseCheck1 :: (_ -> Prop) -> IO ()
verboseCheck1 = verboseCheck . suc id

verboseCheck2 :: (_ -> _ -> Prop) -> IO ()
verboseCheck2 = verboseCheck . suc (suc id)

verboseCheck3 :: (_ -> _ -> _ -> Prop) -> IO ()
verboseCheck3 = verboseCheck . suc (suc (suc id))

verboseCheck4 :: (_ -> _ -> _ -> _ -> Prop) -> IO ()
verboseCheck4 = verboseCheck . suc (suc (suc (suc id)))

verboseCheck5 :: (_ -> _ -> _ -> _ -> _ -> Prop) -> IO ()
verboseCheck5 = verboseCheck . suc (suc (suc (suc (suc id))))


check :: Config -> Prop -> IO ()
check config p = tests config p 0 0 []

tests :: Config -> [Test] -> Int -> Int -> [[String]] -> IO ()
tests _ [] ntest _ stamps = done "Passed" ntest stamps
tests config (t:ts) ntest nfail stamps
  | ntest == maxTest config = done "OK, passed" ntest stamps
  | nfail == maxFail config = done "Arguments exhausted after" ntest stamps
  | otherwise = do
      putStr (every config ntest (args t))
      case result t of
        Undef -> tests config ts ntest (nfail+1) stamps
        Ok    -> tests config ts (ntest+1) nfail (stamp t:stamps)
        Falsified results -> putStr $
          "Falsified by " ++ nth (ntest+1) ++ " test" ++
          (if null (args t) then "." else ".\nArguments:") ++ "\n" ++
          unlines (args t) ++
          if null results then "no result\n"
           else "Results:\n" ++ unlines (nub results)
        Ambigious bs results -> putStr $
          "Ambigious property yields " ++ show bs ++ " for " ++ 
          nth (ntest+1) ++ " test" ++
          (if null (args t) then "." else ".\nArguments:") ++ "\n" ++
          unlines (args t) ++
          if null results then "no result\n"
           else "Results:\n" ++ unlines (nub results)

check' :: Config -> Prop -> Result
check' config p = tests' config p 0 0 []

tests' :: Config -> [Test] -> Int -> Int -> [[String]] -> Result
tests' config (t:ts) ntest nfail stamps
  | ntest == maxTest config = Ok
  | nfail == maxFail config = Falsified ["Arguments exhausted after " ++ show ntest ++ " test"]
  | otherwise = case result t of
                     Undef     -> tests' config ts ntest (nfail+1) stamps
                     Ok        -> tests' config ts (ntest+1) nfail stamps
                     res       -> res 

easyCheck' :: Prop -> Result
easyCheck' = check' easy

easyCheck1' :: (_ -> Prop) -> Result
easyCheck1' = easyCheck' . suc id

easyCheck2' :: (_ -> _ -> Prop) -> Result
easyCheck2' = easyCheck' . suc (suc id)

easyCheck3' :: (_ -> _ -> _ -> Prop) -> Result
easyCheck3' = easyCheck' . suc (suc (suc id))

easyCheck4' :: (_ -> _ -> _ -> _ -> Prop) -> Result
easyCheck4' = easyCheck' . suc (suc (suc (suc id)))

easyCheck5' :: (_ -> _ -> _ -> _ -> _ -> Prop) -> Result
easyCheck5' = easyCheck' . suc (suc (suc (suc (suc id))))

nth :: Int -> String
nth n = case n of 1 -> "first"; 2 -> "second"; 3 -> "third"; _ -> show n++ "th"

done :: String -> Int -> [[String]] -> IO ()
done mesg ntest stamps
  = putStr $ mesg ++ " " ++ show ntest ++ " test"
          ++ (if ntest >= 2 then "s" else "") ++ table
 where
  table = display
        . map entry
        . reverse
        . mergeSort (leqPair (<=) (leqList leqString))
        . map pairLength
        . group
        . mergeSort (leqList leqString)
        . filter (not . null)
        $ stamps

  display []         = ".\n"
  display [x]        = " - " ++ x ++ ".\n"
  display xs@(_:_:_) = ".\n" ++ unlines (map (++".") xs)

  pairLength xss@(xs:_) = (length xss,xs)

  entry (n,xs) = percentage n ntest ++ " " ++ concat (intersperse ", " xs)

  percentage n _ = let s = show n -- ((100*n)`div`m)
                    in replicate (5-length s) ' ' ++ s -- ++ "%"

-- Auxiliary Functions

leqPair :: (a -> a -> Bool) -> (b -> b -> Bool) -> ((a,b) -> (a,b) -> Bool)
leqPair leqa leqb (x1,y1) (x2,y2)
  | x1 == x2  = leqb y1 y2
  | otherwise = leqa x1 x2

valuesOf :: a -> [a]
valuesOf
  -- = depthDiag . someSearchTree . (id$##)
  -- = rndDepthDiag 0 . someSearchTree . (id$##)
  -- = levelDiag . someSearchTree . (id$##)
  -- = rndLevelDiag 0 . someSearchTree . (id$##)
   = rndLevelDiagFlat 5 0 . someSearchTree . (id$##) 
  -- = allValuesB . someSearchTree . (id$##)
