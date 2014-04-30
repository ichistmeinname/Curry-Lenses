import PutLenses
import LensExamples

-- yields y for xs = y:ys
unheadGet :: [a] -> a
unheadGet xs = get' unhead xs

-- yields (42:ys) for xs = y:ys
unheadPut :: [Int] -> [Int]
unheadPut xs = put' unhead (Just xs ? Nothing) 42

-- yields ys for xs = y:ys
untailGet :: [a] -> [a]
untailGet xs = get' untail xs

-- yields (y, 0, 42, 1337) for xs = y : ys
untailPut :: [Int] -> [Int]
untailPut xs = put' untail (Just xs ? Nothing) [0,42,1337]

-- yields (y,ys) for xs = y:ys
consGet :: [a] -> (a,[a])
consGet xs = get' cons xs

-- yields [5,6,7,8]
consPut :: [Int] -> [Int]
consPut xs = put' cons (Just xs ? Nothing) (5,[6,7,8])

-- yields (Just x) for eV = Left x, Nothing otherwise
injLGet :: Either a b -> Maybe a
injLGet eV = getM injL eV

-- yields (Left v)
injLPut :: Either a b -> a -> Either a b
injLPut eV v = put' injL (Just eV) v
injLPut _ v = put' injL Nothing v

-- yields (Just x) for eV = Right x, Nothing otherwise
injRGet :: Either a b -> b
injRGet eV = get' injR eV

-- yields (Right v)
injRPut :: Either a b -> b -> Either a b
injRPut eV v = put' injR (Just eV) v
injRPut _ v = put' injR Nothing v

-- embed an element in a list
-- embedAt :: Int -> Lens [a] a
-- embedAt i = if i == 0 then unhead'
--                       else untail' <.> embedAt (i-1)

-- embedAtGet :: Int -> [a] -> a
-- embedAtGet i xs = get' (embedAt i) xs

-- embedAtPut :: Int -> [a] -> a -> [a]
-- embedAtPut i xs x = put' (embedAt i) (Just xs) x
-- embedAtPut i _  x = put' (embedAt i) Nothing x

-- fails
fstOrSndGet :: (a,a) -> Either a a
fstOrSndGet pair = get' (keepFst \/ keepSnd) pair

ifSGet :: (Int,Int) -> Int
ifSGet s = get' (ifSThenElse (\(a,_) -> a > 3) keepFst keepSnd) s

ifSPut :: (Int,Int) -> Int -> (Int,Int)
ifSPut s v = put' (ifSThenElse (\(a,_) -> a > 3) keepFst keepSnd) (Just s) v
ifSPut _ v = put' (ifSThenElse (\(a,_) -> a > 3) keepFst keepSnd) Nothing v

-- ifGet :: (Maybe (Either v v) -> v -> Bool) -> v -> v
-- ifGet p s = get' (ifThenElse p idLens botLens) s

-- ifPut p s v = put' (ifThenElse p botLens idLens) (Just s) v

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
---------------- Examples from Bidirectionalization for Free -------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

halveTest xs     = get' putHalve xs     == halve xs
flattenTest tree = get' putFlatten tree == flatten tree
rmdupsTest xs    = get' putRmdups xs    == rmdups xs 