import Monadic
import LensExamples

-- yields y for xs = y:ys
unheadGet :: [a] -> Maybe a
unheadGet xs = getM unhead xs

-- yields (42:ys) for xs = y:ys
unheadPut :: [Int] -> [Int]
unheadPut xs = put' unhead (Just xs ? Nothing) 42

-- yields ys for xs = y:ys
untailGet :: [a] -> Maybe [a]
untailGet xs = getM untail xs

-- yields (y, 0, 42, 1337) for xs = y : ys
untailPut :: [Int] -> [Int]
untailPut xs = put' untail (Just xs ? Nothing) [0,42,1337]

-- yields (y,ys) for xs = y:ys
consGet :: [a] -> Maybe (a,[a])
consGet xs = getM cons xs

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
injRGet :: Either a b -> Maybe b
injRGet eV = getM injR eV

-- yields (Right v)
injRPut :: Either a b -> b -> Either a b
injRPut eV v = put' injR (Just eV) v
injRPut _ v = put' injR Nothing v

-- embed an element in a list
embedAt :: Int -> Lens [a] a
embedAt i = if i == 0 then unhead'
                      else untail' <.> embedAt (i-1)

embedAtGet :: Int -> [a] -> Maybe a
embedAtGet i xs = getM (embedAt i) xs

embedAtPut :: Int -> [a] -> a -> [a]
embedAtPut i xs x = put' (embedAt i) (Just xs) x
embedAtPut i _  x = put' (embedAt i) Nothing x

-- fails
fstOrSndGet :: (a,a) -> Either a a
fstOrSndGet pair = get' (keepFst \/ keepSnd) pair

ifSGet :: (Int,Int) -> Int
ifSGet s = get' (ifSThenElse (\(a,b) -> a > 3) keepFst keepSnd) s

ifSPut :: (Int,Int) -> Int -> (Int,Int)
ifSPut s v = put' (ifSThenElse (\(a,b) -> a > 3) keepFst keepSnd) (Just s) v
ifSPut _ v = put' (ifSThenElse (\(a,b) -> a > 3) keepFst keepSnd) Nothing v

ifGet :: (Maybe (Either v v) -> v -> Bool) -> v -> v
ifGet p s = get' (ifThenElse p idLens botLens) s

ifPut p s v = put' (ifThenElse p botLens idLens) (Just s) v

predicate (Just (Left  v)) v' = v > v'
predicate (Just (Right v)) v' = v > v'
predicate Nothing          v' = True

----- LensExamples.curry

centigradeGet :: Float -> Float
centigradeGet temp = get' centigrade (Temp temp)

centigradePut :: Float -> Float -> Temp
centigradePut temp celsius = put' centigrade (Just (Temp temp)) celsius
centigradePut _    celsius = put' centigrade Nothing celsius

minsGet :: Int -> Int -> Int
minsGet h m = get' mins (Time h m)
minsGet h m = get' inTime (Time h m)

minsPut :: Int -> Int -> Int -> Time
minsPut h m newM = put' mins (Just (Time h m)) newM
minsPut h m newM = put' inTime (Just (Time h m)) newM
minsPut h m newM = put' mins Nothing newM
minsPut h m newM = put' inTime Nothing newM