{-------------------------------------------------------------------------------
-- Inspired by definitions from
-- "'Putback' is the Essence of Bidirectional Programming"
-- http://research.nii.ac.jp/~hu/pub/grace12_08.pdf
-------------------------------------------------------------------------------}

module Lens where

import Maybe (isJust)
import SetFunctions (set2,set3,isEmpty,select,sortValuesBy)

fmap :: (a -> b) -> Maybe a -> Maybe b
fmap f (Just v) = Just (f v)
fmap _ Nothing  = Nothing

type Lens s v = s -> v -> s

put :: Lens s v -> s -> v -> s
put lens s v = lens s v

put' :: Lens s v -> Maybe s -> v -> s
put' lens (Just s) v = lens s v
put' _    Nothing  _ = error "put': no value"

putM :: Lens s v -> s -> v -> Maybe s
putM lens s v | isEmpty (set2 lens s v) = Nothing
              | otherwise               = Just (lens s v)

get :: Lens s v -> s -> v
get lens s | put lens s v == s = v
 where v free

get' :: Lens s v -> s -> v
get' lens s | isEmpty moreSolutions = selectedS
            | otherwise = error "invalid lens: more than one result was found"
 where
  (selectedS, moreSolutions) = select $ set2 get lens s

getM :: Lens s v -> s -> Maybe v
getM lens s | isEmpty (set2 get lens s) = Nothing
            | otherwise                 = Just (get' lens s)

getND :: Lens s v -> s -> [v]
getND lens s = sortValuesBy (\_ _ -> True) $ set2 get lens s

putND :: Lens s v -> s -> v -> [s]
putND lens s v = sortValuesBy (\_ _ -> True) $ set3 put lens s v

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

remFst :: (v -> v1) -> Lens v (v1,v)
remFst f val1 (val',val2) | f val2 == val' = val1

remSnd :: (v -> v1) -> Lens v (v,v1)
remSnd f val1 (val2,val') | f val2 == val' = val1

keepFst :: Lens (s1,v) v
keepFst (s,_) v = (s,v)

keepSnd :: Lens (v,s1) v
keepSnd (_,s) v = (v,s)

addSnd :: ((v,s1) -> v -> s1) -> Lens (v,s1) v
addSnd f s@(v,_) v' = (v',f s v) 

(\/) :: Lens s v1 -> Lens s v2 -> Lens s (Either v1 v2)
(l1 \/ l2) s (Left  v1) = disjoint l1 l2 (put l1 s v1)
(l1 \/ l2) s (Right v2) = disjoint l2 l1 (put l2 s v2)

(<.>) :: Lens a b -> Lens b c -> Lens a c
(<.>) l1 l2 sA vC = put l2 (get' l1 sA) vC `bind` put l1 sA

bind :: a -> (a -> b) -> b
bind x f = f x

idLens :: Lens a a
idLens s _ = s

botLens :: Lens a a
botLens _ _ = error "_|_"

mapLens :: Lens a b -> Lens [a] [b]
mapLens _ []     []     = []
mapLens lensF (x:xs) (y:ys) = lensF x y : mapLens lensF xs ys

mapLens' :: Lens a b -> Lens [a] [b]
mapLens' _ []     []     = []
mapLens' _ []     _      = []
mapLens' _ _      []     = []
mapLens' lensF (x:xs) (y:ys) = lensF x y : mapLens' lensF xs ys

unhead :: Lens [a] a
unhead (_:xs) x' = (x':xs)
unhead []     x' = [x']

untail :: Lens [a] [a]
untail (y:_) ys = (y:ys)
untail []    ys = ys

-- unhead' :: Lens [a] a
-- unhead' = cons <.> keepSndOr (\v -> [])

-- untail' :: Lens [a] [a]
-- untail' = cons <.> keepFstOr (\ (x:xs) -> x)

cons :: Lens [a] (a,[a])
cons _ (x',xs') = x':xs'

injL :: Lens (Either a b) a
injL _ v' = Left v'

injR :: Lens (Either a b) b
injR _ v' = Right v'

fstOrSnd :: Lens (a,a) (Either a a)
fstOrSnd (_,y) (Left  x') = (x',y)
fstOrSnd (x,_) (Right y') = (x,y')

ifSThenElse :: (a -> Bool) -> Lens a b -> Lens a b -> Lens a b
ifSThenElse p l1 l2 s v | p s       = l1 s v
                        | otherwise = l2 s v

-- ifThenElse :: (Either v v -> v -> Bool)
--            -> Lens s v
--            -> Lens s v
--            -> Lens s v
-- ifThenElse p l1 l2 s v | p

-- ifGet :: (Maybe (Either v v) -> v -> Bool) -> v -> v
-- ifGet p s = get' (ifThenElse p idLens botLens) s

-- ifPut p s v = put' (ifThenElse p botLens idLens) (Just s) v

disjoint :: Lens a b -> Lens a c -> a -> a
disjoint l1 l2 s
  | dom l1 s && not (dom l2 s) = s
  | otherwise                  = error "disjoint failed"

dom :: Lens a b -> a -> Bool
dom l s = isJust (getM l s)