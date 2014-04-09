module Monadic where

import Maybe (isJust, fromJust)
import SetFunctions
import Unsafe (trace)

undefined = error "bottom"

fmap :: (a -> b) -> Maybe a -> Maybe b
fmap _ Nothing  = Nothing
fmap f (Just v) = Just (f v)

type Lens s v = { put :: Maybe s -> v -> s
                , get :: s -> Maybe v
                }

put' :: Lens s v -> Maybe s -> v -> s
put' l s v = (l :> put) s v

get' :: Lens s v -> s -> v
get' l s = case (l :> get) s of
                Just v  -> v
                Nothing -> error "get': value is `Nothing`" 

getM :: Lens s v -> s -> Maybe v
getM l s = (l :> get) s

-- effect :: (Maybe s -> v -> n -> m) -> Lens n v -> Lens m v
effect f g s v' = f s v' (g s v')

bind :: a -> (a -> b) -> b
bind val f = f val

(<.>) :: Lens s v -> Lens v w -> Lens s w
(<.>) l1 l2 = { put := putNew
              , get := getNew
              }
 where
  putNew ms@(Just s) w = put' l2 (getM l1 s) w `bind` put' l1 ms   
  putNew Nothing  w    = put' l2 Nothing w `bind` put' l1 Nothing
  getNew s             = getM l2 (get' l1 s)

phi :: (v -> Bool) -> Lens v v
phi p = { put := putNew 
        , get := \v' -> if p v' then Just v' else Nothing
        }
 where
  putNew s v | p v       = v
             | otherwise = error "phi: predicate is not fulfilled"

idLens :: Lens v v
idLens = { put := \_ v -> v
         , get := \v'  -> Just v'
         }  

------------------------------------------------------------
----------------------- Products ---------------------------
------------------------------------------------------------

addFst :: (Maybe (s1,v) -> v -> s1) -> Lens (s1,v) v
addFst f = enforceGetPut { put := put_
                         , get := \(_,v') -> Just v'
                         }
 where
  put_ s v' = f s v' `bind` \s1' -> (s1',v')

addSnd :: (Maybe (v,s1) -> v -> s1) -> Lens (v,s1) v
addSnd f = enforceGetPut { put := put_
                         , get := \(v',_) -> Just v'
                         }
 where
  put_ s v' = f s v' `bind` \s1' -> (v',s1')

keepFstOr :: (v -> s1) -> Lens (s1,v) v
keepFstOr f = addFst (\s v' -> maybe (f v') fst s)

keepFst :: Lens (s1,v) v
keepFst = keepFstOr (\v -> failed)

keepSndOr :: (v -> s1) -> Lens (v,s1) v
keepSndOr f = addSnd (\s v' -> maybe (f v') snd s)

keepSnd :: Lens (v,s1) v
keepSnd = keepSndOr (\v -> failed)

copy :: Lens (v,v) v
copy = phi (uncurry (==)) <.> addSnd (\_ v -> v)

------------------------------------------------------------
-------------------------- Lists ---------------------------
------------------------------------------------------------

inList :: Lens [a] (Either () (a,[a]))
inList = isoLens inn out
 where
  inn eVal = either (\() -> []) (\(x,xs) -> x:xs) eVal
  out xs   = case xs of
                  []   -> Left ()
                  y:ys -> Right (y,ys)

outList :: Lens (Either () (a,[a])) [a]
outList = isoLens out inn
 where
  inn eVal = either (\() -> []) (\(x,xs) -> x:xs) eVal
  out xs   = case xs of
                  []   -> Left ()
                  y:ys -> Right (y,ys)

mapLens :: Lens a b -> Lens [a] [b]
mapLens f = (inList <.> (idLens <+> (f <*> mapLens f)) <.> outList)


------------------------------------------------------------
----------------------- Sums -------------------------------
------------------------------------------------------------

(<*>) :: Lens s1 v1 -> Lens s2 v2 -> Lens (s1,s2) (v1,v2)
l1 <*> l2 = { put := put_
            , get := \(s1,s2) -> Just (get' l1 s1, get' l2 s2)
            }
 where
  put_ s (v1',v2') =
    let s1' = put' l1 (fmap fst s) v1'
        s2' = put' l2 (fmap snd s) v2'
    in (s1',s2')


(<+>) :: Lens s1 v1 -> Lens s2 v2 -> Lens (Either s1 s2) (Either v1 v2)
l1 <+> l2 = { put := put_
            , get := \s -> Just (either (Left . get' l1) (Right . get' l2) s)
            }
 where
  put_ s (Left v1') = put' l1 (l s) v1' `bind` Left
  put_ s (Right v2') = put' l2 (r s) v2' `bind` Right
  l :: Maybe (Either s1 s2) -> Maybe s1
  l = maybe Nothing (either Just (const Nothing))
  r :: Maybe (Either s1 s2) -> Maybe s2
  r = maybe Nothing (either (const Nothing) Just)

-- injl :: Lens (Either v1 v2) v1
-- injl = { put := put_
--        , get := \v1 -> Left v1
--        }
--  where
--   put_ (Left  v) = 
--   put_ (Right v) = 

------------------------------------------------------------
--------------------- Isomorphism --------------------------
------------------------------------------------------------

isoLens :: (a -> b) -> (b -> a) -> Lens b a
isoLens f g = { put := \_ v -> f v
              , get := \s   -> Just (g s)
              }

------------------------------------------------------------
--------------------------- Auxs ---------------------------
------------------------------------------------------------

-- enforces _PutTwice_ law
-- s' \in put' l s v => s' = put' l s' v
enforceGetPut :: Lens a b -> Lens a b
enforceGetPut l = { put := put_
                  , get := getM l
                  }
 where
  put_ :: Maybe a -> b -> a
  put_ ms v
   | isJust ms && getM l (fromJust ms) == Just v = fromJust ms
   | otherwise                                   = put' l ms v
