module Monadic where

import Maybe

undefined = error "bottom"

fmap :: (a -> b) -> Maybe a -> Maybe b
fmap _ Nothing  = Nothing
fmap f (Just v) = Just (f v)

type Lens s v = Maybe s -> v -> s

put :: Lens s v -> Maybe s -> v -> s
put l s v = l s v

-- get s = v ⇔ s ∈ l (Just s) v
get :: Lens s v -> s -> v
get l s | s =:= l (Just s) v = v
 where v free

-- effect :: (Maybe s -> v -> n -> m) -> Lens n v -> Lens m v
effect f g s v' = f s v' (g s v')

bind :: a -> (a -> b) -> b
bind val f = f val

-- = g (fmap (get f ) s) v′ >= f s
(<.>) :: Lens s v -> Lens v w -> Lens s w
(<.>) l1 l2 s w = l2 (fmap (get l1) s) w  `bind` l1 s

phi :: (v -> Bool) -> Lens v v
phi p _ v' | p v' = v'

idLens :: Lens v v
idLens _ v' = v' 

------------------------------------------------------------
----------------------- Products ---------------------------
------------------------------------------------------------
addFst :: (Maybe (s1,v) -> v -> s1) -> Lens (s1,v) v
addFst f = enforceGetPut put'
 where
  put' :: Lens (s1,v) v
  put' s v' = f s v' `bind` \s1' -> (s1',v')
  -- enforceGetPut :: Lens (s1,v) v -> Lens (s1,v) v
  -- enforceGetPut lens ms@(Just s) v
    -- | s' =:= lens ms v' & s' =:= lens ms v & v =:= v' & lens (Just s') v =:= s'
    -- = put' ms v
  -- s', v' free
  enforceGetPut = id

addSnd :: (Maybe (v,s1) -> v -> s1) -> Lens (v,s1) v
addSnd f = -- enforceGetPut put'
 put'
 where
  put' s v' = f s v' `bind` \s1' -> (v',s1')
  enforceGetPut = undefined

keepFstOr :: (v -> s1) -> Lens (s1,v) v
keepFstOr f = addFst (\s v' -> maybe (f v') fst s)

keepFst :: Lens (s1,v) v
keepFst = keepFstOr (\v -> failed)

keepSndOr :: (v -> s1) -> Lens (v,s1) v
keepSndOr f = addSnd (\s v' -> maybe (f v') snd s)

keepSnd :: Lens (v,s1) v
keepSnd = keepSndOr (\v -> failed)

-- addFst :: (Maybe (v,v) -> v -> v) -> Lens (v,v) v
-- copy :: Lens (v,v) v
-- copy s = ((phi :: ((v,v) -> Bool) -> Lens (v,v) (v,v)) (rev (idLens :: Lens (v,v) (v,v))) <.> addFst (\_ v -> v)) s
--  where
--   rev :: (a -> Bool) -> (a,a) -> Bool
--   rev f (x,y) = x == f y

------------------------------------------------------------
----------------------- Sums -------------------------------
------------------------------------------------------------

-- (Maybe s1 -> v1 -> s1) -> (Maybe s2 -> v2 -> s2) 
-- -> Maybe (Either s1 s2) -> Either v1 v2 -> Either s1 s2
(<+>) :: Lens s1 v1 -> Lens s2 v2 -> Lens (Either s1 s2) (Either v1 v2)
(l1 <+> _) s (Left v1') = l1 (l s) v1' `bind` Left
 where
  l :: Maybe (Either s1 s2) -> Maybe s1
  l = maybe (Nothing :: Maybe s1) (either Just (const (Nothing :: Maybe s1)))
(_ <+> l2) s (Right v2') = l2 (r s) v2' `bind` Right
 where
  r :: Maybe (Either s1 s2) -> Maybe s2
  r = maybe Nothing (either (const Nothing) Just)

(<*>) :: Lens s1 v1 -> Lens s2 v2 -> Lens (s1,s2) (v1,v2)
(l1 <*> l2) s (v1',v2') =
  let s1' = l1 (fmap fst s) v1'
      s2' = l2 (fmap snd s) v2'
  in (s1',s2')
