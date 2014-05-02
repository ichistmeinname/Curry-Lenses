module Lens where

import Maybe (isJust, isNothing, fromJust)
import SetFunctions

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
  putNew _ v | p v       = v
             | otherwise = error "phi: predicate is not fulfilled"

phiSource :: (s -> Bool) -> Lens s v -> Lens s v
phiSource p l = { put := put_
                , get := get_
                }
 where
  get_ s = case getM l s of
                Just v  -> if p s then Just v else Nothing
                Nothing -> Nothing
  put_ s v = put' l s v `bind` \s' ->
               if p s' then s' else error ("phiSource: predicate is not fulfilled; " ++ 
                                             "source: " ++ show s ++ ", view: " ++ show v)
                  
idLens :: Lens v v
idLens = { put := \_ v -> v
         , get := \v'  -> Just v'
         }  

botLens :: Lens a a
botLens = { put := \_ _ -> error "bottom", get := const Nothing }
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
keepFst = keepFstOr (const failed)

keepSndOr :: (v -> s1) -> Lens (v,s1) v
keepSndOr f = addSnd (\s v' -> maybe (f v') snd s)

keepSnd :: Lens (v,s1) v
keepSnd = keepSndOr (const failed)

remFst :: (v -> v1) -> Lens v (v1,v)
remFst f = { put := put_
           , get := get_
           }
 where
  get_ v                       = Just (f v,v)
  put_ _ (v1,v) | f v == v1 = v
                   | otherwise = error "remFst: first and second value of pair are not equal"  

remSnd :: (v -> v1) -> Lens v (v,v1)
remSnd f = { put := put_
           , get := get_
           }
 where
  get_ v                       = Just (v, f v)
  put_ _ (v,v1) | f v == v1 = v
                   | otherwise = error "remSnd: first and second value of paire are not equal"

remFstOne :: Lens v ((),v)
remFstOne = remFst (const ())

remSndOne :: Lens v (v,())
remSndOne = remSnd (const ())

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

cons :: Lens [a] (a, [a])
cons = inList <.> injR

unhead :: Lens [a] a
unhead = cons <.> keepSnd

untail :: Lens [a] [a]
untail = cons <.> keepFst

unhead' :: Lens [a] a
unhead' = cons <.> keepSndOr (const [])

untail' :: Lens [a] [a]
untail' = cons <.> keepFstOr (\ (x:_) -> x)

------------------------------------------------------------
--------------------------- Sums ---------------------------
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

inj :: (Maybe (Either v v) -> v -> Bool) -> Lens (Either v v) v
inj p = enforceGetPut { put := put_
                      , get := \s -> either Just Just s
                      }
 where
  put_ s v = if p s v then Left v else Right v

injOr :: (v -> Bool) -> Lens (Either v v) v
injOr p = inj (\s v -> maybe (p v) isLeft s)
 where
  isLeft s = either (const True) (const False) s

injL :: Lens (Either v1 v2) v1
injL = { put := \_ v -> Left v
       , get := get_
       }
 where
  get_ (Left  v) = Just v
  get_ (Right _) = Nothing

injR :: Lens (Either v1 v2) v2
injR = { put := \_ v -> Right v
       , get := get_
       }
 where
  get_ (Left  _) = Nothing
  get_ (Right v) = Just v

(\/) :: Lens s v1 -> Lens s v2 -> Lens s (Either v1 v2)
l1 \/ l2 = { put := put_
           , get := get_
           }
 where
  put_ :: Maybe s -> Either v1 v2 -> s
  put_ s (Left  v1) = disjoint l1 l2 (put' l1 s v1)
  put_ s (Right v2) = disjoint l2 l1 (put' l2 s v2)
  get_ :: s -> Maybe (Either v1 v2)
  get_ s = let (v1,v2) = (getM l1 s, getM l2 s)
                  in checkDisjoint v1 v2
  checkDisjoint v1 v2 | isNothing v1 && isNothing v2 = Nothing
                      | isJust    v1 && isJust    v2 = Nothing
                      | isJust    v1 && isNothing v2 = liftMaybe Left v1
                      | isNothing v1 && isJust    v2 = liftMaybe Right v2
  liftMaybe _ Nothing  = Nothing
  liftMaybe f (Just v) = Just (f v)

(\/.) :: Lens s v1 -> Lens s v2 -> Lens s (Either v1 v2)
l1 \/. l2 = (phiSource (not . dom l2) l1) \/ l2

(.\/) :: Lens s v1 -> Lens s v2 -> Lens s (Either v1 v2)
l1 .\/ l2 = l1 \/ (phiSource (not . dom l1) l2)

eitherS :: (s -> Bool) -> Lens s v1 -> Lens s v2 -> Lens s (Either v1 v2)
eitherS p f g = (phiSource p f) \/ (phiSource (not . p) g)

------------------------------------------------------------
--------------------- Conditionals -------------------------
------------------------------------------------------------

ifVThenElse :: (v -> Bool) -> Lens s v -> Lens s v -> Lens s v
ifVThenElse p l1 l2 = ((l1 <.> phi p) .\/ l2) <.> inj (\_ -> p)

ifSThenElse :: (s -> Bool) -> Lens s v -> Lens s v -> Lens s v
ifSThenElse p l1 l2 = { put := \s v -> put' (l s) s v
                      , get := \s -> getM (l (Just s)) s
                      }
 where
  l :: Maybe s -> Lens s v
  l (Just s) = eitherS p l1 l2 <.> injOr (\_ -> p s)
  l Nothing  = eitherS p l1 l2 <.> injOr (\_ -> True)


ifThenElse :: (Maybe (Either v v) -> v -> Bool) -> Lens s v -> Lens s v -> Lens s v
ifThenElse p l1 l2 = (l1 .\/ l2) <.> inj p

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

disjoint :: Lens a b -> Lens a c -> a -> a
disjoint l1 l2 s
  | dom l1 s && not (dom l2 s) = s
  | otherwise                  = error "disjoint failed"

dom :: Lens a b -> a -> Bool
dom l s = isJust (getM l s)