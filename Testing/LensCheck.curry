module LensCheck where

import Lens
import EasyCheck (for, easyCheck, verboseCheck2, (<~>), (==>), Test(..))
import SetFunctions (isEmpty,set2)
import Maybe (isJust)

neList :: [a]
neList = x:xs where x,xs free

checkListGetPut :: Lens [a] b -> [Test]
checkListGetPut lens = for neList (\xs -> check' xs)
 where
  check' xs = isJust (getM lens xs)
               ==> put lens xs (get lens xs) <~> xs

checkListPutGet :: Lens [a] b -> b -> [Test]
checkListPutGet lens x = for neList (\xs -> check' xs)
 where
  check' xs = isJust (putM lens xs x)
                ==> get lens (put lens xs x) <~> x

checkListPutPut :: Lens [a] b -> b -> b -> [Test]
checkListPutPut lens x y = for neList (\xs -> check' xs)
 where
  check' xs = put lens (put lens xs x) y <~> put lens xs y

checkListPutDet :: Lens [a] b -> (b,b) -> [Test]
checkListPutDet lens (y1,y2) =
  for neList (\xs -> for neList (\ys -> check' xs ys))
 where
  check' xs ys =
   isJust (putM lens xs y1) && isJust (putM lens ys y2)
   && put lens xs y1 == put lens ys y2
     ==> y1 <~> y2

checkListPutStab :: Lens [a] b -> [Test]
checkListPutStab lens = for neList (\xs -> check' xs)
 where
  check' xs = not (isEmpty $ set2 (==) (put lens xs y) xs) <~> True
  y free

checkGetPut :: Lens a b -> a -> [Test]
checkGetPut lens x = isJust (getM lens x)
                       ==> put lens x (get lens x) <~> x

checkPutGet :: Lens a b -> a -> b -> [Test]
checkPutGet lens x y = isJust (putM lens x y)
                         ==> get lens (put lens x y) <~> y

checkPutPut :: Lens a b -> a -> b -> b -> [Test]
checkPutPut lens x y z =
  isJust (putM lens x y) && isJust (putM lens (put lens x y) z)
    ==> put lens (put lens x y) z <~> put lens x y

checkPutDet :: Lens a b -> (a,a) -> (b,b) -> [Test]
checkPutDet lens (x1,x2) (y1,y2) =
  isJust (putM lens x1 y1) && isJust (putM lens x2 y2)
    && put lens x1 y1 == put lens x2 y2
      ==> y1 <~> y2

checkPutStab :: Lens a b -> a -> [Test]
checkPutStab lens x = not (isEmpty $ set2 (==) (put lens x y) x) <~> True
 where y free