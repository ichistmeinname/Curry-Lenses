module LensCheck where

import Lens
import EasyCheck (for, easyCheck, verboseCheck2, (<~>), Test(..))


neList :: [a]
neList = x:xs where x,xs free

checkListPutGet :: Lens [a] b -> [Test]
checkListPutGet lens = for neList (\xs -> check' xs)
 where
  check' xs = put lens xs (get lens xs) <~> xs

checkListGetPut :: Lens [a] b -> b -> [Test]
checkListGetPut lens x = for neList (\xs -> check' xs)
 where
  check' xs = get lens (put lens xs x) <~> x

checkListPutPut :: Lens [a] b -> b -> b -> [Test]
checkListPutPut lens x y = for neList (\xs -> check' xs)
 where
  check' xs = put lens (put lens xs x) y <~> put lens xs y

checkPutGet :: Lens a b -> a -> [Test]
checkPutGet lens x = put lens x (get lens x) <~> x

checkGetPut :: Lens a b -> a -> b -> [Test]
checkGetPut lens x y = get lens (put lens x y) <~> y

checkPutPut :: Lens a b -> a -> b -> b -> [Test]
checkPutPut lens x y z = put lens (put lens x y) z <~> put lens x y
