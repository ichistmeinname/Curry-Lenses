module Test where

import EasyCheck

revAntiDistributes :: [a] -> [a] -> [Test]
revAntiDistributes xs ys = reverse (xs++ys) -=- reverse ys++reverse xs

revDistributes :: [Bool] -> [Bool] -> [Test]
revDistributes xs ys = reverse (xs++ys) -=- reverse xs++reverse ys

revAppLawWithTrivials :: [Bool] -> [Bool] -> [Test]
revAppLawWithTrivials xs ys =
  trivial (null xs || null ys) (revAntiDistributes xs ys)

revAppLawWithoutTrivials :: [Bool] -> [Bool] -> [Test]
revAppLawWithoutTrivials xs ys =
  not (null xs || null ys) ==> revAppLawWithTrivials xs ys

neList :: [a]
neList = x:xs
 where
  x, xs free

revAppLawWithCustomGen :: [Test]
revAppLawWithCustomGen =
  for neList (\xs -> for neList (\ys -> revAppLawWithTrivials xs ys))