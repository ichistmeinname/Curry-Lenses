module RegExp where

import Maybe ( isJust )
import Char ( isAlpha )
import List ( tails, isPrefixOf )
import Combinatorial ( sizedSubset )
import Lens

type LensReg1 = RegExp -> Lens String (Int,String)
type LensReg  = RegExp -> Lens String String

type LensReg1M = RegExp -> Lens (Maybe String) (Int,String)

type RegExp = String -> (Bool,String)

type RegLensT  = Lens String (Int,String,String)
type RegLens1M = Lens (Maybe String) (Int,String)
type RegLens1  = Lens String (Int,String)
type RegLens   = Lens String String

copy :: (Char -> Bool) -> RegLens
copy regMatch s v | all regMatch s = v

abQ :: RegLens
abQ ('a':'b':cs) sub = sub ++ cs
abQ ('a':cs)     sub = sub ++ cs

charLT :: (Char -> Bool) -> RegLensT
charLT f = alpha' 0
 where
  alpha' n (c:cs) (index,sub,rs)
    | f c && n == index && cs == rs = sub ++ cs
    | otherwise                     = c: alpha' (n+1) cs (index,sub,rs)

charL1M :: (Char -> Bool) -> RegLens1M
charL1M f = alpha' 0
 where
  alpha' _ Nothing       _           = Nothing
  alpha' n (Just (c:cs)) (index,sub)
    | f c && n == index = Just (sub ++ cs)
    | otherwise               = case alpha' (n+1) (Just cs) (index,sub) of
                                     Just str -> Just (c : str)
                                     Nothing  -> Nothing

charL1 :: (Char -> Bool) -> RegLens1
charL1 f = alpha' 0
 where
  alpha' n (c:cs) (index,sub)
    | f c && n == index = sub ++ cs
    | otherwise               = c: alpha' (n+1) cs (index,sub)

charL :: (Char -> Bool) -> RegLens
charL f (c:cs) sub | f c = sub ++ cs
charL f (c:cs) sub             = c : charL f cs sub

alpha :: RegExp
alpha (c:cs) = (isAlpha c,[c])

star :: RegExp -> RegExp
star reg []                 = (True,"")
star reg (c:cs) | valid1    = case star reg cs of
                                   (True,match2) -> (True,match1 ++ match2)
                                   (False,_)     -> (True,match1)
                | otherwise = (False,match1)
 where
  (valid1,match1) = reg [c]

ac :: RegExp
ac cs = ("ac" `isPrefixOf` cs, "ac")

-- addressRegExp :: RegExp
-- addressRegExp = star alpha <*> whitespace <*>

matchRegExp :: (String -> (Bool,String)) -> String -> (Int,String)
matchRegExp regMatch str = foldr1 (?) $ matches
 where
  matches = map fromJust $ allMatches regMatch str
  fromJust (Just x) = x

allMatches :: RegExp -> String -> [Maybe (Int,String)]
allMatches regMatch str =
  filter isJust $ map (matchSubString regMatch) (zip [0..] $ tails str)

matchSubString :: RegExp -> (Int,String) -> Maybe (Int,String)
matchSubString _        (_,"")       = Nothing
matchSubString checkReg (n,cs@(_:_)) = case checkReg cs of
  (True,match) -> Just (n,match)
  _            -> Nothing

replaceRegInString1' :: LensReg1M
replaceRegInString1' regMatch (Just str) (n,sub)
  | n == index = Just $ replaceRegInString1 regMatch str (n,sub)
  | otherwise  = Nothing
 where
  (index,match) = matchRegExp regMatch str

replaceRegInString :: LensReg
replaceRegInString regMatch str sub = replaceRegInString1 regMatch str (n,sub)
 where n free

replaceRegInString1 :: LensReg1
replaceRegInString1 regMatch str (n,sub)
  | n == index = replace (n,(length match)) str sub
 where
  (index,match) = matchRegExp regMatch str

replace :: (Int,Int) -> String -> String -> String
replace (n,l) (c:cs) sub | n == 0 = sub ++ drop l (c:cs)
                         | n >= 1 = c : replace (n-1,l) cs sub

(>>>) :: (a -> a -> c) -> (b -> b -> d) -> (a,b) -> (a,b) -> (c,d)
(>>>) fV fW (v1,w1) (v2,w2) = (fV v1 v2,fW w1 w2)