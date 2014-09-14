module ReplaceParse
  ( char, charP, digit, whitespace, whitespaces, pure
  , many, optional, paren
  , (<|>), (<<<), (>>>), (<>)
  , get, put, PReplace, replaceParse
  ) where

import SetFunctions
import Char (isDigit, intToDigit)
import Lens ( Lens, get, put)

----- main definitions

-- Datatype definition
--- result is a triple:
---  (1) replacement or pretty-print
---  (2) remaining inputr
---  (3) remaining string to concatenate at the end
type PReplace a = String -> (a,String) -> (String,String,String)

-- Transforms a `PReplace` structure to a lens
replaceParse :: PReplace a -> Lens String (a,String)
replaceParse pReplace input pair =
  case pReplace input pair of
       (res1,_,str) -> res1 ++ str


----- Primitives for replace operations

-- Replaces a given character
char :: Char -> PReplace Char
char c = charP (== c)

-- Replaces a character if the given predicate holds
charP :: (Char -> Bool) -> PReplace Char
charP _ ""     (e,str') = ([e],"",str')
charP p (c:cs) (e,str') | p c  = ([e],cs,str')

-- Replaces a digit
digit :: PReplace Int
digit input (d,str')
  | d <= 9 && d >= 0 = charP isDigit input (intToDigit d,str')

-- Replaces a whitespace
whitespace :: PReplace ()
whitespace input ((),str') = char ' ' input (' ',str')

-- Replaces many whitespaces
whitespaces :: PReplace ()
whitespaces input = case input of
  "" -> whitespace ""
  _  -> ((whitespace >>> whitespaces')) input
 where
  whitespaces' :: PReplace ()
  whitespaces' input' ((),str') = case input' of
    "" -> pure "" ((),str')
    _  -> ((whitespace >>> whitespaces') <|> pure) input' ((),str')

-- Always succeeding replace operator
pure :: PReplace a
pure input (_,str') = ("",input,str')


paren :: PReplace a -> PReplace a
paren pReplace str@"" (e,str') =
  (charP (== '(') <> pReplace <> charP (== ')')) str ((('(',e),')'),str')
paren pReplace str@('(' : _ ++ ")"++_) (e,str') =
  (charP (== '(') <> pReplace <> charP (== ')')) str ((('(',e),')'),str')

 --  | str == ""                 = go str
 --  | str == "(" ++ str1 ++ ")" = go str1
 -- where
 --  str1 free
 --  go input =
 --    case pReplace input (e,')':str') of
 --       (res1,str2,str2') -> ('(' : res1, str2,str2')

many :: PReplace a -> PReplace [a]
many _       input ([],str')   = ("",input,str')
many reparse input (x:xs,str') =
  ((reparse <> many reparse) <|> pure) input ((x,xs),str')

optional :: PReplace a -> PReplace a
optional pReplace = pReplace <|> pure


(<|>) :: PReplace a -> PReplace a -> PReplace a
(pA <|> _)  ""          = pA ""
(pA <|> pB) input@(_:_) = (pA ? pB) input
-- (pA <|> pB) input@(_:_) (e,str')
--   | noParse   = pB input (e,str')
--   | otherwise = pA input (e,str')
--  where
--   noParse = isEmpty (set2 pA input (e,str'))
--   pRes    = pA input (e,str')

(<<<) :: PReplace a -> PReplace () -> PReplace a
(pA <<< pB) input (e,str') = (pA <> pB) input ((e,()),str')

(>>>) :: PReplace () -> PReplace a -> PReplace a
(pA >>> pB) input (e,str') = (pA <> pB) input (((),e),str')

-- non-recursive version
-- (<>) :: PReplace a -> PReplace b -> PReplace (a,b)
-- (pA <> pB) input ((e1,e2),str') = case input of
--   "" -> (res1 ++ res2,str2,str2')
--   _  -> if null str1 && (res2,str',str2') /= pure str1 (e2,str1')
--           then failed
--           else (res1 ++ res2,str2,str2')
--  where
--   (res1,str1,str1') = pA input (e1,"")
--   (res2,str2,str2') = pB str1 (e2,str')

-- recursive version
(<>) :: PReplace a -> PReplace b -> PReplace (a,b)
(pA <> pB) input ((e1,e2),str') = case input of
  "" -> (res1,str2,str1' ++ str2')
  _  -> if null str1 && (res2,str',str2') /= pure str1 (e2,str1')
          then failed
          else (res1,str2,str1' ++ str2')
 where
  (res1,str1,str1') = pA input (e1,res2)
  (res2,str2,str2') = pB str1 (e2,str')