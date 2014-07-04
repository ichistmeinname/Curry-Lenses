--------------------------------------------------------------------------------
--- This module serves as an example application for lenses.
---
--- Instead of a combination of printer and parser, which parses in the `get`
--- and (pretty-) prints in the `put` direction, we want to replace the given
--- string with a new expression.
---
--- The data structure for replacement is defined as follows:
---     data PReplace a = String -> (a,String) -> String
--- The first `String` is a pretty-printed string that represents an expression
--- of type `a`, the first component of the pair `(a,String)` is the value that
--- we want to replace pretty-print as an replacement for the givne string. The
--- second component is a possible remaining string, which will be appended
--- after the pretty-printed value.
--- In the `get` directon, we get a function of type
---     String -> (a,String)
--- which behaves like a parser for an expression of type `a`.
---
--- The intended behaviour is exemplarily show the follwing examples.
---
---   1. You can pretty-print the value, if you use the empty string as the
---      first arugment, i.e. if there is no current pretty-printed version
---      available.
---
---     > put (replaceParse digit) "" (3,"")
---     "3"
---     > put (replaceParse digit) "" (3," tests")
---     "3 tests"
---
---   2. Only pretty-printed values of the given type `a` are valid strings for
---      for the first argument, the `put` direction fails for invalid strings.
---      In particular, this restriction of invalid strings holds for remaining
---      strings, which can be the result of a pretty-print. However, there is
---      one exception: if the remaining string in the second argument and
---      the current string are the same, the replacement succeeds.
---
---     > put (replaceParse digit) "3" (4,"")
---     "4"
---     > put (replaceParse digit) "3" (4," tests")
---     "3 tests"
---     > put (replaceParse digit) "3 !!!" (4," tests")
---     <no result>
---     > put (replaceParse digit) "3 tests" (4," tests")
---     "4 tests"
---
---   3. All other `put` operations are layout-preserving concerning redundant
---      structures like whitespaces, parentheses and other optional parts.
---
---     > put (replaceParse ((digit <<< whitespaces) <> digit)) "3 4" ((1,2),"")
---     "1 2"
---     > put (replaceParse ((digit <<< whitespaces) <> digit))
---           "3   4"
---           ((1,2),"")
---     "1   2"
---
--- @author Sandra Dylus
--- @version June 2014
--------------------------------------------------------------------------------

module Replace
  ( replace, pretty, replaceParse
  , digit, whitespace, whitespaces, char, charP, many, optional, text
  , charMany, textP, whitespaces2
  , paren, succeeds, strict
  , (<>), (<<<), (>>>), (<|>)
  , PReplace
  , get, put
  ) where

import SetFunctions
import Char ( intToDigit, isDigit )
import Lens ( Lens, get, put )
import List ( inits )

type RPLens a = Lens String (a,String)

data Res a = New a
           | Replaced a

type PReplace a = String -> (a,String) -> Res String

infixl 4 <>, <<<, >>>
infixl 3 <|>


replace :: PReplace a -> RPLens a
replace pReplace str pair = case pReplace str pair of
                                 Replaced s -> s

pretty :: PReplace a -> RPLens a
pretty pReplace str pair = case pReplace str pair of
                           New s -> s

replaceParse :: PReplace a -> RPLens a
replaceParse pReplace str pair = unwrap (pReplace str pair)

-- put :: PReplace a -> String -> (a,String) -> String
-- put pReplace str pair = unwrap (pReplace str pair)

-- get :: PReplace a -> String -> (a,String)
-- get pReplace str | replaceParse str v == str = v
--  where v free

digit :: PReplace Int
digit str (d,str')
  | d <= 9 && d >= 0 = (charP isDigit) str (intToDigit d,str')

strict :: PReplace a -> PReplace a
strict pReplace str pair =
  case pReplace str pair of
       New _ -> failed
       res   -> res

-- Choice operator
(pA <|> _)  str@""    (e,str') = pA str (e,str')
(pA <|> pB) str@(_:_) (e,str') = (pA ? pB) str (e,str')
-- (pA <|> pB) str@(_:_) (e,str')
--   | isFailure pA str (e,str') = pB str (e,str')
--   | otherwise                 =
--       case pA str (e,str') of
--            res@(Replaced _) -> res
--            New _            -> case pB str (e,str') of
                                    -- res@(Replaced _) -> res

(<>) :: PReplace a -> PReplace b -> PReplace (a,b)
(pA <> pB) str ((expr1,expr2),str')
  | str == str1 ++ str2 =
     if null str
       then pA str1 (expr1, unwrap (pB str2 (expr2,str')))
       else (strict pA) str1 (expr1, unwrap ((strict pB) str2 (expr2,str')))
 where str1, str2 free

(<<<) :: PReplace a -> PReplace () -> PReplace a
(pA <<< pB) str (e,str') = (pA <> pB) str ((e,()),str')

(>>>) :: PReplace () -> PReplace b -> PReplace b
(pA >>> pB) str (e,str') = (pA <> pB) str (((),e),str')


empty :: PReplace a
empty _ _ = New ""

manyWhitespaces :: PReplace ()
manyWhitespaces str ((),str')
  | n > 0 = (many whitespace) str (replicate n (),str')
 where n free

whitespaces2 :: PReplace ()
whitespaces2 str ((),str') = charMany ' ' str (' ',str')

whitespaces :: PReplace ()
whitespaces input = case input of
  "" -> whitespace ""
  _  -> ((whitespace >>> whitespaces')) input
 where
  whitespaces' :: PReplace ()
  whitespaces' ""           ((),str') = succeeds "" ((),str')
  whitespaces' input'@(_:_) ((),str') =
    ((whitespace >>> whitespaces') <|> succeeds) input' ((),str')

succeeds :: PReplace a
succeeds "" (_,str') = Replaced str'

whitespace :: PReplace ()
whitespace str ((),str') = char ' ' str (' ',str')

paren :: PReplace a -> PReplace a
-- paren pReplace str@"" (e,str') = go str
--   (charP (== '(') <> pReplace <> charP (== ')')) str ((('(',e),')'),str')
-- paren pReplace str@("(" ++ str1 ++ ")" ++ str2) (e,str') =
--     (charP (== '(') <> pReplace <> charP (== ')')) str ((('(',e),')'),str')
paren pReplace str (e,str')
  | str == ""                 = go str ""
  | str == "(" ++ str1 ++ ")" ++ str2 = go str1 str2
 where
  str1,str2 free
  go input rightInj =
    case pReplace input (e,')': rightInj ++ str') of
       Replaced res -> Replaced ('(' : res)
       New      res -> New ('(' : res)
-- paren pReplace ("(" ++ str1 ++ ")" ++ str2) (e,str') = go str1 str2
--   -- | str == ""                 = go str
--   -- | str == "(" ++ str1 ++ ")" ++ str2 = go str1
--  where
--   -- str1,str2 free
--   go input rightInj =
--     case pReplace input (e,')': rightInj ++ str') of
--        Replaced res -> Replaced ('(' : res)
--        New      res -> New ('(' : res)

inject pReplace leftInj rightInj input (e,str') =
    case pReplace input (e,leftInj ++ rightInj ++ str') of
       Replaced res -> Replaced ('(' : res)
       New      res -> New ('(' : res)

optional :: PReplace a -> PReplace b -> PReplace (a,b)
optional pA pB str pair@((e1,e2),str') = case str of
  "" -> pB str (e2,str')
  _  -> opt (strict pA) pB
 where
  opt p1 p2
    | all (\substr -> isFailure p1 substr (e1,str')) (inits str) =
        p2 str (e2,str')
    | otherwise                   = (p1 <> p2) str pair

many :: PReplace a -> PReplace [a]
many pReplace str (xs,str') =
  case xs of
       [y]  -> pReplace str (y,str')
       y:ys -> (pReplace <> many pReplace) str ((y,ys),str')

-- many :: PReplace a -> PReplace [a]
-- many _       _ ([],str')   = Replaced str'
-- many reparse input (x:xs,str') =
--   ((reparse <> many reparse) <|> succeeds) input ((x,xs),str')


charP' :: (Char -> Bool) -> PReplace ()
charP' p input (_,new) =
  case input of
       "" -> New new
       _  -> char' input new
 where
  char' (c':str') rest
    -- | p c'                    = Replaced (v:rest)
    | p c' && rest == str'    = Replaced rest
    | p c' && null str'       = Replaced rest
    -- | p c' && not (null rest) = New (v:rest)
    | otherwise               = New rest

charP :: (Char -> Bool) -> PReplace Char
charP p input (v,new) =
  case input of
       "" -> New (v:new)
       _  -> char' input new
 where
  char' (c':str') rest
    -- | p c'                    = Replaced (v:rest)
    | p c' && rest == str'    = Replaced (v:rest)
    | p c' && null str'       = Replaced (v:rest)
    -- | p c' && not (null rest) = New (v:rest)
    | otherwise               = New (v:rest)

char :: Char -> PReplace Char
char chr input pair = charP (== chr) input pair

charMany :: Char -> PReplace Char
charMany chr input (new,rest) =
  case input of
       "" -> New (new:rest)
       _  -> charMany' input rest
 where
  charMany' input'@(c:cs) str'
    | chr == c && str' == cs    = Replaced input'
    | chr == c && not (null cs) =
        case charMany' cs str' of
             New str      -> New str
             Replaced str -> Replaced (new:str)
    | chr == c                  = Replaced (new:cs ++ str')
    | otherwise                 = New (new:str')

text :: String -> PReplace String
text txt (str ++ rest) (e,str')
  | null str && null rest      = New (e ++ str')
  | txt == str && rest == str' = Replaced (e ++ str')
  | txt == str && null rest    = Replaced (e ++ str')

textP :: (String -> Bool) -> PReplace String
textP p (str ++ rest) (e,str')
  | null str && null rest      = New (e ++ str')
  | p str && rest == str' = Replaced (e ++ str')
  | p str && null rest    = Replaced (e ++ str')
  | p str                 = Replaced (e ++ str')


----- Auxiliary Functions for data structure `Res`

isNew :: Res a -> Bool
isNew (New _)      = True
isNew (Replaced _) = False

unwrap :: Res a -> a
unwrap (New v)      = v
unwrap (Replaced v) = v

isFailure :: PReplace a -> String -> (a,String) -> Bool
isFailure pReplace str (e,str') = isEmpty (set2 pReplace str (e,str'))