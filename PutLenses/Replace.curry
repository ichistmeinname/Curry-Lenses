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
---     > num "" (3,"")
---     "3"
---     > num "" (3," tests")
---     "3 tests"
---
---   2. Only pretty-printed values of the given type `a` are valid strings for
---      for the first argument, the `put` direction fails for invalid strings.
---      In particular, this restriction of invalid strings holds for remaining
---      strings, which can be the result of a pretty-print. However, there is
---      one exception: if the remaining string in the second argument and
---      the current string are the same, the replacement succeeds.
---
---     > num "3" (4,"")
---     "4"
---     > num "3" (4," tests")
---     "3 tests"
---     > num "3 !!!" (4," tests")
---     <no result>
---     > num "3 tests" (4," tests")
---     "4 tests"
---
---   3. All other `put` operations are layout-preserving concerning redundant
---      structures like whitespaces, parentheses and other optional parts.
---
---     > ((num <<< whitespace) <> num) "3 4" ((1,2),"")
---     "1 2"
---     > ((num <<< whitespace) <> num) "3   4" ((1,2),"")
---     "1   2"
---
--- @author Sandra Dylus
--- @version June 2014
--------------------------------------------------------------------------------

module Replace where

import Char ( intToDigit, isDigit )
import qualified Lens as L

data Res a = New a
           | Replaced a

type PReplace a = String -> (a,String) -> Res String

infixl 4 <>, <<<, >>>
infixl 3 <|>

replace :: PReplace a -> String -> (a,String) -> String
replace pReplace str pair = case pReplace str pair of
                                 Replaced s -> s

pretty :: PReplace a -> String -> (a,String) -> String
pretty pReplace str pair = case pReplace str pair of
                           New s -> s

get :: PReplace a -> String -> (a,String)
get pReplace str | (pretty pReplace ? replace pReplace) str v == str = v
 where v free

data Expr = BinOp Op Expr Expr
          | Num Int

data Op = Plus | Minus | Mult | Div

expr :: PReplace Expr
expr str (BinOp op e1 e2,str') =
  ((plusMinus <<< whitespace) <> (expr <<< whitespace)
                         <> expr) str (((op,e1),e2),str')
expr str (Num v,str')   = num str (v,str')

num :: PReplace Int
num str (d,str')
  | d <= 9 && d >= 0 = (charP isDigit) str (intToDigit d,str')


plusMinus :: PReplace Op
plusMinus str (op,str') = charP (`elem` ['+','-']) str (opStr,str')
 where
  opStr = case op of
               Plus  -> '+'
               Minus -> '-'

-- paren :: PReplace
-- paren = (opening <> closing)
--  where
--   opening str (e,str') =

strict :: PReplace a -> PReplace a
strict pReplace str pair =
  case pReplace str pair of
       New _ -> failed
       res   -> res

-- Choice operator
(<|>) :: PReplace a -> PReplace a -> PReplace a
(pA <|> pB) str (e, str') =
  case (pA ? pB) str (e,str') of
       res@(Replaced _) -> res

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

-- whitespaces :: PReplace ()
whitespaces str ((),str') | n > 0 = (many whitespace) str (replicate n (),str')
 where n free
-- charMany ' ' str str'

whitespace :: PReplace ()
whitespace str ((),str') = char ' ' str str'

paren :: PReplace a -> PReplace a
paren pReplace str (e,str') =
  (charP (== '(') <> pReplace <> charP (== ')')) str ((('(',e),')'),str')

optional :: Char -> PReplace a -> PReplace a
optional opt pReplace =
 (charP' (== opt) >>> pReplace) <|> pReplace

many :: PReplace a -> PReplace [a]
many pReplace str (xs,str') =
  case xs of
       [y]  -> pReplace str (y,str')
       y:ys -> (pReplace <> many pReplace) str ((y,ys),str')

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

char :: Char -> String -> String -> Res String
char chr input new = charP (== chr) input (chr,new)

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

charMany :: Char -> String -> String -> Res String
charMany chr input new =
  case input of
       "" -> New (chr:new)
       _  -> charMany' chr input new
 where
  charMany' c str@(c':str') rest
    | c == c' && rest == str'    = Replaced str
    | c == c' && not (null str') = case charMany' c str' rest of
                                        New cs      -> Replaced cs
                                        Replaced cs -> Replaced (c:cs)
    | c == c'  = Replaced (c:str' ++ rest)
    | otherwise                  = New (c:rest)

charManyStrict :: Char -> String -> String -> Res String
charManyStrict chr input new =
  case input of
       "" -> New (chr:new)
       _  -> charMany' chr input new
 where
  charMany' c (c':str') rest
    -- | c == c' && rest == str'    = Replaced str
    | c == c' && null str'       = Replaced (c:rest)
    | c == c' && not (null str') = case charMany' c str' rest of
                                        Replaced cs -> Replaced (c:cs)
    -- | c == c'  = Replaced (c:str' ++ rest)
    -- | otherwise                  = New (c:rest)

test str = if str == str1 ++ str2
          -- && not (null str1)
          -- && not (null str2)
         then (plusMinus str1 (Plus, unwrap newString),str1,str2)
         else failed
 where
  str1, str2 free
  newString = whitespace str2 ((),"test")
  -- Replaced res       = plusMinus str1 (Plus, newString)

test2 pA pB str (expr1,expr2) str'
  | str == str1 ++ str2 = -- && not (null str1) && not (null str2) =
     if null str
       then pA str1 (expr1, unwrap (pB str2 (expr2,str')))
       else (strict pA) str1 (expr1, unwrap ((strict pB) str2 (expr2,str')))
 where str1,str2 free

test3 str expr2 str' | str == str1 ++ str2 ++ str3 =
-- && not (null str1) && not (null str2) =
  if null str
    then (endString,str1,str2)
    else if any null [str1,str2]
         then failed
         else (endString,str1,str2)
  -- | null str1 && null str2 = pA str1 (expr1, unwrap newString)
 where
  newString = (strict (charP (isDigit))) str2 (intToDigit expr2,str')
  endString = (strict (charP (== '+'))) str1 ('+', unwrap newString)

  str1, str2, str3 free

test4 str ((expr1,expr2),str')
 | str == str1 ++ str2 ++ str3 && unwrap endString ++ str' == str =
     if null str
        then (endString,str1,str2)
        else if any null [str1,str2]
                then failed
                else (endString,str1,str2)
  -- && not (null str1) && not (null str2) =
  -- | null str1 && null str2 = pA str1 (expr1, unwrap newString)
 where
  newString = (strict (charP isDigit)) str2 (expr2,str')
  endString = (strict (charP (== '+'))) str1 (expr1, unwrap newString)
  str1, str2, str3 free

test5 str (expr1,expr2,expr3) str'
  | str == str1 ++ str2 ++ str3 ++ str4 ++ str5 ++ str6 =
-- && not (null str1) && not (null str2) =
 if null str
     then (newString str',str1,str2,str3,str4,str5,str6)
     else if any null [str1,str2,str3,str4,str5]
            then failed
            else if any isNew [endString str'
                              ,newString str'
                              ,secondString str'
                              ,thirdString str'
                              ,fourthString str']
                   then failed
-- (newString str',str1,str2,str3,str4,str5,str6,str')
                   else
                   (newString str',str1,str2,str3,str4,str5,str6)
-- | null str1 && null str2 = pA str1 (expr1, unwrap newString)
 where
  endString s = (strict num) str5 (expr3,s)
  fourthString s = (strict whitespace) str4 ((), unwrap (endString s))
  thirdString s = (strict num) str3 (expr2,unwrap (fourthString s))
  secondString s = whitespace str2 ((), unwrap (thirdString s))
  newString s = plusMinus str1 (expr1, unwrap (secondString s))
  str1, str2, str3, str4, str5, str6 free

test6 str (expr1,expr2,expr3) str'
  | str == str1 ++ str2 ++ str3 ++ str4 ++ str5 ++ str6
    && str == unwrap (newString id)  = if null str
     then (newString strict,str1,str2,str3,str4,str5,str6)
     else if any null [str1,str2,str3,str4,str5]
            then failed
            else if any isNew [endString strict
                              ,newString id
                              ,secondString id
                              ,thirdString id
                              ,fourthString id]
                   then failed
-- (newString str',str1,str2,str3,str4,str5,str6,str')
                   else
                   (newString id,str1,str2,str3,str4,str5,str6)
 where
  endString f = (f num) str5 (expr3,str')
  fourthString f = whitespace str4 ((), unwrap (endString f))
  thirdString f = num str3 (expr2,unwrap (fourthString f))
  secondString f = whitespace str2 ((), unwrap (thirdString f))
  newString f = plusMinus str1 (expr1, unwrap (secondString f))
  str1, str2, str3, str4, str5, str6 free


test7 str (expr1,expr2,expr3) str'
  | str == str1 ++ str2 ++ str3 ++ str4
    -- && (str == unwrap newString ++ str')
    = if null str
     then (newString,str1,str2,str3)
     else if any isNew [endString,thirdString,newString]
            then failed
            else (newString,str1,str2,str3)
 where
  endString = (strict num) str3 (expr3,str')
  thirdString = (strict (num <<< whitespace)) str2 (expr2,unwrap endString)
  newString = (plusMinus <<< whitespace) str1 (expr1, unwrap thirdString)
  str1, str2, str3, str4 free


isNew :: Res a -> Bool
isNew (New _)      = True
isNew (Replaced _) = False

unwrap :: Res a -> a
unwrap (New v)      = v
unwrap (Replaced v) = v