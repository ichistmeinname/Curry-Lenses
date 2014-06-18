module Replace where

import Char ( intToDigit, isDigit )
import qualified Lens as L

data Res a = New a
           | Replaced a

type PReplace a = String -> (a,String) -> Res String

replace :: PReplace a -> String -> (a,String) -> String
replace pReplace str pair = case pReplace str pair of
                                 Replaced s -> s

pretty :: PReplace a -> String -> (a,String) -> String
pretty pReplace str pair = case pReplace str pair of
                           New s -> s

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
num str (d,str') | d <= 9 && d >= 0 = (charP isDigit) str (intToDigit d,str')

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

-- Choice operator
(<|>) :: PReplace a -> PReplace a -> PReplace a
(pA <|> pB) str (expr,str') =
  case pA str (expr,str') of
       New val      -> case pB str (expr,str') of
                            New val'      -> failed
                            replacedStr   -> replacedStr
       replacedStr  -> replacedStr

(<>) :: PReplace a -> PReplace b -> PReplace (a,b)
(pA <> pB) str ((expr1,expr2),str')
  | str == str1 ++ str2 = -- && not (null str1) && not (null str2) =
     Replaced res
  -- | null str1 && null str2 = pA str1 (expr1, unwrap newString) 
 where
  Replaced newString = pB str2 (expr2,str')
  Replaced res       = pA str1 (expr1, newString)
  str1, str2 free

(<<<) :: PReplace a -> PReplace () -> PReplace a
(pA <<< pB) str (e,str') = (pA <> pB) str ((e,()),str')

(>>>) :: PReplace () -> PReplace b -> PReplace b
(pA >>> pB) str (e,str') = (pA <> pB) str (((),e),str')

whitespace str ((),str') = charMany ' ' str str'

-- many :: (P

charStrict :: (Char -> Bool) -> PReplace Char
charStrict p input (v,new) =
  case input of
       "" -> New (v:new)
       _  -> char' input new
 where
  char' str@(c':str') rest
    | p c' && rest == str'    = Replaced (v:rest)
    | p c' && null str'       = Replaced (v:rest)
    -- | p c' && not (null rest) = New (v:rest)
    | otherwise               = failed

charP :: (Char -> Bool) -> PReplace Char
charP p input (v,new) =
  case input of
       "" -> New (v:new)
       _  -> char' input new
 where
  char' str@(c':str') rest
    | p c'                    = Replaced (v:rest)
    -- | p c' && rest == str'    = Replaced (v:rest)
    -- | p c' && null str'       = Replaced (v:rest)
    -- | p c' && not (null rest) = New (v:rest)
    | otherwise               = New (v:rest)

char :: Char -> String -> String -> Res String
char chr input new = charP (== chr) input (chr,new)
 --  case input of
 --       "" -> New (chr:new)
 --       _  -> char' chr input new
 -- where
 --  char' c str@(c':str') rest
 --    | c == c' && rest == str'    = Replaced str
 --    | c == c' && null str'       = Replaced (c:rest)
 --    | c == c' && not (null rest) = New (c:rest)
 --    | otherwise                  = New (c:rest)

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

test str = if str == str1 ++ str2
          -- && not (null str1)
          -- && not (null str2)
         then (plusMinus str1 (Plus, unwrap newString),str1,str2)
         else failed
 where
  str1, str2 free
  newString = whitespace str2 ((),"test")
  -- Replaced res       = plusMinus str1 (Plus, newString)

test2 str (expr1,expr2) | str == str1 ++ str2 = -- && not (null str1) && not (null str2) =
      (endString,str1,str2)
  -- | null str1 && null str2 = pA str1 (expr1, unwrap newString) 
 where
  newString = (num) str2 (expr2,"")
  endString = (plusMinus) str1 (expr1, unwrap newString)
  str1, str2 free

test3 str expr2 | str == str1 ++ str2 = -- && not (null str1) && not (null str2) =
      (endString,str1,str2)
  -- | null str1 && null str2 = pA str1 (expr1, unwrap newString) 
 where
  newString = (charStrict (isDigit)) str2 (intToDigit expr2,"")
  endString = (charStrict (== '+')) str1 ('+', unwrap newString)
  str1, str2 free


isNew :: Res a -> Bool
isNew (New _)      = True
isNew (Replaced _) = False

unwrap :: Res a -> a
unwrap (New v)      = v
unwrap (Replaced v) = v