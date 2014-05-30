module Replace where

import Char ( intToDigit )

data Res a = New a
           | Replaced a

type PReplace a = String -> (a,String) -> Res String

data Expr = BinOp Op Expr Expr
          | Num Int
          | Paren Expr

data Op = Plus | Minus | Mult | Divs

-- expr :: PReplace Expr
-- expr str (BinOp op e1 e2,str') =
--   ((plusMinus <<< whitespace) <> (expr <<< whitespace)
--                            <> expr) str (((op,e1),e2),str')
-- expr str (Paren e,str')
--   | head str == '(' = "(" ++ expr (tail str) (e,")"++str')
--   | otherwise = "(" ++ expr str (e,')':str')
-- expr str (Num v,str')   = num str (v,str')

num :: PReplace Int
num str (d,str') | d <= 9 && d >= 0 = char (intToDigit d) str str'

(<>) :: PReplace a -> PReplace b -> PReplace (a,b)
(pA <> pB) str ((expr1,expr2),str')
  | str == str1 ++ str2 && not (null str1) && not (null str2) =
     pA str1 (expr1, newString)
 where
  Replaced newString = pB str2 (expr2,str')
  str1, str2 free

(<<<) :: PReplace a -> PReplace () -> PReplace a
(pA <<< pB) str (expr,str') = (pA <> pB) str ((expr,()),str')

(>>>) :: PReplace () -> PReplace b -> PReplace b
(pA >>> pB) str (expr,str') = (pA <> pB) str (((),expr),str')

whitespace str ((),str') = charMany ' ' str str'

plusMinus :: PReplace Op
plusMinus str (op,str') =
  case op of
       Plus  -> char '+' str str'
       Minus -> char '-' str str'

-- many :: (P

char :: Char -> String -> String -> Res String
char c str rest =
  case str of
       "" -> New (c:rest)
       _  -> char' c str rest
 where
  char' c str@(c':str') rest
    | c == c' && rest == str'    = Replaced str
    | c == c' && null str'       = Replaced (c:rest)
    | c == c' && not (null rest) = New (c:rest)
    | otherwise                  = New (c:rest)

charMany :: Char -> String -> String -> Res String
charMany c str rest =
  case str of
       "" -> New (c:rest)
       _  -> charMany' c str rest
 where
  charMany' c str@(c':str') rest
    | c == c' && rest == str'    = Replaced str
    | c == c' && not (null str') = case charMany' c str' rest of
                                        New cs      -> Replaced cs
                                        Replaced cs -> Replaced (c:cs)
    | c == c'  = Replaced (c:str' ++ rest)
    | otherwise                  = New (c:rest)
  -- charMany'' c str@(c':str') rest
  --   -- | c == c' && rest == str'    = str
  --   | c == c' && not (null str') = c:charMany'' c str' rest
  --   | c == c'  = c:str' ++ rest
  --   | otherwise                  = rest

test str = if str == str1 ++ str2
          && not (null str1)
          && not (null str2)
         then (plusMinus str1 (Plus, newString),str1,str2)
         else failed
 where
  str1, str2 free
  Replaced newString = whitespace str2 ((),"test")
  -- Replaced res       = plusMinus str1 (Plus, newString)


isNew :: Res a -> Bool
isNew (New _)      = True
isNew (Replaced _) = False

unwrap :: Res a -> a
unwrap (New v)      = v
unwrap (Replaced v) = v