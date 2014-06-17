module Replace where

import Char ( intToDigit, isDigit )

data Res a = New a
           | Replaced a

type PReplace a = String -> (a,String) -> Res String

replace :: PReplace a -> String -> (a,String) -> String
replace pReplace str = unwrap . pReplace str

data Expr = BinOp Op Expr Expr
          | Num Int

data Op = Plus | Minus | Mult | Div

expr :: PReplace Expr
expr str (BinOp op e1 e2,str') =
  ((expr <<< whitespace) <> (plusMinus <<< whitespace)
                           <> expr) str (((e1,op),e2),str')
-- expr str (Paren e,str')
--   | head str == '(' = "(" ++ expr (tail str) (e,")"++str')
--   | otherwise = "(" ++ expr str (e,')':str')
expr str (Num v,str')   = num str (v,str')

num :: PReplace Int
num str (d,str') | d <= 9 && d >= 0 = (char' isDigit) str (intToDigit d,str')

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
  | str == str1 ++ str2 && not (null str1) && not (null str2) =
     pA str1 (expr1, newString)
 where
  Replaced newString = pB str2 (expr2,str')
  str1, str2 free

(<<<) :: PReplace a -> PReplace () -> PReplace a
(pA <<< pB) str (e,str') = (pA <> pB) str ((e,()),str')

(>>>) :: PReplace () -> PReplace b -> PReplace b
(pA >>> pB) str (e,str') = (pA <> pB) str (((),e),str')

whitespace str ((),str') = charMany ' ' str str'

plusMinus :: PReplace Op
plusMinus str (op,str') = char' (`elem` ['+','-']) str (opStr,str')
 where
  opStr = case op of
               Plus  -> '+'
               Minus -> '-'
-- many :: (P

char' :: (Char -> Bool) -> PReplace Char
char' p input (v,new) =
  case input of
       "" -> New (v:new)
       _  -> char'' input new
 where
  char'' str@(c':str') rest
    | p c' && rest == str'    = Replaced (v:rest)
    | p c' && null str'       = Replaced (v:rest)
    | p c' && not (null rest) = New (v:rest)
    | otherwise               = New (v:rest)

char :: Char -> String -> String -> Res String
char chr input new =
  case input of
       "" -> New (chr:new)
       _  -> char' chr input new
 where
  char' c str@(c':str') rest
    | c == c' && rest == str'    = Replaced str
    | c == c' && null str'       = Replaced (c:rest)
    | c == c' && not (null rest) = New (c:rest)
    | otherwise                  = New (c:rest)

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