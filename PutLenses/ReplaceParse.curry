module ReplaceParse where

import SetFunctions
import Char (isDigit, intToDigit)

type Reparse a = String -> (a,String) -> (String,String,String)

data Expr = BinOp Op Expr Expr
          | Num Int
data Op = Plus | Minus | Mult | Div

-- String -> (Char,String) -> String
char' :: Char -> Reparse Char
char' c = char (== c) 

char :: (Char -> Bool) -> Reparse Char
char _ ""     (e,str') = ([e],"",str')
char p (c:cs) (e,str') | p c  = ([e],cs,str')

-- manyChar :: (Char -> Bool) -> Reparse Char
-- manyChar p "" 

digit :: Reparse Int
digit input (d,str') | d <= 9 && d >= 0 = char isDigit input (intToDigit d,str')

plusMinus :: Reparse Op
plusMinus input (e,str') =
 case e of
      Plus  -> char p input ('+',str')
      Minus -> char p input ('-',str')
 where
  p op = op == '+' || op == '-'

expr :: Reparse Expr
expr input (BinOp op e1 e2,str') =
  ((plusMinus <<< whitespace)
    <> (expr <<< whitespace)
    <> expr) input (((op,e1),e2),str')
expr input (Num d, str') = digit input (d,str')

whitespace :: Reparse ()
whitespace input ((),str') = char' ' ' input (' ',str')

whitespaces :: Reparse ()
whitespaces input = case input of
  "" -> whitespace ""
  _   -> ((whitespace >>> whitespaces') <|> succeeds) input
 where
  whitespaces' :: Reparse ()
  whitespaces' ""          = succeeds ""
  whitespaces' input@(_:_) = ((whitespace >>> whitespaces') <|> succeeds) input

succeeds :: Reparse a
succeeds input (_,str') = ("",input,str')

(<|>) :: Reparse a -> Reparse a -> Reparse a
(pA <|> pB) input (e,str') | noParse   = pB input (e,str')
                           | otherwise = pA input (e,str')
 where
  noParse = isEmpty (set2 pA input (e,str'))

(<<<) :: Reparse a -> Reparse () -> Reparse a
(pA <<< pB) input (e,str') = (pA <> pB) input ((e,()),str')


(>>>) :: Reparse () -> Reparse a -> Reparse a
(pA >>> pB) input (e,str') = (pA <> pB) input (((),e),str')


(<>) :: Reparse a -> Reparse b -> Reparse (a,b)
(pA <> pB) input pair@((e1,e2),str') = case input of
  "" -> (res1,str2,str1' ++ str2')
  _  -> -- if null str1 && (res2,str',str2') /= succeeds str1 (e2,str1')
        --   then failed
        --   else 
          (res1,str2,str1' ++ str2')
 where
  (res1,str1,str1') = pA input (e1,res2)
  (res2,str2,str2') = pB str1 (e2,str')
  -- (res3,str3,str3')       = pA input (e1,res4)
  -- (res4,str4,str4')       = pB str3 (e2,str3')

------- "+ 1" (Mult 3,"Plus 4")
------plus "+ 12348257" (Minus, "eferfg")

-- (<*>) :: Reparse (a -> b) -> Reparse a -> Reparse b
-- (pF <*> pA) str (

put :: Reparse a -> String -> (a,String) -> String
put reparse input pair = case reparse input pair of
  (res1,str1,str) -> res1 ++ if null str
                               then ""
                               else if str == str1
                                      then str1
                                      else str ++ str1

put' :: Reparse a -> String -> (a,String) -> String
put' reparse input pair = res1 ++ str
 where (res1,"",str) = reparse input pair


get :: Reparse a -> String -> (a,String)
get reparse input | put reparse input v == input = v
 where v free