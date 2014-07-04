module ReplaceTest where

import Char (intToDigit, isDigit)
import Replace

----- arithmetic expressions

data Expr = BinOp Op Expr Expr
          | Num Int
data Op = Plus | Minus | Mult | Div

-- prefix expressions
expr :: PReplace Expr
expr = expr' <|> paren expr'
 where
  expr' input (BinOp op e1 e2,str') =
    ((plusMinus <<< whitespaces)
      <> (expr <<< whitespaces)
      <> expr) input (((op,e1),e2),str')
  expr' input (Num d, str') = digit input (d,str')

-- infix expressions
infixExpr :: PReplace Expr
infixExpr str t@(e,_) = (case e of
  BinOp Plus  _ _ -> expr' <|> term
  BinOp Minus _ _ -> expr' <|> term
  _           -> term) str t
 where
  expr' str1 (BinOp op e1 e2,str1') =
    ((term <<< whitespaces)
      <> plusMinus
      <> (whitespaces >>> infixExpr)) str1 (((e1,op),e2),str1')

term :: PReplace Expr
term str f@(e,_) = (case e of
  BinOp Mult _ _ -> term' <|> factor
  BinOp Div  _ _ -> term' <|> factor
  _ -> factor) str f
 where
  term' str1 (BinOp op e1 e2, str1') =
   ((factor <<< whitespaces)
        <> multDiv
        <> (whitespaces >>> term)) str1 (((e1,op),e2),str1')

factor :: PReplace Expr
factor str f@(e,_) = (case e of
  Num _ -> factor' <|> paren factor'
  _     -> paren infixExpr) str f
 where
  factor' str1 (Num v,str1') = digit str1 (v,str1')

plusMinus :: PReplace Op
plusMinus str (op,str') = charP (`elem` ['+','-']) str (opStr,str')
 where
  opStr = case op of
               Plus  -> '+'
               Minus -> '-'

multDiv :: PReplace Op
multDiv  str (op,str') = charP (`elem` ['*','/']) str (opStr,str')
 where
  opStr = case op of
               Mult -> '*'
               Div  -> '/'


----- test cases

put1 = (1, e, e == "(1 + 3) * 2")
 where e = put (replaceParse infixExpr) "" (BinOp Mult (BinOp Plus (Num 1) (Num 3)) (Num 2),"")

put2 = (2, e, e == "(1 + 3) * 2 TEST")
 where e = put (replaceParse infixExpr) "" (BinOp Mult (BinOp Plus (Num 1) (Num 3)) (Num 2)," TEST")

put3 = (3, e, e == "2 * 3")
 where e = put (replaceParse infixExpr) "1 * 4" (BinOp Mult (Num 2) (Num 3),"")

put4 = (4, e, e == "(1 + 3) * 2")
 where e = put (replaceParse infixExpr) "(1 - 3) * 1" (BinOp Mult (BinOp Plus (Num 1) (Num 3)) (Num 2),"")

put5 = (5, e, e == "(1 / 4)")
 where e = put (replaceParse infixExpr) "(1 * 4)" (BinOp Div (Num 1) (Num 4),"")

put6 = (6, e, e == "(1  /  3)")
 where e = put (replaceParse infixExpr) "(1  *  5)" (BinOp Div (Num 1) (Num 3),"")

put7 = (7, e, e == "1  /  3")
 where e = put (replaceParse infixExpr) "1  *  5" (BinOp Div (Num 1) (Num 3),"")


putTests = [put1,put2,put3,put4,put5,put6,put7]

test ts = map check ts
 where
  check (n, _, True ) = (n,"POSITIVE")
  check (n, e, False) = (n,e)

get1 = (1, e, e == (BinOp Plus (Num 1) (Num 2),"") || e == (Num 1," + 2"))
 where e = get (replaceParse infixExpr) "1 + 2"

-- does not terminate for Replace
get2 = (2, e, e == (BinOp Plus (Num 1) (Num 2),""))
 where e = get (replaceParse infixExpr) "(1 + 2)"

get3 = (3, e, e == (BinOp Plus (Num 1) (Num 2),"")
           || e == (Num 1, "   +  2"))
 where e = get (replaceParse infixExpr) "1   +  2"

get4 = (4, e, e == (BinOp Plus (Num 1) (Num 2)," TEST")
           || e == (Num 1," + 2 TEST"))
 where e = get (replaceParse infixExpr) "1 + 2 TEST"


-- optional1 = (1, e, e == "(5)")
--  where e = put (replaceParse (optional whitespace (paren digit))) "(3)" (((),5),"")

-- optional2 = (2, e, e == " (5)")
--  where e = put (replaceParse (optional whitespace (paren digit))) " (3)" (((),5),"")

-- optional3 = (3, e, e == "(5)")
--  where e = put (replaceParse (optional whitespace (paren digit))) "" (((),5),"")