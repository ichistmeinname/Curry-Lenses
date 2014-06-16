module PP where

import Lens
import Char (isAlpha, isAlphaNum, isSpace, isDigit, intToDigit)
import List (isSuffixOf,intercalate)
import Maybe
import Sort
import SetFunctions

-- type PPrinter = String -> (a,String) -> String
type PPrinter a = Lens String (a,String)
-- type Printer a = a -> String

pParse :: PPrinter a -> String -> a
pParse pp str = foldr (\ (expr',str') expr ->
    if null str' then expr' else expr) err values
 where
  values = getND pp str
  err    = error "no complete parse"

pPrint :: PPrinter a -> a -> String
pPrint pp val = pp "" (val,"")

----- data structure for arithmetic expressions

data Expr     = BinOp Op Expr Expr
              | Paren Expr
              | Num Int
data Op       = Plus | Mult | Div | Minus


----- combinators for printing
infixl 4 <>, <<<, >>>
infixl 3 <|>

(<>) :: PPrinter a -> PPrinter b -> PPrinter (a,b)
(pA <> pB) str ((expr1,expr2),str') = pA str (expr1, newString)
 where
  newString = pB str (expr2,str')

(<<<) :: PPrinter a -> PPrinter () -> PPrinter a
(pA <<< pB) str (expr,str') = (pA <> pB) str ((expr,()),str')

(>>>) :: PPrinter () -> PPrinter b -> PPrinter b
(pA >>> pB) str (expr,str') = (pA <> pB) str (((),expr),str')

(<|>) :: PPrinter a -> PPrinter a -> PPrinter a
(pA1 <|> pA2) str pair = case pA1 str pair of
                              []   -> pA2 str pair
                              str' -> str'


----- simple version of printer with prefix operators

ppExpr :: PPrinter Expr
ppExpr str (BinOp op e1 e2,str') =
  ((ppOp <<< ppWhitespaces) <> (ppExpr <<< ppWhitespace)
                           <> ppExpr) str (((op,e1),e2),str')
ppExpr str (Paren expr,str')     = "(" ++ ppExpr str (expr,")"++str')
ppExpr str (Num v,str')          = ppNum str (v,str')

ppOp :: PPrinter Op
ppOp _ (Plus,str') = "+" ++ str'
ppOp _ (Mult,str') = "*" ++ str'

ppNum :: PPrinter Int
ppNum _ (d,str') | d <= 9 && d >= 0 = show d ++ str'

ppWhitespace :: PPrinter ()
ppWhitespace _ ((),str') = " " ++ str'

ppWhitespaces :: PPrinter ()
ppWhitespaces str ((),str') = (ppWhitespace <|> ppWhitespaces) str ((),str')

ppPure :: PPrinter a
ppPure _ (_,str') = str'

----- better version of printer with infix operators

aBool :: Bool
aBool = v where v free

xor :: Bool -> Bool -> Bool
xor x y = not (x == y)

test :: () -> (Expr,String)
test _ = if put ppExpr' "4  + 4" v == "4  + 4" then v else failed
 where v free

ppExpr' :: PPrinter Expr
ppExpr' str t@(e,str') = case e of
  BinOp op e1 e2 ->
    ((ppTerm <<< ppWhitespaces)
      <> ppPlusMinus
      <> (ppWhitespaces >>> ppExpr')) str (((e1,op),e2),str')
  _                   -> ppTerm str t

ppTerm :: PPrinter Expr
ppTerm str (BinOp op e1 e2, str') =
  ((ppFactor <<< ppWhitespace)
   <> ppMultDiv
   <> (ppWhitespace >>> ppTerm)) str (((e1,op),e2),str')
ppTerm str f = ppFactor str f

ppFactor :: PPrinter Expr
ppFactor str (Paren expr,str') = "(" ++ ppExpr' str (expr,")"++str')
ppFactor str (Num v,str') = ppNum str (v,str')

ppMultDiv :: PPrinter Op
ppMultDiv _ (Mult,str') = "*" ++ str'
ppMultdiv _ (Div,str')  = "/" ++ str'

ppPlusMinus :: PPrinter Op
ppPlusMinus _ (Plus,str')  = "+" ++ str'
ppPlusMinus _ (Minus,str') = "-" ++ str'

-- does not work for printer (non-deterministic!)
ppMany :: PPrinter a -> PPrinter [a]
ppMany _ _ ([],str')   = str'
ppMany pp str (x:xs,str') = ppMany pp str (xs,pp str (x,str'))


----- Lenses with canonizers (Quotient Lenses)
type CC s v = (s -> v,v -> s)

ccGet :: CC t s -> Lens s v -> t -> v
ccGet (canon,_) lens t | put lens (canon t) v == (canon t) = v
 where v free

ccPut :: CC t s -> Lens s v -> s -> v -> t
ccPut (_,choose) lens s v = choose (lens s v)

ccWhitespace :: CC String String
ccWhitespace = (intercalate " " . words, id)