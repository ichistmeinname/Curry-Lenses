module PP where

import Lens
import Char (isAlpha, isAlphaNum, isSpace, isDigit)
import List (isSuffixOf)
import SetFunctions
import EasyCheck
import Maybe

type PParser a = (a,String) -> String -> (a,String)
-- type Printer a = a -> String

type PPrinter a = Lens String (a,String)

parse :: PPrinter a -> String -> a
parse pp str = foldr (\ (expr',str') expr ->
    if null str' then expr' else expr) err values
 where
  values = getND pp str
  err    = error "no complete parse"

pPrint :: PPrinter a -> a -> String
pPrint pp val = pp "" (val,"")

-----
data Expr     = BinOp Op Expr Expr
              | Paren Expr
              | Num Int
data Op       = Plus | Mult | Div | Minus


showExpr :: Int -> PPrinter Expr
showExpr _ _ (Num n,str') = show n ++ str'
showExpr pred str (BinOp Plus e1 e2,str') = showParen (pred > 0)
  (showExpr pred <**> showOp <**> showExpr pred) str (((e1,Plus),e2),str')
showExpr pred str (BinOp Mult e1 e2,str') = showParen (pred > 0)
  (showExpr (pred' e1) <**> showOp <**> showExpr (pred' e2)) str (((e1,Mult),e2),str')
 where
  pred' e | isPlusOp e = pred + 1
          | otherwise  = pred

isPlusOp :: Expr -> Bool
isPlusOp expr = case expr of
                     BinOp Plus _ _ -> True
                     _              -> False

showParen :: Bool -> PPrinter a -> PPrinter a
showParen b pp str (v,str')
  | b         = "(" ++ pp str (v,str'++")")
  | otherwise = pp str (v,str')

showOp :: PPrinter Op
showOp _ (Plus,str')  = "+" ++ str'
showOp _ (Minus,str') = "-" ++ str'
showOp _ (Mult,str')  = "*" ++ str'
showOp _ (Div,str')   = "/" ++ str'


infixl 4 <**>, <**, **>
infixl 3 <||>

(<**>) :: PPrinter a -> PPrinter b -> PPrinter (a,b)
(pA <**> pB) str ((expr1,expr2),str') = pA str (expr1, newString)
 where
  newString = pB str (expr2,str')

(<**) :: PPrinter a -> PPrinter () -> PPrinter a
(pA <** pB) str (expr,str') = (pA <**> pB) str ((expr,()),str')

(**>) :: PPrinter () -> PPrinter b -> PPrinter b
(pA **> pB) str (expr,str') = (pA <**> pB) str (((),expr),str')

(<||>) :: PPrinter a -> PPrinter a -> PPrinter a
(pA1 <||> pA2) str pair = case pA1 str pair of
                               []   -> pA2 str pair
                               str' -> str'

ppExpr :: PPrinter Expr
ppExpr str (BinOp op e1 e2,str') =
  ((ppOp <** ppWhitespace) <**> (ppExpr <** ppWhitespace)
                           <**> ppExpr) str (((op,e1),e2),str')
ppExpr str (Paren expr,str')     = "(" ++ ppExpr str (expr,")"++str')
ppExpr str (Num v,str')          = ppNum str (v,str')

ppExpr' :: PPrinter Expr
ppExpr' str b@(BinOp op e1 e2,str') =
  ((ppTerm <** ppWhitespace)
   <**> ppPlusMinus
   <**> (ppWhitespace **> ppExpr')) str (((e1,op),e2),str')
ppExpr' str t = ppTerm str t

ppTerm :: PPrinter Expr
ppTerm str (BinOp op e1 e2, str') =
  ((ppFactor <** ppWhitespace)
   <**> ppMultDiv
   <**> (ppWhitespace **> ppTerm)) str (((e1,op),e2),str')
ppTerm str f = ppFactor str f

ppFactor :: PPrinter Expr
ppFactor str (Paren expr,str') = "(" ++ ppExpr' str (expr,")"++str')
ppFactor str (Num v,str') = ppNum str (v,str')

ppMultDiv :: PPrinter Op
ppMultDiv str (Mult,str') = "*" ++ str'
ppMultdiv str (Div,str')  = "/" ++ str'

ppPlusMinus :: PPrinter Op
ppPlusMinus str (Plus,str')  = "+" ++ str'
ppPlusMinus str (Minus,str') = "-" ++ str'

ppOp :: PPrinter Op
ppOp _ (Plus,str') = "+" ++ str'
ppOp _ (Mult,str') = "*" ++ str'

ppOp' :: PPrinter Op
ppOp' str (Plus,str') | "+" ++ str' == str = "+" ++ str'
                      | "*" ++ str' == str = "+" ++ str'
ppNum :: PPrinter Int
ppNum _ (d,str') | d <= 9 && d >= 0 = show d ++ str'

ppWhitespace :: PPrinter ()
ppWhitespace _ ((),str') = " " ++ str'

ppWhitespaces :: PPrinter [()]
ppWhitespaces = ppMany ppWhitespace

ppMany :: PPrinter a -> PPrinter [a]
ppMany pp str ([],str')   = str'
ppMany pp str (x:xs,str') = ppMany pp str (xs,pp str (x,str'))


ppExpr'' :: PParser Expr
ppExpr'' (_,str') (d:str)
  | isDigit d = (case d of
                     '1' -> Num 1
                     '2' -> Num 2
                     '3' -> Num 3,str ++ str')
ppExpr'' (b,str') str =
  (BinOp op e1 e2,strR3 ++ str')
 where
   op', e1', e2' free
   (e1,strR1) = ppExpr'' (e1',"") str
   (op,strR2) = ppOp'' (op',"") strR1
   (e2,strR3) = ppExpr'' (e2',"") strR2

ppOp'' :: PParser Op
ppOp'' (_,str') ('+':str) = (Plus,str ++ str')
ppOp'' (_,str') ('*':str) = (Mult,str ++ str')

-- PParser a  == (a,String) -> String -> (a,String)
-- ppBool :: PP Bool
-- ppBool str ((b,str'):rs) = show b ++ str' ? ppBool str rs

-- ppExpr _ str = case pExpr str of
-- type Lens a b = a -> b -> a
