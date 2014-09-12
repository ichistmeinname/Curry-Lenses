import ReplaceParse
import Maybe

----- Arithmetic Expressions

data Expr = BinOp Op Expr Expr | Num Int
data Op   = Plus | Minus | Mult | Div

rpExpr :: PReplace Expr
rpExpr str (BinOp op e1 e2,str') =
  ((rpOp <<< whitespaces) <> (rpExpr <<< whitespaces)
                          <> rpExpr) str (((op,e1),e2),str')
rpExpr str (Num v,str')          = digit str (v,str')

rpOp :: PReplace Op
rpOp input (op,str') = charP isOp input (fromJust opStr,str')
 where
  opStr = lookup op [(Plus,'+'),(Minus,'-'),(Mult,'*'),(Div,'/')]

isOp :: Char -> Bool
isOp c = any (== c) "+*-/"