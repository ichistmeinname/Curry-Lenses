module PP where

import Lens
import Char (isAlpha, isAlphaNum, isSpace, isDigit, intToDigit)
import List (isSuffixOf,intercalate,find)
import Maybe
import Sort
import SetFunctions

-- type PPrinter = String -> (a,String) -> String
type PPrinter a = Lens String (a,String)
-- type Printer a = a -> String

pParse :: PPrinter a -> String -> a
pParse pp str = maybe err fst $ find ((== "") . snd) values
 where
  values = getND pp str
  err    = error "no complete parse"

pPrint :: PPrinter a -> a -> String
pPrint pp val = pp "" (val,"")

----- data structure for arithmetic expressions

data Expr     = BinOp Op Expr Expr
              | Num Int
data Op       = Plus | Mult | Div | Minus

-- (BinOp Plus (BinOp Plus (Num 2) (Num 3)) (Num 4))

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
(pA1 <|> pA2) str pair = case isEmpty (set2 pA1 str pair) of
                              True   -> pA2 str pair
                              False  -> pA1 str pair

char :: PPrinter Char
char _ (c,str') = c : str'

charP :: (Char -> Bool) -> PPrinter Char
charP p _ (c,str') | p c = c : str'

digit :: PPrinter Int
digit _ (d,str') | d <= 9 && d >= 0 = show d ++ str'

-- does not work with free variable for printer (non-deterministic!)
many :: PPrinter a -> PPrinter [a]
many _ _ ([],str')   = str'
many pp str (x:xs,str') = (pp <> many pp) str ((x,xs),str')

many1 :: PPrinter a -> PPrinter [a]
many1 pp str (x:xs,str') = (pp <> many pp) str ((x,xs),str')

whitespace :: PPrinter ()
whitespace str ((),str') = charP (== ' ') str (' ',str')

-- does not work for parsing
whitespaces' :: PPrinter ()
whitespaces' str ((),str') = (whitespace <|> whitespaces') str ((),str')

whitespaces :: PPrinter [()]
whitespaces = many1 whitespace


-- alternative version for replace-parser is not applicable,
--   because the input is never acutally consumed, thus,
--   the first rule of `whitespaces2'` is never reached
whitespaces2 :: PPrinter ()
whitespaces2 input = case input of
  "" -> whitespace ""
  _  -> (whitespace >>> whitespaces2') input
 where
  whitespaces2' ""           ((),str') = complete "" ((),str')
  whitespaces2' input'@(_:_) ((),str') =
    (whitespace >>> whitespaces2') input' ((),str')

pure :: PPrinter a
pure _ (_,str') = str'

complete :: PPrinter a
complete "" (_,str') = str'


----- simple version of printer with prefix operators
ppExpr :: PPrinter Expr
ppExpr str (BinOp op e1 e2,str') =
  ((ppOp <<< whitespace) <> (ppExpr <<< whitespace)
                         <> ppExpr) str (((op,e1),e2),str')
ppExpr str (Num v,str')          = digit str (v,str')

ppOp :: PPrinter Op
ppOp str (op,str') = charP isOp str (fromJust opStr,str')
 where
  opStr = lookup op [(Plus,'+'),(Minus,'-'),(Mult,'*'),(Div,'/')]

isOp :: Char -> Bool
isOp c = any (== c) "+*-/"

----- prefix operators with redundant whitespaces
-- does not terminate in put-drection because of heavily use of nondeterminism
ppExprSpaces :: PPrinter Expr
ppExprSpaces str (BinOp op e1 e2,str') =
  ((ppOp <> whitespaces) <> (ppExprSpaces <> whitespaces)
                         <> ppExprSpaces) str (((opSpaces,e1Spaces),e2),str')
 where
  opSpaces = (op,_)
  e1Spaces = (e1,_)
ppExprSpaces str (Num v,str')          = digit str (v,str')


----- better version of printer with infix operators
ppExpr' :: PPrinter Expr
ppExpr' str t@(BinOp op e1 e2,str')
  | op == Plus || op == Minus =
      ((ppTerm <<< whitespace)
        <> ppPlusMinus
        <> (whitespace >>> ppExpr')) str (((e1,op),e2),str')
  | otherwise                 = ppTerm str t
ppExpr' str t@(Num _,_)       = ppTerm str t

ppTerm :: PPrinter Expr
ppTerm str f@(BinOp op e1 e2, str')
  | op == Mult || op == Div =
    ((ppFactor <<< whitespace)
      <> ppMultDiv
      <> (whitespace >>> ppTerm)) str (((e1,op),e2),str')
  | otherwise               = ppFactor str f
ppTerm str f@(Num _,_)      = ppFactor str f

ppFactor :: PPrinter Expr
ppFactor str (e,str') = case e of
  Num v -> digit str (v,str')
  _     -> "(" ++ ppExpr' str (e,")" ++ str')
     
ppMultDiv :: PPrinter Op
ppMultDiv _ (Mult,str') = "*" ++ str'
ppMultdiv _ (Div,str')  = "/" ++ str'

ppPlusMinus :: PPrinter Op
ppPlusMinus _ (Plus,str')  = "+" ++ str'
ppPlusMinus _ (Minus,str') = "-" ++ str'


----- Lenses with canonizers (Quotient Lenses)
type CC s v = (s -> v,v -> s)

ccGet :: CC t s -> Lens s v -> t -> v
ccGet (canon,_) lens t | put lens (canon t) v == (canon t) = v
 where v free

ccPut :: CC t s -> Lens s v -> s -> v -> t
ccPut (_,choose) lens s v = choose (lens s v)

ccWhitespace :: CC String String
ccWhitespace = (intercalate " " . words, id)