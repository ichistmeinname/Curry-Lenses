module PP where

import Lens
import Char (isAlpha, isAlphaNum, isSpace, isDigit)
import List (isSuffixOf)
import SetFunctions
import EasyCheck
import Maybe

type Parser a = String -> [(a,String)]
-- type Printer a = a -> String

type PP a = Lens String (a,String)

parse :: PP a -> String -> a
parse pp str = case get pp str of
                    (expr,"") -> expr
                    _         -> error "no parse"

pPrint :: PP a -> a -> String
pPrint pp val = pp "" (val,"")

-----
data Expr     = BinOp Op Expr Expr
              | Paren Expr
              | Num Int
              | Test
data Op       = Plus | Mult | Div | Minus

infixl 4 <**>, <**, **>
infixl 3 <||>

(<**>) :: PP a -> PP b -> PP (a,b)
(pA <**> pB) str ((expr1,expr2),str') = pA str (expr1, newString)
 where
  newString = pB str (expr2,str')

(<**) :: PP a -> PP () -> PP a
(pA <** pB) str (expr,str') = (pA <**> pB) str ((expr,()),str')

(**>) :: PP () -> PP b -> PP b
(pA **> pB) str (expr,str') = (pA <**> pB) str (((),expr),str')

(<||>) :: PP a -> PP a -> PP a
(pA1 <||> pA2) str pair = case pA1 str pair of
                               []   -> pA2 str pair
                               str' -> str'

ppExpr :: PP Expr
ppExpr str (BinOp op e1 e2,str') =
  ((ppOp <** ppWhitespace) <**> (ppExpr <** ppWhitespace)
                           <**> ppExpr) str (((op,e1),e2),str')
ppExpr str (Paren expr,str')     = "(" ++ ppExpr str (expr,")"++str')
ppExpr str (Num v,str')          = ppNum str (v,str')

ppExpr' :: PP Expr
ppExpr' str b@(BinOp op e1 e2,str') =
  ((ppTerm <** ppWhitespace)
   <**> ppPlusMinus
   <**> (ppWhitespace **> ppExpr')) str (((e1,op),e2),str')
ppExpr' str t = ppTerm str t

ppTerm :: PP Expr
ppTerm str (BinOp op e1 e2, str') =
  ((ppFactor <** ppWhitespace)
   <**> ppMultDiv
   <**> (ppWhitespace **> ppTerm)) str (((e1,op),e2),str')
ppTerm str f = ppFactor str f

ppFactor :: PP Expr
ppFactor str (Paren expr,str') = "(" ++ ppExpr' str (expr,")"++str')
ppFactor str (Num v,str') = ppNum str (v,str')

ppMultDiv :: PP Op
ppMultDiv str (Mult,str') = "*" ++ str'
ppMultdiv str (Div,str')  = "/" ++ str'

ppPlusMinus :: PP Op
ppPlusMinus str (Plus,str')  = "+" ++ str'
ppPlusMinus str (Minus,str') = "-" ++ str'

ppOp :: PP Op
ppOp str (Plus,str') = "+" ++ str'
ppOp str (Mult,str') = "*" ++ str'

ppOp' :: String -> (Op,String) -> String
ppOp' str (Plus,str') | "+" ++ str' == str = "+" ++ str'
                      | "*" ++ str' == str = "+" ++ str'
ppNum :: PP Int
ppNum _ (d,str') | d <= 9 && d >= 0 = show d ++ str'

ppWhitespace :: PP ()
ppWhitespace str ((),str') = " " ++ str'

-- ppExpr'' :: Expr -> String -> Expr
-- ppExp'' _ [v] | isDigit v = case v of
--                                  '1' -> Num 1
-- ppExpr'' b str = BinOp (ppOp'' b str1) (ppExpr

-- ppOp'' :: Op -> String -> Op
-- ppOp'' _ ('+') = Plus
-- ppOp'' _ ('*') = Mult

-- type PP a = String -> [(a,String)] -> String

-- ppBool :: PP Bool
-- ppBool str ((b,str'):rs) = show b ++ str' ? ppBool str rs

-- ppExpr _ str = case pExpr str of
-- type Lens a b = a -> b -> a


infixl 4 <*>, <$>, <*, *>
infixl 3 <|>

(<*>) :: Parser (a -> b) -> Parser a -> Parser b
(pF <*> pA) str = [ (f v, str'') | (f,str') <- pF str, (v,str'') <- pA str']

(<*) :: Parser a -> Parser b -> Parser a
pA <* pB = const <$> pA <*> pB

(*>) :: Parser a -> Parser b -> Parser b
pA *> pB = flip const <$> pA <*> pB

(<$>) :: (a -> b) -> Parser a -> Parser b
(f <$> pA) str = map (\ (v,str') -> (f v, str')) res
 where
  res = pA str

(<|>) :: Parser a -> Parser a -> Parser a
(pA1 <|>  pA2) str = case pA1 str of
                          []  -> pA2 str
                          res -> res

pure :: a -> Parser a
pure v = \str -> [(v,str)]

pExpr' :: Parser Expr
pExpr' = (\t op e -> BinOp op t e) <$> pTerm <*> pPlusMinus <*> pExpr' <|> pTerm
pTerm = (\f op t -> BinOp op f t) <$> pFactor <*> pMultDiv <*> pTerm <|> pFactor
pFactor = pTerminal '(' *> pExpr' <* pTerminal ')' <|> pNum

pPlusMinus :: Parser Op
pPlusMinus []                   = []
pPlusMinus (op:str) | op == '+' = [(Plus,"")]
                    | op == '-' = [(Minus,"")]
                    | otherwise = []

pMultDiv :: Parser Op
pMultDiv []                   = []
pMultDiv (op:str) | op == '*' = [(Mult,"")]
                  | op == '/' = [(Div,"")]
                  | otherwise = []

pTerminal :: Char -> Parser ()
pTerminal _ []                   = []
pTerminal c (c':str) | c == c'   = [((),str)]
                     | otherwise = []

pExpr :: Parser Expr
pExpr = pBinOp <|> pNum
 where
  pBinOp = ((\e1 op e2 -> BinOp Plus e1 e2) <$>
              (pExpr <* pWhitespace) <*> pOp <*> (pWhitespace *> pExpr))

pOp :: Parser Op
pOp [] = []
pOp (op:str) | op == '+' = [(Plus,str)]
             | op == '*' = [(Mult,str)]
             | otherwise = []

pNum :: Parser Expr
pNum [] = []
pNum (n:str) | isDigit n = (\n -> [(Num n, str)]) $
  case n of
       '0' -> 0
       '1' -> 1
       '2' -> 2
       '3' -> 3
       '4' -> 4
       '5' -> 5
       '6' -> 6
       '7' -> 7
       '8' -> 8
       '9' -> 9

pWhitespace :: Parser ()
pWhitespace ""       = []
pWhitespace (ws:str) | isSpace ws = [((),str)]
                     | otherwise  = []
many :: Parser a -> Parser [a]
many pA = some pA <|> pure []

some :: Parser a -> Parser [a]
some pA = (:) <$> pA <*> many pA