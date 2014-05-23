module PP where

import Lens
import Char (isAlpha, isAlphaNum, isSpace, isDigit)
import List (isSuffixOf)
-- import Parser

-- data Rel a b = Rel [(a,b)]

-- rel :: a -> b -> Rel a b -> Bool
-- rel x y (Rel rs) = (x,y) `elem` rs

-- addRel :: a -> b -> Rel a b -> Rel a b
-- addRel x y (Rel rs) = Rel ((x,y):rs)

-- type Maintainer a b = (((a,b) -> b),((a,b) -> a))

-- lensToMaintainer :: Lens a b -> Maintainer a b
-- lensToMaintainer lens = (left,right)
--  where
--   left  (x,y) = get lens x
--   right (x,y) = put lens x y

-----

data Expr     = BinOp Op Expr Expr
              | Paren Expr
              | Lit Int
data Op       = Plus | Mult | Div | Minus

type Parser a = String -> [(a,String)]
type Printer a = String -> a -> String

type PP a = Lens String (a,String)

-- type P Bool = String -> (Bool,String) -> String

ppBool :: PP Bool
-- pBool str (b,str') | str == show b ++ str' = show b ++ str'
                   -- | otherwise             = show b ++ str'
ppBool _ (b,str') = show b ++ str'

infixl 4 <**>

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
ppExpr str (Paren expr,str')     = "(" ++ ppExpr str (expr,"") ++ ")" ++ str'
ppExpr str (Lit v,str')          = ppNum str (v,str')

ppExpr' :: PP Expr
ppExpr' str b@(BinOp op e1 e2,str') =
  ((ppTerm <** ppWhitespace)
   <**> ppPlusMinus
   <**> (ppWhitespace **> ppExpr')) str (((e1,op),e2),str')
ppExpr' str t = ppTerm str t

ppTerm :: PP Expr
ppTerm str (BinOp op e1 e2, str') =
  (ppFactor <**> ppMultDiv <**> ppTerm) str (((e1,op),e2),str')
ppTerm str f = ppFactor str f

ppFactor :: PP Expr
ppFactor str (Paren expr,str') = "(" ++ ppExpr' str (expr,"") ++ ")"
ppFactor str (Lit v,str') = ppNum str (v,str')

ppMultDiv :: PP Op
ppMultDiv str (Mult,str') = "*" ++ str'
ppMultdiv str (Div,str')  = "/" ++ str'

ppPlusMinus :: PP Op
ppPlusMinus str (Plus,str')  = "+" ++ str'
ppPlusMinus str (Minus,str') = "-" ++ str'

ppOp :: PP Op
ppOp str (Plus,str') = "+" ++ str'
ppOp str (Mult,str') = "*" ++ str'

ppNum :: PP Int
ppNum _ (d,str') | d <= 9 && d >= 0 = show d ++ str'

ppWhitespace :: PP ()
ppWhitespace str ((),str') = " " ++ str'

-- type PP a = String -> [(a,String)] -> String

-- ppBool :: PP Bool
-- ppBool str ((b,str'):rs) = show b ++ str' ? ppBool str rs

-- ppExpr _ str = case pExpr str of
-- type Lens a b = a -> b -> a

--- TODO: Infix operator in pretty printing!
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
pNum (n:str) | isDigit n = (\n -> [(Lit n, str)]) $
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


-- type PP a = Lens String a

-- type PP2 a = Lens a String

-- ppExpr :: PP Expr
-- ppExpr str (STerm t) = ppTerm str t

-- ppTerm :: PP Term
-- ppTerm str (SFactor f) = ppFactor str f

-- ppFactor :: PP Factor
-- ppFactor str l@(Lit v) = show v
-- -- ppFactor str (Paren e) = "(" ++ ppExpr str e ++ ")"

-- ppE :: PP2 Expr
-- ppE e litStr = STerm $ SFactor $ Lit $ (read litStr :: Int)

-- expr1 = STerm (SFactor (Paren (STerm (SFactor (Lit 6)))))
-- expr2 = STerm (SFactor (Lit 6))

-- read :: String -> a
-- read str = selectValue $ set1 readTerm str