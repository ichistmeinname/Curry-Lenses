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
data Op       = Plus | Mult

type Parser a = String -> [(a,String)]
type P a = String -> [(a,String)] -> String

-- type P Bool = String -> (Bool,String) -> String

pBool :: P Bool
pBool str (b, str') | isSuffixOf str' str = show b ++ str'
pBool "testdoof" (True, "doof") = "Truedoof"

type Printer a = String -> a -> String

type LensParser a = Lens a String

-- ppExpr _ str = case pExpr str of
-- type Lens a b = a -> b -> a

infixl 4 <*>, <$>, <*, *>

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

pExpr :: Parser Expr
pExpr (c:str)
  | c == '+'  = ((\e1 e2 -> BinOp Plus e1 e2) <$>
                  (pWhitespace *> pExpr) <*> (pWhitespace *> pExpr)) str
  | isDigit c = pNum (c:str)
  | otherwise = []

pNum :: Parser Expr
pNum (n:str) = (\n -> [(Lit n, str)]) $
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