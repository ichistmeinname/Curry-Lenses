\chapter{Combinatorial Library: Examples for
  Lenses}\label{a:combExamples}

We give some exemplary lens definitions on user-defined data types
using the combinatorial library we implemented in Curry. %
The corresponding implementation can be found in Section
\ref{sec:implComb}. %
Most of the following lens definitions are straightforward. %
In the first example, we use the combinator |isoLens| to define a
mapping from our data type to a tuple representation. %
This tuple representation can be further processed with the
combinators of the library. %
On the hand, we have two examples that work directly on primitive data
types. %
In addition, these two examples include constraints for constructing a
value of the data structure. %
The lens definitions can be seen as smart constructors for these
algebraic data types. %

\section*{Person examples}
\numberson
\numbersright
\begin{code}
data Person = Person Name City
type First = String

person :: Lens Person (Name,City)
person = isoLens inn out
  where
   inn (n,c)        = Person n c
   out (Person n c) = (n,c)

nameOrAddress :: Lens Person String
nameOrAddress = nameLens ? addressLens

nameLens :: Lens Person First
nameLens = person <.> keepSnd

addressLens :: Lens Person Address
addressLens = person <.> keepFst
\end{code}


\section*{Temperature examples}
\numbersreset
\begin{spec}
data Temp = Temp Float

centigrade :: Lens Temp Float
centigrade = isoLens inn out
  where
   inn celsius = Temp (cToF celsius)
   out (Temp temp) = fToC temp
\end{spec}
\begin{spec}
cToF :: Float -> Float
cToF c = c *. 1.8 +. 32

fToC :: Float -> Float
fToC f = (f -. 32) *. (5/.9)
\end{spec}

\section*{Time examples}
\numbersreset
\begin{spec}
data Time = Time Int Int

time :: Lens Time Int
time = isoLens innT (\(Time hour min) -> hour * 60 + min)

mins :: Lens Time Int
mins = isoLens innT (\(Time _ min) -> min)

innT :: Int -> Time
innT m = Time (m `quot` 60) (m `mod` 60)
\end{spec}

\chapter{Printer-Parser for Arithmetic Expressions with Infix Notation}\label{a:ppInfix}

In addition to the presented printer-parser for arithmetic expression
in prefix notation in Section \ref{subsec:ppPrefix}, we define a more
complex and convenient printer-parser. %
The following code shows a printer-parser implementation for
arithmetic expression in infix notation. %
Because of the complication with redundant whitespaces in the parsing
direction, the implementation eschews this improvement. %
As a side-effect of integrating parsing techniques into the
printer-parser, the implementation needs to avoid left-recursions. %
Thus, the printer-parser follows the traditional technique for
implementing a parser for arithmetic expressions. %

\numbersreset
\begin{spec}
ppExpr' :: PPrinter Expr
ppExpr' str t@(BinOp op e1 e2,str')
  | op == Plus || op == Minus =
      ((ppTerm <<< whitespace)
        <> ppPlusMinus
        <> (whitespace >>> ppExpr')) str (((e1,op),e2),str')
  | otherwise                 = ppTerm str t
ppExpr' str t@(Num _,_)       = ppTerm str t
\end{spec}
\begin{spec}
ppTerm :: PPrinter Expr
ppTerm str f@(BinOp op e1 e2, str')
  | op == Mult || op == Div =
    ((ppFactor <* whitespace)
      <> ppMultDiv
      <> (whitespace *> ppTerm)) str (((e1,op),e2),str')
  | otherwise               = ppFactor str f
ppTerm str f@(Num _,_)      = ppFactor str f

ppFactor :: PPrinter Expr
ppFactor str f@(e,str') = case e of
  Num v       -> digit str (v,str')
  _           -> "(" ++ ppExpr' str (e,")" ++ str')
     
ppMultDiv :: PPrinter Op
ppMultDiv _ (Mult,str')  = "*" ++ str'
ppMultdiv _ (Div,str')   = "/" ++ str'

ppPlusMinus :: PPrinter Op
ppPlusMinus _ (Plus,str')   = "+" ++ str'
ppPlusMinus _ (Minus,str')  = "-" ++ str'
\end{spec}

In order to see the printer-parser in action, we give some exemplary
expressions for printing and parsing. %

\numbersoff
\begin{spec}
> pPrint ppExpr' (BinOp Mult (Num 1) (BinOp Mult (Num 2) (Num 3)))
1 * 2 * 3

> pPrint ppExpr' (BinOp Mult (Num 7) (BinOp Plus (Num 5) (Num 3)))
"7 * (5 + 3)"

> pPrint ppExpr' (BinOp Mult (Num 1) (BinOp Mult (BinOp Plus (Num 2)
(Num 3)) (BinOp Minus (Num 5) (Num 1))))
"1 * (2 + 3) * (5 - 1)"

> pParse ppExpr' "1 + 3 * 5"
(BinOp Plus (Num 1) (BinOp Mult (Num 3) (Num 5)))

> pParse ppExpr' "1 + 3 * (4 - 9)"
(BinOp Plus (Num 1) (BinOp Mult (Num 3) (BinOp Minus (Num 4) (Num 9))))
\end{spec}
