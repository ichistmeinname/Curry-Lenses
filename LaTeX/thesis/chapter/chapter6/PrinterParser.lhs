\section{Case Study I - Bidirectional Printer-Parser}\label{sec:printerParser}
Printer and parser are well-studied fields in Computer Science and
take an important role in the design of programming languages. %
In order to prove a programming language's expressiveness, the
implementation of a printer or parser library is a very popular
example. %
Printer and parser are also represented in processes that include
reading and updating data from files. %
For example, measurements of experiments in the field of biology are
often tracked in CSV files. %
In order to analyse these results, we need to read the CSV data from
the files, manipulate them and write the results back to another
file. %
Parsers specify the format of the CSV data in hand and yield an
editable structure in the source language, whereas printers write the
results back in a valid format. %
This process leads to the observation that printer and parsers fulfil
certain round-tripping rules, which need to be considered. %
We want to print the resulting data in a valid format, that is, the
printed results can be reread again by the parser. %
This observation leads to the following equation for a data structure
|val :: a| and associated functions |parse_a| for parsing and
|print_a| for printing, which are parametrised over the specific data
structure. %

\begin{equation}\tag{Print-Parse}
  |parse_a (print_a val) = val|
\end{equation}

We might also consider the inverse round-tripping rule and demand that
if we parse a string and print it again, we get the original string as
result. %
The following equation postulates this round-tripping rule. %

\begin{equation}\tag{Parse-Print}
  |print_a (parse_a str) = str|
\end{equation}

In certain scenarios, the \emph{Parse-Print} rule is a desirable
requirement. %
For example, when we choose a format for the data that is unambiguous,
both round-tripping rules together form the requirements for a correct
implementation. %
|Show| and |Read| instances for data structures in Haskell also form
the identity this round-tripping rules. %
On the other hand, in many scenarios regarding data acquisition, users
handle data manipulation, update, and sometimes even input manually. %
Especially in the field of programming languages, users write the code
that the parser consumes, whereas generated code may be part of many
programs. %
Therefore, most programming languages allow redundancies like spaces
and braces, so that a parsed data structure corresponds to more than
one string representation. \\%

In the following, we present different approaches to combine the
definition of printer and parser into one function. %
We can achieve this combination in the setting of lenses: we define a
lens for printing in the put and parsing in get direction. %
In addition, we provide a handfull of combinators to simplify the
definition of such \emph{printer-parser} for arbitrary data
structures. %

All approaches have the same interface of combinators, and define a
lens of type |Lens String (a,String)|. %
The underlying lens implementation is the put-based lens library
presented in Section \ref{sec:implPut}. %
We provide the following set of combinators, where |X| stands for the
used data structure that varies with the different implementations: %

\begin{itemize}
\item |(<>) :: X a -> X b -> X (a,b)|
\item |(<||>) :: X a -> X a -> X a|
\item |digit :: X Int|
\item |charP :: (Char -> Bool) -> X Char|
\item |many :: X a -> X [a]|
\item |whitespace :: X ()|
\end{itemize}

As the running example for each implementation we use a standard
example in the context of parsers as well as printers: arithmetic
expressions. %
Therefor, we give the definition of a algebraic datatype for
arithmetic expressions first. %

\begin{spec}
data Expr     = BinOp Op Expr Expr
              | Num Int
data Op       = Plus | Mult | Div | Minus
\end{spec}

\subsection{Printer-Parser}
The first implementation that we present is based on a lens definition
that pretty-prints a data structure in the put direction, and parses a
string into that data structure in the get direction. %

The lens has the form |type PPrinter a = Lens String (a,String)|. %
Remember, the extended type signature of |PPrinter a| corresponds to
|String -> (a,String) -> String|. %
The first argument is the input string, the second argument is a pair
consisting of the actual data type that we want to print and parse,
and a remaining string. %
This representation\footnote{a slightly different one, but still
  equivalent} is most commonly used for pretty-printers to achieve a
linear run-time subject to the resulting pretty-printed string. %
With this lens definition at hand, we can define two functions |pParse
:: PPrinter a -> String -> a| and |pPrint :: PPrinter a -> a ->
String|.\footnote{Note: the additional \emph{"p"} stands for \emph{"pretty"}.} %
Both functions are parametrised over a printer-parser lens, they run
this lens with the given string in the get direction and with the
given data structure in put direction, respectively. %
% We postpone the definitions of these functions, and give the
% implementation of the provided combinators first. %

\subsubsection*{Primitives}
As first primitive printer-parser, we define |digit :: PP Int| that
pretty-prints a digit in the \emph{put}- and parses a digit in the
|get|-direction. %

\begin{spec}
digit :: PPrinter Int
digit _ (d,str') | d <= 9 && d >= 0 = show d ++ str'
\end{spec}

We ignore the given string and replace it with the representation of
the given digit and adding the remaining string at the end of the
resulting string. %
Since the type of the data is not restricted to digits only, we
include a check if the range of the given |Int| value is between |0|
and |9|. %

The primitive |charP| prints and parses only characters that fulfil
a given predicate. %
We can easily define a combinator that prints and parses a whitespace
character by means of |charP|. %

\begin{spec}
charP :: (Char -> Bool) -> PPrinter Char
charP p _ (c,str') | p c = c : str'

whitespace' :: PPrinter Char
whitespace' = charP (== ' ') 
\end{spec}

In order to define more meaningful printer-parsers, e.g., for the
arithmetic expression we introduced earlier, we need combinators to
compose primitives. %
The first composition combinator builds a pair from two given
printer-parser. %

\begin{spec}
(<>) :: PPrinter a -> PPrinter b -> PPrinter (a,b)
(pA <> pB) str ((expr1,expr2),str') = pA str (expr1, newString)
 where
  newString = pB str (expr2,str')
\end{spec}

In order to get a better understanding of this composition, we expend
the type of the result, i.e., |PPrinter (a,b)| becomes |String ->
(a,b) -> String)|. %
This observation leads to the four given arguments: the two
printer-parsers, the given string and a pair of data structures
coupled with the remaining string. %
We can compose these two printer-parsers to achieve a meaningful
consecutive execution. %
First, we apply the second printer-parser |pB| to produce a new
string. %
The resulting string, on the other hand, is used as remaining string
in the application of the other printer-parser |pA|. %
This construction works straightforward, because the given string is
just replaced with the pretty-printed result in the definition of the
primitives. %

% Furthermore, we provide a combinator to pretty-print one of two
% alternatives. %
% That is, we have two printers-parser in hand and run the second one
% only, if the first one yields an empty string as result. %
% \begin{spec}
% (<|>) :: PPrinter a -> PPrinter a -> PPrinter a
% (pA1 <|> pA2) str pair = case pA1 str pair of
%                               []   -> pA2 str pair
%                               str' -> str'
% \end{spec}

% With the alternative combinator, we can easily define a function to
% run a printer-parser several times on a given input. %
% In the case of |many|, the given printer-parser can be applied several
% times or not at all. %

% \begin{spec}

% \end{spec}

\phantomsection

\subsubsection*{Parsing and Printing Arithmetic Expressions in Prefix Notation}\label{subsec:ppPrefix}

Next, we define the appropriate printer-parsers for the data structure
of arithmetic expressions given above. %
In order to define a printer-parser for our data structure |Expr|, we
need to cover both representations of the definition. %
An expression can be a number or a composition of two expressions with
a binary operator. %
For the purpose of simplicity, we implement the pretty-print of an
arithmetic expression in prefix notation at first. %

\begin{spec}
ppExpr :: PPrinter Expr
ppExpr str (Num v,str')          = digit str (v,str')
ppExpr str (BinOp op e1 e2,str') =
  ((ppOp <> ppExpr) <> ppExpr) str (((op,e1),e2),str')
\end{spec}

In the case of a given |Num|-constructor, we use the primitive |digit|
and, thus, limiting the identifiers of the arithmetic expressions to
|Integer| values between |0| and |9|.\footnote{We make this limitation
  due to simplicity reasons, we could easily write a version that
  prints an |Integer| value instead.} %
The rule for a binary operator looks a bit more complicated. %
However, the rule follows naturally from the definition of the data
type. %
At first, we print the given operator followed by both of its
arguments. %
The arguments of a binary operator are arithmetic expressions,
thus, we print the arguments in a recursive manner. %
Due the two consecutive uses of the composition combinator, |<>|, the
data has to be provided in form of a nested pair. %
The inner pair consists of the operator and the first arithmetic
expression. %
Then, this pair is injected as the first component of the outer pair
because of the second usage of |<>|. %
The part of the second component of this outer is assigned to the
second argument of the binary operator. %
In order to test this lens definition, we have to define the
printer-parser for operators of type |Op| first. %

\begin{spec}
ppOp :: PPrinter Op
ppOp _ (Plus,str') = "+" ++ str'
ppOp _ (Mult,str') = "*" ++ str'
\end{spec}

For the purpose of simplicity, we limit our definition to two
operators: |Plus| and |Mult|. %
Obviously, we use |"+"| and |"*"| a string representatives for an
addition and multiplication operator, respectively. %

With these handy lens definitions in hand, we can use the dedicated
function |pPrint| to see the pretty-printed version of an exemplary
arithmetic expression. %

\begin{spec}
> pPrint ppExpr (Num 3)
"3"

> pPrint ppExpr (BinOp Plus (Num 2) (Num 3))
"+23"
\end{spec}

Unfortunately, we were a bit careless when we defined the string
representation of an expression with a binary operator. %
The resulting string is not \emph{pretty} at all. %
The upside is that the parser already works very well. %
In the following examples, we use |pParse| to reconstruct the
expression from the string representation. %

\begin{spec}
> pParse ppExpr "+23"
(BinOp Plus (Num 2) (Num 3))

> pParse ppExpr "42"
Main: UserException "no complete parse"
Evaluation terminated with non-zero status 1
\end{spec}

In the first expression, we gain the original expression when parsing
|"+23"| as expected. %
Due to the restricted representation of numeric identifier as digits,
each digit of the string |"23"| is parsed as an argument of the binary
operator |Plus|. %
Because of this restriction, we cannot parse a numeric value like
|"42"|. %
Thus, the last expression |pParse ppExpr "42"| fails with the remark
that the given string did not parse completely. %
Without investing this failing expression in too much detail, we can
take a quick look at the parsing result. %
The type of |ppExpr| is |PPrinter Expr|, that is, the function
|ppExpr| is a lens function. %
For that reason, we can use |ppExpr| in get and put direction. %
As mentioned before, whereas the put direction corresponds to a
pretty-print, we can parse a string into a value of type |Expr| when
using the get direction of the lens definition. %

\begin{spec}
> get ppExpr "42"
(Num 4,"2")
\end{spec}

In this case, the expression |get ppExpr "42"| is of type
|(Expr,String)| and yields the parsed value and the remaining
string. %
As we will see later, the definition of |parse| distinguishes if the
resulting remaining string is the empty string or not. %
In case of a non-empty string, the parse could not be completed and
the function throws an exception. %


\subsubsection*{Pretty Arithmetic Expressions}

In order to continue on \emph{pretty}-printing arithmetic expression,
we have to rewrite the second case of our |ppExpr| definition. %
Obviously, a pretty representation of arithmetic expression consists
of whitespace between each token. %
For the purpose of adding whitespace to the string representation, we
integrate the function |whitespace'| into our definition of |ppExpr|
from above. %
In the end, we add a whitespace after the binary operator and the
first argument of that operator. %

\begin{spec}
ppExpr str (BinOp op e1 e2,str') =
  ((ppOp <> whitespace') <> (ppExpr <> whitespace')
                         <> ppExpr) str ((((op,_),(e1,_)),e2),str')
\end{spec}

Unfortunately, due to the integration of whitespace and the related
increased usage of the composition combinator, this definition looks
rather cumbersome and complicated. %
In particular, the whitespaces we add in the definition are ignored in
the data that is given in form of a deeply nested pair. %
The definition above uses a anonymous free variable, which is bound to
a whitespace character when actually calling this function. %
The variable is bound to a whitespace, because a whitespace is the
only valid value, such that the |whitespace'| function yields a
result. %
Instead of using anonymous free variable, we can explicitly assign
whitespace characters as arguments. %

In the case of an injected prettiness factor like additional
whitespaces, it would be convenient to provide a combinator to ignore
an printer-parser's result. %
Those additional adjustments regarding the printed string mostly occur
in the context of composing. %
Thus, we define two additional composition functions to ignore the
printer-parse to the right and to the left, respectively. %
We can implement such a combinator by means of |(<>)|. %

\begin{spec}
(<<<) :: PPrinter a -> PPrinter () -> PPrinter a
(pA <<< pB) str (expr,str') = (pA <> pB) str ((expr,()),str')

(>>>) :: PPrinter () -> PPrinter b -> PPrinter b
(pA >>> pB) str (expr,str') = (pA <> pB) str (((),expr),str')
\end{spec}

Unfortunately, we need to restrict the type of the ignored result to
the Unit type. %
We have to make up a value to pass to |(<>)| as first and second
argument, respectively. %
Since we cannot always devise a suitable value for any type, we
restrict these combinators to use Unit only. %
The Unit type has only one valid value, thus, we can always pass along
|()| as argument. %

In order to use these combinators in our definition of |ppExpr|, we
have to implement a whitespace printer-parser of type |PPrinter ()|. %

\begin{spec}
whitespace :: PPrinter () whitespace _ ((),str') = " " + str'
\end{spec}

Fortunately, the definition is straightforward: we ignore the given
string and pattern match on the unit value in order to produce a
whitespace in front of the remaining string, which is given as part of
the input pair. %

Last but not least, we can integrate the newly defined functions in
our definition of |ppExpr| to enhance the readability. %

\begin{spec}
ppExpr str (BinOp op e1 e2,str') =
  ((ppOp <<< whitespaces) <> (ppExpr <<< whitespace)
                           <> ppExpr) str (((op,e1),e2),str')
\end{spec}

In order to assure that our reimplementation produces prettier string
representations, we run our examples from above, again. %
Furthermore, the string representation are supposed to be parseable as
well. %
Thus, we also give some examples for parsing valid string
representations of arithmetic expressions as well as some cases, where
our parses does not yield a completely parsed string. %

\begin{spec}
> pPrint ppExpr (BinOp Plus (Num 2) (Num 3))
"+ 2 3"

> pParse ppExpr "+ 2 3"
BinOp Plus (Num 2) (Num 3)

> pParse ppExpr "+ + 2 3 4"
BinOp Plus (BinOp Plus (Num 2) (Num 3)) (Num 4)

> get ppExpr "+ 2 34"
(BinOp Plus (Num 2) (Num 3),"4")

> get ppExpr2 "+ 23 4"
-- no result
\end{spec}

The second and third example shows a successful parse for a simple and
a nested arithmetic expression, respectively. %
Next, we reuse the example from above that includes a
number with two digits as second argument for |Plus|, thus, leading to a
remaining string. %
Note, the parse does not fail completely: the prefix |"+ 2 3"| is
parsed correctly and yields |BinOp Plus (Num 2) (Num 3)| as resulting
expression. %
In contrast, the last example fails completely and has no result. %
The given string |"+ 23 4"| does not consist of a valid prefix to
yield a partial result with a remaining string. %

\subsubsection*{Definition of |pParse| and |pPrint|}
\todo{Should I give the definition of |pParse| and |pPrint|?}
% \begin{spec}
% pParse :: PPrinter a -> String -> a
% pParse pp str = maybe err fst (find ((== "") . snd) values)
%  where
%   values = getND pp str
%   err    = error "no complete parse"

% print :: PPrinter a -> a -> String
% print pp val = pp "" (val,"")
% \end{spec}

\subsubsection*{The Downside}
As a main disadvantage of this approach, we cannot ignore redundant
parts in the parsing direction. %
If these redundancies do not appear in the pretty-printer definition,
we do note have the option to parse them anyway. %
We have already mentioned the original precedent in the introduction
of this section: optional whitespaces as delimiter between tokens. %
In order to make this case more clear, we define a printer-parser for
one or more whitespaces. %

\begin{spec}
whitespaces1' :: PPrinter [()]
whitespaces1' str (x:xs,str') =
   (whitespace <> whitespaces') str ((x,xs),str')
  where
   whitespaces' str ([],str)     = str'
   whitespaces' str (x:xs,str')  =
     whitespace str (x,"") ++ whitespaces' str (x,str')
\end{spec}

The function |whitespaces1'| pretty-prints a series of whitespace
depending on the length of the given list, which cannot be empty. %
In contrast, the auxiliary function |whitespaces'| can pretty-print
zero or many whitespaces. %
For the parsing direction, we want both functions to parse a series of
whitespaces. %
A generalisation of these functions comes in handy to pretty-print a
list of elements and to parse a series of characters or strings of the
same category, respectively. %
For that purpose, we define a generalised version |many1| and |many|
that takes a printer-parse as argument and applies it to each element
of a given list. %

\begin{spec}
many :: PPrinter a -> PPrinter [a]
many _ _ ([],str')   = str'
many pp str (x:xs,str') = (pp <> many pp) str ((x,xs),str')

many1 :: PPrinter a -> PPrinter [a]
many1 pp str (x:xs,str') = (pp <> many pp) str ((x,xs),str')
\end{spec}

With this definition at hand, we can define a modified version of
|whitespaces|. %

\begin{spec}
whitespaces :: PPrinter [()]
whitespaces = many1 whitespace
\end{spec}

Next, we integrate the additional trailing whitespaces into our
printer-parser for arithmetic expression. %
This integration implicates to change the usage of |<<<| and |>>>| to
the traditional composition operator again. %
We can only ignore data of type |Unit|, but |whitespaces| expects a
list of |Unit|. %

\begin{spec}
ppExprSpaces :: PPrinter Expr
ppExprSpaces str (BinOp op e1 e2,str') =
  ((ppOp <> whitespaces) <> (ppExprSpaces <> whitespaces)
                         <> ppExprSpaces) str (((opSpaces,e1Spaces),e2),str')
 where
  opSpaces = (op,_)
  e1Spaces = (e1,_)
ppExprSpaces str (Num v,str')          = digit str (v,str')
\end{spec}

Since we do not care about the number of trailing whitespaces after an
operator and its expressions, respectively, we use an anonymus free
variable as input data. %
This free variable can be bound to any number of |Unit| elements in
order to parse all occurring whitespaces. %
As a first example, we can parse trailing whitespaces after the binary
operator as well as a pretty-printed version. %

\begin{spec}
> pParse ppExprSpaces "+   1 2"
(BinOp Plus (Num 1) (Num 2),"")

> pParse ppExprSpaces "+ 3 4"
(BinOp Plus (Num 1) (Num 2),"")
\end{spec}

Fortunately, the integration of redundant whitespaces works like a
charm. %
However, there must be a downside of this implementation, otherwise we
would not discuss it under the used subtitle. %
In the beginning, we said that we cannot parse redundancies that are
not included in the printer-parser's definition. %
In our case, we have added these redundancies and, thus, can parse
them in a convenient way. %
This observation leads to the question: how does this integration
effect the pretty-print of the arithmetic expression?
In the pretty-printed version of an arithmetic expression, we do not
allow any leading and trailing whitespaces. %
Let us test the behaviour by pretty-printing the expression from
above. %

\begin{spec}
> pPrint ppExprSpaces (BinOp Plus (Num 1) (Num 2)
"+ 1 2"
"+ 1  2"
"+  1 2"
"+ 1   2"
"+  1  2"
"+   1 2"
"+ 1    2"
"+  1   2"
"+   1  2"
"+    1 2"
...
\end{spec}

Unfortunately, this expression does not terminate, but yields all
possible versions of string representatives. %
Possible versions include a different number of trailing whitespace
after the operator and the first argument of that operator. %
This unsatisfactory result arises from the use of the free variables
in the definition of |ppExprSpaces|. %
We cannot fix the number of used whitespaces to one, the fact is that
the free variable is instantiated nondeterministically to a suitable
value. %
In our case, |[()]| is not the only suitable value, any list of |Unit|
values fits the specification of the pretty-printer. \\%

Furthermore, the definitions for the pretty-printer have to be more
sophisticated than usual. %
The printer and parser definitions are connected, that is, typical
restrictions known from parser constructions need to be considered. %
In our example, we only defined a prefix based arithmetic expression
data structure, but with infix operators, we have to avoid
left-recursion in the definitions. %
This modification leads to a more complex definition of the
printer-parser that bears a resemblance to a typical parser for
arithmetic expression with infix operators. %
For the interested reader, we give the implementation for arithmetic
expressions with infix operators as well as some examples for
pretty-printing and parsing in Appendix \ref{a:ppInfix}. %

\subsection{Replace-Parser}

% \subsection{Parser-Printer}

\subsection{Similar Approaches}

\begin{itemize}
\item FlipPr \cite{flippr}
\item Invertible Syntax Description \cite{invertibleSyntax}
\item Arrows \cite{invertibleArrows}
\item unparsing/parsing \cite{parsing2}
\end{itemize}
