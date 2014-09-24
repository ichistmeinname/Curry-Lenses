\section{Case Study I - Bidirectional Printer-Parser}\label{sec:printerParser}
Pretty-Printers and parsers are well-studied fields in Computer Science and
take an important role in the design of programming languages. %
In order to prove a programming language's expressiveness, the
implementation of a printer or parser library is a very popular
example. %
Printers and parsers are also represented in processes that include
reading and updating data from files. %
For example, measurements of experiments in the field of biology are
often tracked in CSV files. %
In order to analyse these results, we need to read the CSV data from
the files, manipulate them and write the results back to another
file. %
Parsers specify the format of the CSV data in hand and yield an
editable structure in the source language, whereas printers write the
results back in a valid format. %
This process leads to the observation that printers and parsers fulfil
certain round-tripping rules, which need to be considered. %
We want to print the resulting data in a valid format, that is, the
printed results can be reread again by the parser. %
This observation leads to the following equation for a data structure
|value :: a| and associated functions |parse_a| for parsing and
|print_a| for printing, which are parametrised over the specific data
structure. %
%
\begin{align*}\tag{Print-Parse}
  |parse_a (print_a value) = value|
\end{align*}
%
We might also consider the inverse round-tripping rule and demand that
if we parse a string and print it again, we get the original string as
result. %
The following equation postulates this round-tripping rule. %
%
\begin{align*}\tag{Parse-Print}
  |print_a (parse_a str) = str|
\end{align*}
%
In certain scenarios, the \emph{Parse-Print} rule is a desirable
requirement. %
For example, if we choose a format for the data that is unambiguous,
both round-tripping rules together form the requirements for a correct
implementation. %
|Show| and |Read| instances for data structures in Haskell often form
these round-tripping rules as well. %
On the other hand, in many scenarios regarding data acquisition, users
handle data manipulation, update, and sometimes even input manually. %
Especially in the field of programming languages, users write the code
that the parser consumes, whereas generated code may be part of many
programs. %
Therefore, most programming languages allow redundancies like spaces
and parentheses, so that a parsed data structure corresponds to more than
one string representation. \\%

In the following, we present different approaches to combine the
definition of printers and parsers into one function. %
We can achieve this combination in the setting of lenses: we define a
lens for printing in the put and parsing in get direction. %
In addition, we provide a handful of combinators to simplify the
definition of such \emph{printer-parser} for arbitrary data
structures. %

All approaches have the same interface of combinators, and define a
lens of type |Lens String (a,String)|. %
The underlying lens implementation is the put-based lens library
presented in Section \ref{sec:implPut}. %
We provide the following set of combinators, where |X| stands for the
used type of the printer-parser that varies with the different
implementations: %

\begin{itemize}
\item |(<>) :: X a -> X b -> X (a,b)|
\item |(<||>) :: X a -> X a -> X a|
\item |digit :: X Int|
\item |charP :: (Char -> Bool) -> X Char|
% \item |many :: X a -> X [a]|
\item |space :: X ()|
\end{itemize}

As the running example for each implementation we use a standard
example in the context of parsers as well as printers: arithmetic
expressions. %
Therefore, we give the definition of an algebraic data type for
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

\begin{spec}
pParse :: PPrinter a -> String -> a
pParse pp str = maybe err fst (find ((== "") . snd) values)
 where
  values = getND pp str
  err    = error "no complete parse"

pPrint :: PPrinter a -> a -> String
pPrint pp val = pp "" (val,"")
\end{spec}

The definition of |pParse| first collects all results of the given
printer-parser running in the get direction by using |getND|. %
The auxiliary |getND| uses \emph{Set Functions} to yield a list of
results instead of nondeterministic results. %
In the end, |pParse| chooses the first result that has no remaining
string, i.e., a result with a complete parse, and yields the parsed
value. %
If there are no complete parses, we throw an error. %

The function |pPrint| applies the given printer-parser to the empty
string as first argument to trigger a pretty-print. %

\subsubsection*{Primitives}
As first primitive printer-parser, we define |digit :: PPrinter Int| that
pretty-prints a digit in the \emph{put}- and parses a digit in the
|get|-direction. %

\begin{spec}
digit :: PPrinter Int
digit _ (d,str') | 0 <= d && d <= 9 = show d ++ str'
\end{spec}

We ignore the given string, replace it with the representation of
the given digit, and add the remaining string at the end of the
resulting string. %
Since the type of the data is not restricted to digits only, we
include a check if the range of the given |Int| value is between |0|
and |9|. %

The primitive |charP| prints and parses only characters that fulfil
a given predicate. %
We can easily define a combinator that prints and parses a space
character by means of |charP|. %

\begin{spec}
charP :: (Char -> Bool) -> PPrinter Char
charP p _ (c,str') | p c = c : str'

space' :: PPrinter Char
space' = charP (== ' ') 
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

In order to get a better understanding of this composition, we expand
the type of the result, i.e., |PPrinter (a,b)| becomes |String ->
((a,b),String) -> String|. %
This observation leads to the four given arguments: the two
printer-parsers, the given string and a pair of data structures
coupled with the remaining string. %
We can compose these two printer-parsers to achieve a meaningful
consecutive execution. %
First, we apply the second printer-parser |pB| to produce a new
string. %
Then, the resulting string is used as remaining string
in the application of the other printer-parser |pA|. %
This construction works straightforward, because the given string is
just replaced with the pretty-printed result in the definition of the
primitives. %

Furthermore, we provide a combinator to pretty-print one of two
alternatives. %
That is, we have two printer-parsers at hand and run the second one
only if the first one fails. %

\begin{spec}
(<|>) :: PPrinter a -> PPrinter a -> PPrinter a
(pA1 <|> pA2) str pair = case isEmpty (set2 pA1 str pair) of
                              True   -> pA2 str pair
                              False  -> pA1 str pair
\end{spec}

In this case, we use Set Functions to reason about a possibly failing
replace-parser. %
That is, if the first replace-parser does not yield any result when
applied to the appropriate arguments, i.e., the list of all results is
empty, we run the second one instead. %

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
need to cover both representations of the definition: an expression
can be a number or a composition of two expressions with a binary
operator. %
For the purpose of simplicity, we implement the pretty-print of an
arithmetic expression in prefix notation at first. %

\begin{spec}
ppExpr :: PPrinter Expr
ppExpr str (Num v,str')          = digit str (v,str')
ppExpr str (BinOp op e1 e2,str') =
  ((ppOp <> ppExpr) <> ppExpr) str (((op,e1),e2),str')
\end{spec}

In the case of a given |Num|-constructor, we use the primitive |digit|
and, thus, limit the identifiers of the arithmetic expressions to
|Int| values between |0| and |9|.\footnote{We make this limitation
  due to simplicity reasons, we could easily write a version that
  prints an |Int| value instead.} %
The rule for a binary operator looks a bit more complicated. %
However, the rule follows naturally from the definition of the data
type. %
At first, we print the given operator followed by both of its
arguments. %
The arguments of a binary operator are arithmetic expressions, thus,
we print the arguments in a recursive manner. %
Due the two consecutive uses of the composition combinator, |<>|, the
data have to be provided in form of a nested pair. %
The inner pair consists of the operator and the first arithmetic
expression. %
Then, this pair is injected as the first component of the outer pair
because of the second usage of |<>|. %
The part of the second component of this outer pair is assigned to the
second argument of the binary operator. %
In order to test this lens definition, we have to define the
printer-parser for operators of type |Op| first. %

\begin{spec}
ppOp :: PPrinter Op
ppOp str (op,str') = charP isOp str (fromJust opStr,str')
 where
  opStr = lookup op [(Plus,'+'),(Minus,'-'),(Mult,'*'),(Div,'/')]

isOp :: Char -> Bool
isOp c = c `elem` "+*-/"
\end{spec}

We use the obvious symbols as string representatives for the
arithmetic operators. %
Note, we use |fromJust|\footnote{
|fromJust :: Maybe a -> a|\\
|fromJust (Just v)  = v|\\
|fromJust Nothing  = error "Maybe.fromJust: Nothing"|
} from the \texttt{Maybe} library that unwraps
the |Just| constructor from the given value. %

With these lens definitions at hand, we can use the dedicated
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
Without investigating this failing expression in too much detail, we can
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
Remember, the definition of |pParse| distinguishes if the resulting
remaining string is the empty string or not. %
In case of a non-empty string, the parse could not be completed and
the function throws an exception. %


\subsubsection*{Pretty Arithmetic Expressions}

In order to continue on \emph{pretty}-printing arithmetic expression,
we have to rewrite the second case of our |ppExpr| definition. %
Obviously, a pretty representation of arithmetic expression needs a
space character between each token. %
For the purpose of adding space to the string representation, we
integrate the function |space'| into our definition of |ppExpr|
from above. %
In the end, we add a space after the binary operator and the
first argument of that operator. %

\begin{spec}
ppExpr str (BinOp op e1 e2,str') =
  ((ppOp <> space') <> (ppExpr <> space')
                         <> ppExpr) str ((((op,_),(e1,_)),e2),str')
\end{spec}

Unfortunately, due to the integration of space and the related
increased usage of the composition combinator, this definition looks
rather cumbersome and complicated. %
In particular, the space, which we add in the definition, are ignored in
the data that are given in form of a deeply nested pair. %
The definition above uses an anonymous free variable, which is bound to
a space character when actually calling this function. %
The variable is bound to a space, because a space is the
only valid value, such that the |space'| function yields a
result. %
Instead of using an anonymous free variable, we can explicitly assign
space characters as arguments. %

In the case of an injected prettiness factor like additional
spaces, it would be convenient to provide a combinator to ignore
a printer-parser's result. %
Those additional adjustments regarding the printed string mostly occur
in the context of composition. %
Thus, we define two additional composition functions to ignore the
printer-parser to the right and to the left, respectively. %
We can implement such a combinator by means of |(<>)|. %

\begin{spec}
(<*) :: PPrinter a -> PPrinter () -> PPrinter a
(pA <* pB) str (expr,str') = (pA <> pB) str ((expr,()),str')

(*>) :: PPrinter () -> PPrinter b -> PPrinter b
(pA *> pB) str (expr,str') = (pA <> pB) str (((),expr),str')
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
have to implement a space printer-parser of type |PPrinter ()|. %

\begin{spec}
space :: PPrinter ()
space _ ((),str') = " " + str'
\end{spec}

Fortunately, the definition is straightforward: we ignore the given
string and pattern match on the unit value in order to produce a
space in front of the remaining string, which is given as part of
the input pair. %

Last but not least, we can integrate the newly defined functions in
our definition of |ppExpr| to enhance the readability. %

\begin{spec}
ppExpr str (BinOp op e1 e2,str') =
  ((ppOp <* space) <> (ppExpr <* space)
                           <> ppExpr) str (((op,e1),e2),str')
\end{spec}

In order to assure that our reimplementation produces prettier string
representations, we run our examples from above again. %
Furthermore, the string representations are supposed to be parsable as
well. %
Thus, we also give some examples for parsing valid string
representations of arithmetic expressions as well as some cases, where
our parser does not yield a completely parsed string. %

\begin{spec}
> pPrint ppExpr (BinOp Plus (Num 2) (Num 3))
"+ 2 3"

> pParse ppExpr "+ 2 3"
BinOp Plus (Num 2) (Num 3)

> pParse ppExpr "+ + 2 3 4"
BinOp Plus (BinOp Plus (Num 2) (Num 3)) (Num 4)

> get ppExpr "+ 2 34"
(BinOp Plus (Num 2) (Num 3),"4")

> get ppExpr "+ 23 4"
-- no result
\end{spec}

The second and third example show a successful parse for a simple and
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

% \subsubsection*{Definition of |pParse| and |pPrint|}
% \todo{Should I give the definition of |pParse| and |pPrint|?}
% \begin{spec}
% pParse :: PPrinter a -> String -> a
% pParse pp str = maybe err fst (find ((== "") . snd) values)
%  where
%   values = getND pp str
%   err    = error "no complete parse"

% print :: PPrinter a -> a -> String
% print pp val = pp "" (val,"")
% \end{spec}

\subsubsection*{Multiple Spaces}
As a main disadvantage of this approach, we cannot ignore redundant
parts in the parsing direction. %
If these redundancies do not appear in the pretty-printer's definition,
we do not have the option to parse them anyway. %
We have already mentioned an original precedent in the introduction
of this section: optional spaces as delimiter between tokens. %
In order to make this case more clear, we define a printer-parser for
one or more spaces. %

\begin{spec}
spaces1' :: PPrinter [()]
spaces1' str (x:xs,str') =
   (space <> spaces') str ((x,xs),str')

spaces' str ([],str)     = str'
spaces' str (x:xs,str')  =
  space str (x,"") ++ spaces' str (x,str')
\end{spec}

The function |spaces1'| pretty-prints a series of space
depending on the length of the given list, which cannot be empty. %
In contrast, the auxiliary function |spaces'| can pretty-print
zero or many spaces. %
For the parsing direction, we want both functions to parse a series of
spaces. %
A generalisation of these functions comes in handy to pretty-print a
list of elements and to parse a series of characters or strings of the
same category, respectively. %
For that purpose, we define a generalised version |many1| and |many|
that takes a printer-parser as argument and applies it to each element
of a given list. %

\begin{spec}
many :: PPrinter a -> PPrinter [a]
many _ _ ([],str')   = str'
many pp str (x:xs,str') = (pp <> many pp) str ((x,xs),str')
\end{spec}
\begin{spec}
many1 :: PPrinter a -> PPrinter [a]
many1 pp str (x:xs,str') = (pp <> many pp) str ((x,xs),str')
\end{spec}

With this definition at hand, we can define a modified version of
|spaces|. %

\begin{spec}
spaces :: PPrinter [()]
spaces = many1 space
\end{spec}

Next, we integrate the additional trailing spaces into our
printer-parser for arithmetic expression. %
This integration implicates to change the usage of |(<*)| and |(*>)| to
the traditional composition operator again. %
We can only ignore data of type |Unit|, but |spaces| expects a
list of |Unit|. %

\begin{spec}
ppExprSpaces :: PPrinter Expr
ppExprSpaces str (BinOp op e1 e2,str')  =
  ((ppOp <> spaces) <> (ppExprSpaces <> spaces)
                         <> ppExprSpaces) str (((opSpaces,e1Spaces),e2),str')
  where
   opSpaces = (op,_)
   e1Spaces = (e1,_)
ppExprSpaces str (Num v,str')           = digit str (v,str')
\end{spec}

Since we do not care about the number of trailing spaces after an
operator and its expressions, respectively, we use an anonymous free
variable as input. %
This free variable can be bound to any number of |Unit| elements in
order to parse all occurring spaces. %
As a first example, we can parse trailing spaces after the binary
operator as well as a pretty-printed version. %

\begin{spec}
> pParse ppExprSpaces "+   1 2"
(BinOp Plus (Num 1) (Num 2),"")

> pParse ppExprSpaces "+ 3 4"
(BinOp Plus (Num 1) (Num 2),"")
\end{spec}

Fortunately, the integration of redundant spaces works like a
charm. %

\subsubsection*{The Downside}
However, there is a downside to this solution. %
In the beginning we said that we cannot parse redundancies that are
not included in the printer-parser's definition. %
In our case, we have added these redundancies and, thus, can parse
them in a convenient way. %
This observation leads to the question: how does this integration
effect the pretty-printing of the arithmetic expression?
In the pretty-printed version of an arithmetic expression, we do not
allow any leading and trailing spaces. %
Let us test the behaviour by pretty-printing the expression from
above. %

%format /// = "\quad\quad\vdots"
\begin{spec}
> pPrint ppExprSpaces (BinOp Plus (Num 1) (Num 2)
"+ 1 2"
"+ 1  2"
"+  1 2"
"+ 1   2"
///
\end{spec}

Unfortunately, this expression does not terminate, but yields all
possible versions of string representatives. %
Possible versions include a different number of trailing space
after the operator and the first argument of that operator. %
This unsatisfactory result arises from the use of the free variables
in the definition of |ppExprSpaces|. %
We cannot fix the number of used spaces to one, the fact is that
the free variable is instantiated nondeterministically to a suitable
value. %
In our case, |[()]| is not the only suitable value, any list of |Unit|
values fits the specification of the pretty-printer. %

The work of \cite{quotientLenses} sounds promising as a solution to
our problem. %
Quotient lenses are well-behaved bidirectional transformations that
allow the programmer to specify equivalence relations on the data he
wants process. %
The authors have added an implementation of quotient lenses to the
Boomerang language. %
However, we did not have the time to implement our own version of
quotient lenses to the context of our study case. %

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

In a second approach, we want to tackle the disadvantages of
printer-parsers concerning redundancies and optional parsing rules. %
These disadvantage arises from the operational, state-free approach of
our first implementation. %
The design of printer-parsers provides the definition of
pretty-printers and implicates a corresponding parser. %
Unfortunately, the corresponding parser is only suited for the
pretty-printed string representation. %
In the previous subsection, we gave an exemplary definition to parse a
variable amount of spaces, which was unsatisfactory. %
The defined lens can be used in the parsing direction, but behaves
heavily nondeterministic when pretty-printing a value. %
One cause of the problem is that we cannot reason about single steps
of the underlying parser without changing the semantic of the
pretty-printer. %
Therefore, we want to discuss another implementation that has more
information about intermediate results. %
In order to achieve this additional information, we change the
underlying data structure in contrast to the printer-parser. %
Because of the changed data structure, we can also adjust the semantic
and add an inspection of the given input string. %
Instead of replacing the input string collectively, we want to perform
a layout preserving replacement. %
The new implementation provides a function |replaceParse :: PReplace a
-> PRLens a| that takes a specification of a so-called
\emph{replace-parser} and yields a lens function. %
The resulting lens has the same type as our printer-parser. %

\begin{spec}
type RPLens a = Lens String (a,String)
\end{spec}

We can use this lens function in the common way using |get| and
|put|. %
The get function for a |RPLens| corresponds to a parsing action like
before and in the put direction we replace a given string and try to
preserve its layout. %
If the given string is empty or does not fulfil the replace-parser's
specification, we pretty-print the data structure at hand. %
As an example: we have an arithmetic expression with more than one
space as delimiter for its arguments and update only the second
argument. %

\begin{spec}
> put (replaceParse rpExpr) "+  3 2" (BinOp Plus (Num 1) (Num 2) ,"")
"+  1 2"
\end{spec}

The result of our replace-parser assures that the layout of the
given string is preserved. %
On the other hand, we can parse the resulting string with the
replace-parser again and, hopefully, gain the original value of the
arithmetic expression. %

\begin{spec}
> get (replaceParse rpExpr) "+  1 2"
(BinOp Plus (Num 1) (Num 2),"")
\end{spec}

Indeed, the parsing direction works like a charm and ignores the
redundant spaces. %
As mentioned above, if the given string is empty, the replace-parser
prints the given value in its prettiest version, i.e., as specified in
the lens definition of |rpExpr|. %
The second argument of the pair represents a remaining string as in
the previous version. %
We can use this component to add a generic string at the end. %

\begin{spec}
> put (replaceParse rpExpr) "" (BinOp Mult (Num 3) (Num 7) ,"")
"* 7 2"

> put (replaceParse rpExpr) "" (BinOp Mult (Num 3) (Num 7) ," test123")
"* 7 2 test123"
\end{spec}

\subsubsection*{Implementation of Primitives}

In the following, we take a closer look at the underlying
implementation. %
First, we want to discuss the used data structure that tracks the
intermediate result of a replacement and pretty-print, respectively. %
We called this data structure the specification of a replace-parser
above. %

\begin{spec}
type PReplace a = String -> (a,String) -> Res String

data Res a = New a | Replaced a
\end{spec}

We can observe that the used data structure is a variant of the
|RPLens|, which wraps the resulting string into a new data
structure. %
This new data structure gives information about an action in put
direction. %
If the resulting string is wrapped with the |New| constructor, the
replace-parser performed a pretty-print on the given value. %
Otherwise, the value was pretty-printed with respect to the underlying
layout of the given string. %
In order to give an example, for a primitive combinator |charP ::
(Char -> Bool) -> PReplace ()|, we get the following results based on
the given string. %

\begin{spec}
> charP isDigit "1" ('5',"")
Replaced "5"

> charP isDigit "a" ('5',"")
New "5"
\end{spec}

In the examples, we want to replace a digit character with |'5'|. %
The first one succeeds with an replacement, because the given string
consists of a digit as well. %
In the second case, we ignore the input because it does not harmonise
with the given specification and pretty-print the given value. %

For the purpose of clarification, we start with the implementation of
|charP| as first primitive combinator for replace-parsers. %

\begin{spec}
charP :: (Char -> Bool) -> PReplace Char
charP p input (v,new) = case  input of
                              ""  -> New (v:new)
                              _   -> char' input new
  where
   char' (c':str') rest
     | p c' && (rest == str' || null str')  = Replaced rest
     | otherwise                            = New (v:rest)
\end{spec}

In two cases, we replace the input and pretty-print the value: the
input string is empty or the input does not fulfil the given
predicate. %
In particular, we check the predicate on the first character of the
input string and restrict the remaining input to be empty or to be
equal to the given remaining string |str'|. %
That is, if we want to replace a digit with another and the input
string consists of two digits, the specification is not fulfilled and
we pretty-print the value. %
However, for an identical remaining string, the replace-parser can
perform an replacement, indicated by the usage of the |Replaced|
constructor. %
We give two additional examples to clarify this circumstance. %

\begin{spec}
> charP isDigit "41" ('5',"")
New "5"

> charP isDigit "41" ('5',"1")
Replaced "51"
\end{spec}

With the help of |charP|, we can define a series of additional
primitives. %
In the example, we used |charP| to replace and pretty-print a digit,
we can enhance this expression to be applicable for actual |Int|
values. %
In addition, we can define a primitive to handle a space. %

\begin{spec}
digit :: PReplace Int
digit str (d,str')
  | 0 <= d && d <= 9 = (charP isDigit) str (intToDigit d,str')

space :: PReplace ()
space str ((),str') = charP (== ' ') str (' ',str')
\end{spec}

We use the auxiliary function |intToDigit :: Int -> Char| from the
@Char@ library that converts an integer value to a character. %
The condition |0 <= d && d <= 9| guarantees that no integer value
greater than |9| is used converted into a character. %

% Furthermore, we need composing combinators like |<>|, |<<<|, |>>>| and
% |<|>| to build more complex replace-parsers with primitives as basis. %
% We start with the alternative combinator, |<|>|, because of its simple
% implementation. %

% \begin{spec}
% (pA <|> _)  input@""    (e,str') = pA input (e,str')
% (pA <|> pB) input@(_:_) (e,str') = (pA ? pB) input (e,str')
% \end{spec}

% The alternative combinator takes two replace-parsers and applies them
% nondeterministically. %
% However, in case of an empty input string and, thus, for
% pretty-printing, we just apply the first parser and ignore the second
% one. %
% As a consequence, we always use both parses nondeterministically in
% the parsing direction. %
% That is, when we parse a string the input string is not empty, thus,
% we have to apply the second rule. %
% In the previous approach, we did not have the possibility to
% distinguish between rules for parsing and printing. %
% The additional information helps us to differentiate these two cases
% and get a convenient behaviour of the alternative combinator. %

\subsubsection*{Composition}
Moreover, we want to compose several primitives to build new
replace-parsers. %
For that purpose, we introduce the composition combinator |(<>)|, and
its descendants |(<*)| and |(*>)| in order to ignore the left and right
result, respectively. %

\begin{spec}
(<>) :: PReplace a -> PReplace b -> PReplace (a,b)
(pA <> pB) str ((expr1,expr2),str')
   | null str = pA str (expr1, unwrap (pB str (expr2,str')))
   | str == str1 ++ str2 =
      (strict pA) str1 (expr1, unwrap ((strict pB) str2 (expr2,str')))
  where str1, str2 free
\end{spec}

When composing two replace-parsers, we split the input string into two
halves. %
We take advantage of the logic features of Curry and split the input
string nondeterministically. %
This idea is adopted from the general approach of the \texttt{Parser}
library\footnote{\url{http://www-ps.informatik.uni-kiel.de/kics2/lib/CDOC/Parser.html}}
in Curry that is based on functional-logic parsers as presented by
\cite{logicParser}. %
In case of an empty input string, we run the first replace-parser on
its corresponding value and its half of the input. %
As remaining string, we provide the result of the second
replace-parser that works on its half of the input string and the
given remaining string. %
That is, we concatenate the results of the both parsers and add the
remaining string to the end. %
Because of the constructor wrapped around the resulting string, we use
the auxiliary function |unwrap| to access the containing string. %

\begin{spec}
unwrap :: Res a -> a
unwrap (New v)      = v
unwrap (Replaced v) = v
\end{spec}

As an important part of the second case, we use the function
|strict| to ensure that the given replace-parser performs a replacement. %

\begin{spec}
strict :: PReplace a -> PReplace a
strict pReplace str pair =
  case pReplace str pair of
       New _ -> failed
       res   -> res
\end{spec}

We want to guarantee that both replace-parsers perform a replacement
on their dedicated input string. %
Otherwise, the resulting string could be a combination of a
layout-preserving and pretty-printed variant of the given value. %
In our implementation, the composition fails if one of the given
replace-parsers fails to replace its value for the given input. %
In particular, the definition behaves nondeterministically and tries
every combination of two substrings that can be combined to the input
string. %
As a convenient side-effect, the usage of |strict| guarantees that an
empty substring fails for both replace-parsers. %
The |strict| function fails for every value constructed with the |New|
constructor and an empty string as input causes most primitives to
perform a pretty-print, thus, yields a string wrapped in a |New|
constructor. %
In the case of an empty input string, we apply both replace-parsers in
series without using the strict version. %

Furthermore, the implementation of |(<*)| and |(*>)| is straightforward
and the same as for the printer-parser, but with an adapted type
signature. %

\begin{spec}
(<*) :: PReplace a -> PReplace () -> PReplace a
(pA <* pB) str (e,str') = (pA <> pB) str ((e,()),str')

(*>) :: PReplace () -> PReplace b -> PReplace b
(pA *> pB) str (e,str') = (pA <> pB) str (((),e),str')
\end{spec}

\subsubsection*{Arithmetic Expressions, again}

In order to compare this approach with our previous implementation of
printer-parsers, we want to define a replace-parser for arithmetic
expressions. %

\begin{spec}
rpExpr :: PReplace Expr
rpExpr str (BinOp op e1 e2,str')  =
  ((rpOp <* spaces)  <> (rpExpr <* spaces)
                           <> rpExpr) str (((op,e1),e2),str')
rpExpr str (Num v,str')           = digit str (v,str')
\end{spec}

The definition of a replace-parser for arithmetic expressions has a
high resemblance to our version for printer-parsers. %
This resemblance arises from the usage of the same set of primitives
and combinators that can be used to define more complex function
definitions. %
The only difference is that we use |spaces| here. %
Because of the underlying data structure of this approach, we can
finally implement primitives to parse optional redundancies in the
parsing direction without interfering with the pretty-printer. %

The following code shows the implementation of |spaces| that
pretty-prints exactly one space, but can parse and replace
several spaces. %

\begin{spec}
spaces :: PReplace ()
spaces input = case input of
  ""  -> space ""
  _   -> (space *> spaces') input
 where
  spaces' input' ((),str') = case input' of
    ""  -> pure input' ((),str')
    _   -> (space *> spaces') input' ((),str')
\end{spec}

In case of an empty input string, we pretty-print one space. %
Otherwise we read or replace as much spaces as possible until the
input string is empty. %
If the input string is finally empty, the first rule of the local
function |spaces'| is used to add the remaining string to the end
of the resulting string. %
Here, we use the auxiliary function |pure| that ignores its input
string as well as its value and yields the remaining string as a
result of a replacement. %
That is, the resulting string is wrapped in the |Replaced| constructor
and |pure| always succeeds to replace a given input string. %

\begin{spec}
pure :: PReplace a
pure _ (_,str') = Replaced str'
\end{spec}

This implementation differs from the one given for the
printer-parser. %
Using a primitive like |many| and a free variable in the
implementation of |rpExpr| does not lead to success either, because of
the introduced nondeterminism. %
Nevertheless, the idea of this implementation is not applicable for
the printer-parser, because we take advantage of the implementation of
|(*>)|. %
In the replace-parser's version of |(*>)|, we actually consume the
input string. %
Thus, we actually reach the point where we stop producing or reading
spaces. %
In case of printer-parsers, the input string is ignored completely,
hence, leading to a failing parsings action on every input string. %

Last but not least, we use the same implementation to handle the
operators of an arithmetic expression like for the printer-parser. %
In fact, if it were not for the delimiting spaces, we could have
used the exact same implementation as before, but using a different
type signature. %

\subsubsection*{Poor Performance}
The more interesting part is the behaviour of our given
implementation. %
We have already seen a handful of examples as a motivation for the
idea of replace-parsers in the beginning of this subsection. %
Nonetheless, we want to give some more examples to highlight the improvement in
contrast to the previous implementation. %

\begin{spec}
> put (replaceParse rpExpr) "+  1  2" (BinOp Mult (Num 3) (Num 4),"")
"*  3  4"

> get (replaceParse rpExpr) "+  1    2"
(BinOp Plus (Num 1) (Num 2)),"")
\end{spec}

Unfortunately, this approach comes with some disadvantages, too. %
The second example indicates a performance problem due to the string
splitting on combinators like |(<>)|,|(<*| and |(*>)|. %
Collectively, there are four redundant spaces to consume when
replacing the given string with a new value. %
For each space, we introduce an additional combinator, |(<<<)|,
that splits the input string into two halves. %
In the end, the longer the input string and the more composition
combinators we use, the more splitting combinations arise and the
number of splits increases, respectively. %
That is, the nondeterministic search for a suitable splitting
increases fast and causes a bad performance. %
This performance issue affects the parsing direction as well as the
get direction in case of a replacement. %
However, the pretty-printer still performs quite well for large
expression terms. %

\begin{spec}
> put  (replaceParse rpExpr)
       ""
       (BinOp  Mult
               (BinOp  Plus
                       (BinOp  Plus
                               (BinOp Plus (Num 3) (Num 1))
                               (BinOp Minus (Num 7) (Num 3)))
                       (BinOp Div (Num 8) (Num 2)))
               (BinOp Minus (Num 3) (Num 1)),"")
"* + + + 3 1 - 7 3 / 8 2 - 3 1"
\end{spec}

The expression consists of thirty combinators and has three levels
nesting for recursive calls and executes in |2| milliseconds. %
In comparison to the get direction, we validate the execution time as
a good result. %

In the following, we show a series of graphs to illustrate the
performance issue.\footnote{We tested the performance with KiCS2 and
  the |+time| flag; the testing device was a MacBook Air with Mac OS
  10.9.4 as operation system, |4| GB memory and a |1.8| GHz Intel Core
  i5 as processor.} %
Both graphs have the same labels: the x-axis represents the number of
combinators used in an expression; the y-axis indicates the execution
time of an expression. %
In Figure~\ref{fig:plotReplaceA}, we show the execution time of a
replacement action for an increasing number of combinators. %
The behaviour of the graph indicates an exponential growth. %
In order to investigate this hypothesis, we used a logarithmic scale
for the execution time in Figure~\ref{fig:plotReplaceB}. %
Indeed, the adjusted graph shows a linear growth, which indicates an
overall exponential runtime with respect to the number of
combinators. %

\begin{figure}[h]
\includegraphics[width=\textwidth]{../images/replace.pdf}
\caption{Performance of replacing for increasing number of combinators}
\label{fig:plotReplaceA}
\end{figure}

\begin{figure}[h]
\includegraphics[width=\textwidth]{../images/replace2.pdf}
\caption{Performance of replacing for increasing number of combinators}
\label{fig:plotReplaceB}
\end{figure}

We also measured the performance for a parsing action, unfortunately,
the results are even worse. %
The results are illustrated in Figure~\ref{fig:plotParse}. %
Parsing a string with our library performs bad, because the get
function guesses the resulting value -- the arithmetic expression. %
Thus, the performance depends on the size of the arithmetic expression
and of the number of possible values of each component. %
For example, the operator data type has four possible constructors,
each of this constructor is tested in order to find the corresponding
arithmetic expression for a given string. %

\todo{change Figure~\ref{fig:plotParse}}
\begin{figure}[h]
\includegraphics[width=\textwidth]{../images/kitten.jpg}
\caption{Performance of parsing for increasing number of combinators}
\label{fig:plotParse}
\end{figure}

% \begin{figure}[h]
% \caption{Performance of pretty-printing for increasing number of combinators}
% \label{fig:plotPretty}
% \end{figure}

\subsubsection*{Ace In The Hole}

Although we do not prioritise a good performance, we take a last try
on implementing a replace-parser. %
Instead of a functional-logic approach that makes heavily use of
nondeterminism, we change our underlying implementation to aim for a
more functional approach. %
The weak point of our first implementation was the composition
combinator. %
When composing two replace-parsers, we make a guess on how to split
the input string such that both replace-parsers yield appropriate
results. %
In the functional approach, the first parser yields a remaining string
that is used as input for the second parser. %
Our parsing result has the same representation: the result of the get
direction is |(a,String)|. %
However, we cannot effectively use this result for the definition of a
replace-parser, because of the underlying put-based lenses. %
Hence, we need to find a convenient representation for the underlying
data structure in order to use intermediate results for the parsing
direction as well. %

\begin{spec}
type PReplace a = String -> (a,String) -> (String,String,String)
\end{spec}

We choose a triple as result of our replace-parser. %
The first component represents the string of a successful replacement
or pretty-print, whereas the other two components are remaining
strings. %
We distinguish between the remaining input string that has not been
consumed yet as the second component, and, lastly, the remaining
string to concatenate at the end of the resulting string. %

As a first representative example, we define a primitive to handle a
char that fulfils a given predicate, again. %

\begin{spec}
charP :: (Char -> Bool) -> PReplace Char
charP _ ""     (e,str') = ([e],"",str')
charP p (c:cs) (e,str') | p c  = ([e],cs,str')
\end{spec}

For an empty input string, we pretty-print the given char value -- the
pretty-printed result is the first component of the triple. %
In case of a replacement, we consume the first character if the
predicate holds and need to remember the remaining input in the second
component. %
The given remaining string is transfered as the third component of the
triple. %

With this technique at hand, we can define a more convenient
composition combinator that does not rely on nondeterminism. %

\begin{spec}
(<>) :: PReplace a -> PReplace b -> PReplace (a,b)
(pA <> pB) input ((e1,e2),str') = case input of
   ""  -> (res1,str2,str1' ++ str2')
   _   -> if  null str1 && (res2,str',str2') /= pure str1 (e2,str1')
            then failed
            else (res1,str2,str1' ++ str2')
  where
   (res1,str1,str1') = pA input (e1,res2)
   (res2,str2,str2') = pB str1 (e2,str')
\end{spec}

The general idea of the combinator is as before: we apply the first
replace-parser on the input string, the corresponding value and the
result of the second parser. %
The important difference is that we can actually use the remaining
string of the first replace-parser as argument to the second one. %
Unfortunately, we lose performance by concatenating both remaining
strings for the resulting tuple. %
However, this concatenation is rather a technicality than a huge
problem; we can easily rewrite the code using |foldr|, i.e.,
|str1' ++ str2'| becomes |foldr (:) str2' str1'|. %
The additional check on a non-empty string can be seen as an
equivalent to the usage of |strict| in the previous implementation. %
When composing two replace-parsers, we want to make sure that both
actually consume any input. %
However, there is one exception: if the given replace-parsers does not
care about its input, we do not want the composition to fail. %
Therefor, we use |pure|, the primitive replace-parser that succeeds on
every input, in order to handle the special case of a non-consuming
replace-parser. %

In order to use the definition of |rpExpr| that we have given above,
we still need to define |spaces|. %
The key for a modified definition of |spaces| is a combination of
|pure| and the alternative combinator |(<||>)|. %
In Curry, we can define the alternative combinator using the choice
operator, |?|, and nondeterministically choose one of the given
replace-parsers. %

\begin{spec}
(<||>) :: PReplace a -> PReplace a -> PReplace a
(pA <||> _)  ""          = pA ""
(pA <||> pB) input@(_:_) = (pA ? pB) input
\end{spec}

In order to restrict the nondeterministic behaviour to the parsing
direction and a replacement action, we simply apply only the first
replace-parser for an empty string. %
That is, pretty-printing is still deterministic and does not introduce
a choice between the two given replace-parsers. %

In the end, we can define |spaces| as follows. %

\begin{spec}
spaces :: PReplace ()
spaces input = case input of
   ""  -> space ""
   _   -> ((space *> spaces')) input
  where
   spaces' :: PReplace ()
   spaces' input' ((),str') = case input' of
      ""  -> pure "" ((),str')
      _   -> ((space *> spaces') <||> pure) input' ((),str')
\end{spec}

The definition of |spaces'| benefits from the combination of the
alternative combinator and |pure|. %
This combination allows us to consume an arbitrary number of
spaces and stop whenever the |space| parser fails. %
We also stop if the input string becomes empty, and yield a result to
signal a successful consumption. %

Due to the actual consumption of the input string, we achieve better
results for the performance. %
In Figure~\ref{fig:replaceBetter}, we show the measured execution time
for the same expressions that we ran for the previous
implementation. %
The results include two additional test expressions in order to show
that the expected run time with respect to the number of used
combinators is nearly linear. %

\begin{figure}
\includegraphics[width=\textwidth]{../images/replaceBetter.pdf}
\caption{Performance of replacing for an increasing number of combinators}
\label{fig:replaceBetter}
\end{figure}

% \subsection{Parser-Printer}

\subsection{Conclusion and Similar Approaches}

In this section we presented the usage of lenses for a new approach to
specify printers and parsers in one definition. %
The resulting performance is still in need of improvement, but the
implementation is sufficient for a first prototype. %
This approach is a big selling point for nondeterministic lenses. %
The nondeterministic parsing direction helps us to achieve a
meaningful lens definition in both the get and the put direction. %
Several papers that deal with bidirectional programming mention the
usage of lenses to unify the definition of printers and parsers. %
However, there is only one actual implementation that realises this
specification with the help of lenses. %

The approach is followed by \cite{parsing1} and \cite{parsing2},
respectively. %
The first publication is a conglomerate of case studies in the field
of bidirectionalisation that also includes a section about printers
and parsers. %
The second publication focuses on parsing and \emph{unparsing}, in
which the authors illustrate a megamodel of parsing. %
This megamodel includes all different artefacts and the corresponding
mappings of one artefact to another. %
As a result of this investigation, he applies different
bidirectionalisation techniques in order to implement parsing and
unparsing in the meta-programming language
Rascal~\citeyearpar{rascal}. %
The mappings include similar features that are available in our
implementation: layout-preservation, parsing, unparsing and
redundancies in the parsing direction. %
In addition, the authors discuss a feature to automatically correct
misspellings and likewise parse errors. %

Other related work includes the idea of \cite{invertibleSyntax}, who
propose a new interface to describe parser and pretty-printer in a
single program. %
They provide a type class |Syntax delta| that includes common parser
functions like |(<$$>)|,|(<*>)|, |(<||>)|, |pure| and |empty|. %
Due to the usage of pretty-printers, the composition combinator,
|<*>|, forms a pair, like in our implementation, instead of using the
traditional composition semantic of parsers. %
In order to define parsers and pretty-printers, the programmer defines
an instance for the |Syntax| typeclass. %
In the end, the implemented instance decides if the combinator behaves
like a pretty-printer or a parser. %
This approach has similarties to using a pair of functions to
represent lenses: we still have to define and maintain both sides of
the implementation. %
Thus, we see an advantage of our implementation in comparison to the
idea of Rendell and Ostermann. %

Last but not least, we want to mention the publication of
\cite{flippr}, who introduce a \emph{FliPpr}, a program transformation
system that can be used to define pretty-printers and gain a
corresponding parser. %
FlipPr produces a CFG parser that is consistent with the given
definition of the pretty-printer. %
Their advantage in comparison to bidirectional approaches is that
they can outsource their parsing algorithms to parser generators and
reuse efficient implementations of exisiting pretty-printer
libraries. %
In particular, FlipPr is implemented in Haskell and reuses
\citeauthor{pretty}'s existing implementation of pretty-printers,
additionally, they make use of
\emph{happy}\footnote{\url{http://www.haskell.org/happy/}} as parser
generator. %
