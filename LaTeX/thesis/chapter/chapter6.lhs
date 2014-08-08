\chapter{Case Studies}

\section{Case Study I - Bidirectional Printer-Parser}

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
one string representation. %

Like for parser and printers, we need a handfull of combinators to
simplify the definition of new printer-parsers for our own
structure. %
At first, we declare our data structure as |type PP a = String ->
(a,String) -> String|, which is sometimes more, sometimes less used in
the implementations. %
The first argument is the input string, the second argument is pair
consisting of a representation and a remaining string. %
In the initial \emph{put}-direction, we concatenate the remaining
string and the end of the pretty-printed value of the given
representation. %
This representation\footnote{a slightly different one, but still
  equivalent} is most commonly used for pretty-printers to achieve a
linear run-time subject to the resulting pretty-printed string. %
Additionally, we have two functions |pretty :: PP a -> String ->
(a,String) -> String| and |parse :: PP a -> String -> (a,String)|. %
All following implementations define the same basic combinators, that
we will present next. %

As primitive printer-parsers, we define |digit :: PP Int| that
pretty-prints a digit in the \emph{put}- and parses a digit in the
|get|-direction.

\subsection{Similar Approaches}

\begin{itemize}
\item FlipPr \cite{flippr}
\item Invertible Syntax Description \cite{invertibleSyntax}
\item Arrows \cite{invertibleArrows}
\item unparsing/parsing \cite{parsing2}
\end{itemize}

\subsection{Printer-Parser}
In a first step, we implemented an abstraction for printing and
parsing, that only needs one definition. %
The data structure has the form |type PPrinter a = Lens String
(a,String)|. %
We need to define the `put`-direction for our lens, that is, our
printer-parser definitions have the following form: |ppExpr :: String
-> (a,String) -> String|. %

As a main disadvantage of this approach, we cannot ignore redundant
parts in the given string, when we want to parse, if this redundancies
do not appear in the pretty-printer definition. %
Furthermore, the definitions for the printer have to be more
sophisticated than usual. %
The printer and parser definitions are connected, that is, typical
restrictions known from parser constructions need to be considered. %
In our example, we only defined a prefix based arithmetic expression
data structure, but with infix operators, we have to avoid
left-recursion in the definitions. %

\subsection{Replace-Parser}

\subsection{Parser-Printer}

\section{Case Study II - WUILenses}
\begin{itemize}
\item WUI \cite{wui}
\item Spicey \cite{spicey}
\item lenses for web data \cite{webLenses}
\end{itemize}arrows
\subsection{Projections for Spicey with Lenses}
\subsection{Example web page}

\section{Case Study III - Lenses for Records}

Lorem ipsum ... introduction.

\subsection{Record Syntax in Curry}
In the current KICS2 implementation\footnote{cite anything}, we can
define types similiar to data type declarations as records in
Haskell. %
In the remainder of this section, we call these defintions record
types. %
As an examplatory definition of a record types, we define |Person|, a
data structure that two fields, a first and a last name, and both are fields represented
as |String|s. %

\begin{code}
type Person = { first :: String, last :: String }
type Contact = { person :: Person, street :: String }
\end{code}

At first, the compiler desugars record declarations to data type definitions
first. %
The value constructor's name is identical to the name of the record
type and this constructor takes as many arguments as fields exist in
the record declarations. %

\begin{code}
data Person = Person String String
data Contact = Contact Person String
\end{code}

Furthermore, the field name accessors are labels that are only known
in combination with special syntactical constructs, which are |:>| and |:=|. %
That is, we can define a value of type |Person| with record notation,
where |:=| is used to assign a field to a value within record construction and
|:>| is an accessor that we can use to get a value out of a
record. %

\begin{spec}
aPerson :: Person
aPerson = { first := "Bob", last := "Dylan" }

> aPerson :> first
"Bob"

> aPerson :> last
"Dylan"
\end{spec}

Record updates are also possible; in order to update a given record
type |Person| with fields |first| and |last|, we use another
special syntax operator, |{ _ := _ || _ }|, to annotate which record
value and which record fields of that record we
want to change. %

\begin{code}
appendToFirst :: Person -> Person
appendToFirst person = { aLabel2 := val + 1 | person }
  where val = person :> first
\end{code}

The construction without the pipe operator looks like a normal record
definition, in combination with the pipe, we can update a record value
that is given to the right of the operator. %
In the the example, we have a record value with two fields, but only
one field is explicitely set to the left of the pipe operator. %
Record updates allow the programmer to only write down the fields to be
updated for a given record, all other fields remain unchanged. % 

The usage of records can be very helpful and elegant, but has its downsides as
well. %
In contrast to Haskell, the fields |first| and |last| of the |Person|
type are not functions, but syntactical constructs called labels. %
The advantage of these labels is that the name spaces of function
names and labels are disjoint. %
That is, we can define functions |first| and |person| without
interfering with our existing record fields. %

\begin{code}
first :: Person -> String
first p = p :> first

person :: Contact -> Person
person c = c :> person
\end{code}

\subsection{Step by Step: From Records to Lenses}

In order to examine records a little bit further, let us define more
complex record field accessors for nested record definitions. %
For example, the record type |Contact| contains a field |person| of
type |Person|, which, again, is a record type itself with fields
|first| and |last|, both of type |String|. %

\subsubsection{Getting there is half the fun}

We can define a function |getFirstForAddress| that takes a value of
type |Contact| as argument and yields a |String| as result. %
The result |String| is the first name of the person of the given
contact, i.e., we first access the field |person| and use the resulting record
type |Person| to access the field |first|. %

\begin{code}
getFirstForContact :: Contact -> String
getFirstForContact contact = contact :> person :> first
\end{code}

The definition of this function looks straightforward and quite
compositional, but since |person| and |first| are not real function
but labels, i.e. special syntactical constructs, we cannot compose
these accessors like in Haskell\footnote{}. %
In order to make this point more clear, we define a second version
|getFirstForContact'|, which is defined with the help of the functions
|first| and |person| that we defined earlier. %

\begin{code}
getFirstForContact' :: Contact -> String
-- getFirstForContact' contact = first (person contact)
-- getFirstForContact' contact = (first . person) contact
getFirstForContact' = first . person
\end{code}

In Curry, we cannot apply well-known simplification mechanism, e.g.,
eta-reduction or point-free style, for record accessors. %

\subsubsection{Set your record straight}

In a second step, we define a function to change the value of a field
in a given record. %
Thus, we define a function |setFirstForContact|, which takes a |Contact|
and a |String| as arguments in order to change the first name of the
person within that contact. %

\begin{code}
setFirstForContact :: Contact -> String -> Contact
setFirstForContact contact name =
  { person := { first := name | contact :> person } | contact }
\end{code}

For this function definition, we need to update two record values: the
person within the given contact and the first name of that person. %
Therefore, we need to access the value of the field |person| of the
given contact, i.e., |contact :> person|, in order to modify the
|first| field. %
The resulting modified value of type |Person| is the new value of the
|person| field for the given contact. %
As the record gets more and more nested, the more complex is the update
mechanism. %
Similiar as for the selection function, we take a try to simplify the
update function as well. %
First, we define two auxiliary functions |first'| and |person'| that
update the field corresponding to their names for a given value of
type |Person| and |Contact|, respectively. %

\begin{code}
first' :: Person -> String -> Person
first' p new = { first := new | p }

person' :: Contact -> Person -> Contact
person' c new = { person := new | c }
\end{code}

With the help of the selection functions |person| and |first| to access
the person within a contact and the first name of a person,
respectively, we can redefine the setter function above.  %

\begin{code}
setFirstForContact' :: Contact -> String -> Contact
setFirstForContact' c new = person' (person c) (first' (first person) new)
\end{code}

The new version of the nested setter functions looks a little bit less
complicated, but it seems time-consuming to define all these auxiliary
functions for all record types that we define in a program. %
Thus, as a next step, we try to generalise the defined get and set
function to work for all record types. %

\subsubsection{Make you a lens for a greater good}
We define a function |get :: (rec -> recField) -> rec -> recField|, where |rec| is
a record type and |recField| is the type of a field of that record. %

\begin{code}
type Get a b = a -> b

get :: Get a b -> a -> b
get getF val = getF val
\end{code}

As we have seen above, it is easy to compose getters for nested record
values; with the new defined |get| function, we can access a field of a
record value as follows\footnote{As we stated before, KICS2 translated
  a record type into a data type declaration, that is, the REPL
  uses this translated data type when printing a record value}. %

\begin{spec}
personGet :: Get Contact Person
personGet c = c :> person
firstGet :: Get Person -> String
firstGet p = p :> first

aContact :: Contact a
Contact = { person := aPerson, street := "Folkstreet 1969" }

> get personGet aContact
Person "Bob" "Dylan"
> get (firstGet . personGet) aContact
"Bob"
\end{spec}

For a generalised setter function, we define |set :: (rec -> recField -> rec) -> rec -> recField
-> rec| and we use the type variables, again, as descriptive names. %

\begin{code}
type Set a b = a -> b -> a

set :: Set a b -> a -> b -> a
set setF val new = setF val new
\end{code}

When revising the setter function for the nested record value, we come
to the conclusion that we cannot compose two setters in the same
smooth way as the getters. %
Let us try to define a combinator |(<.>) :: Set a b -> Set b c
-> Set a c| anyway, which takes two setter functions and yields a new,
combined setter. %

\begin{spec}
(<.>) :: Set a b -> Set b c -> Set a c
(fAB <.> fBC) valA valC =
   let  newB = fBC valB valC
        valB = undefined
   in fAB valA newB
\end{spec} 

The secod setter function |fBC :: b -> c -> b| yields a value of type
|b|, which is the same type the first setter
function |fAB :: a -> b -> a| takes as its second argument, that is,
we can combine the given value |valA :: a| and the result of the first
setter to get a new value |newB :: b|. %
The setter function |fBC|  takes a value of type |b| and one of type
|c| as its arguments, we have |valC :: c| as an argument, so the only
missing piece is a value of type |b|. %
The two setter function alone cannot be combined in a meaningful way;
we need the corresponding getter function |getAB :: a -> b| to fill
the missing piece. %
Thus, we add another argument to the combinator |(<.>)| in order to
complete the definition. %

\begin{spec}
(<.>) :: (Get a b,Set a b) -> Set b c -> Set a c
((getAB,setAB) <.> setBC) valA valC =
   let  newB = setBC valB valC
        valB = getAB valA
   in setAB valA newB
\end{spec}

With the new definition of the combinator |(<.>)|, we change the first
name of a contact as before, but in addition, we gain a general mechanism to
change any field of a record. %

\begin{spec}
personSet :: Set Contact Person
personSet c newP = { person := newP | c }

firstSet :: Set Person String
firstSet p newF = { first := newF | p }

> set ((personGet,personSet) <.> firstSet) aContact "Bobby"
Contact (Person "Bobby" "Dylan") "Folkstreet 1969"
\end{spec}

In the last step, we change the second argument to match the type of
the first, i.e., we take two pairs, where the first component is a getter
and the second component is a setter function. %
This change leads to pair as resulting type as well, that is, we can
define both, compositions of getter and setter functions, in one
combinator. %
In the end, we get the following definition of |(<.>)|. %

\begin{code}
(<.>) :: (Get a b, Set a b) -> (Get b c, Set b c) -> (Get a c, Set a c)
((getAB, setAB) <.> (getBC,setBC)) = (getAC, setAC)
  where
   getAC = getBC . getAB
   setAC valA = setAB valA . setBC (getAB valA)
\end{code}

The attentive reader may recognise the structure: it looks exactly
like our primitve lens definitions from Section \todo{which
  section?}. %
This observation leads to the idea of a new transformation of record
declartions in Curry, which we discuss in the next subsection. %

\subsection{Record Transformation}
Instead of introducing special syntactical constructs like |rec :> recField| and |{ recField := newValue || rec
}| to select and update a record field for a given record, we use
lenses as a general mechanism. %
As a bonus, nested records updates gain a general combinator to change
a deep nested record field more easily. %
In order to give a better insight about this idea, we first give the generated
counterpart for the record definition of the beginning of the
section as Curry code.  %

\begin{code}
type Contact = { person :: Person, street :: String }
type Person = { first :: String, last :: String }

-- generated code
data Contact = Contact Person String
data Person = Person sString String

person :: (Contact -> Person, Contact -> Person -> Contact)
person = (personGet,personSet)
  where
   personGet (Contact p _)      = p
   personSet (Contact p s) newP = Address newP s

first :: (Person -> String, Person -> String -> Person)
first = (firstGet,firstSet)
  where
   firstGet (Person f _)      = f
   firstSet (Person f l) newF = Person newF l 
\end{code}

As a first observation, we can rewrite the type signature of |person|
to highlight the similiarity to the definitions above; we generalise
this type signature again and define a type synonym |Lens a b| for a
less verbose type signature in future code examples. %

\begin{spec}
-- person :: (Contact -> Person, Contact -> Person -> Contact)
-- person :: (Get Contact Person, Set Contact Person)
person :: Lens Contact Person

type Lens a b = (Get a b, Set a b)
\end{spec}

%format label2 a b = "f_{" a "_" b "}"
%format label3 a b c = "f_{" a "_{" b "\dots" c "}}"
%format label_tau a b c = "f_{" a "_{" b "\dots" c "}}^{\tau_" a "}"
%format Rec = "R"

Next, we examine the generated code a bit more. %
As in the current transformation of record types, we generate a data
type declaration corresponding to the record type: one value
constructor with the same name as the record type and each field is of
the record type is an argument of the value constructor. %
The arrangement of arguments are adopted from the record
declaration. %
That is, for a record declaration |type Rec = { label2 1 1, ... ,
  label2 1 n :: tau 1, ... , label2 k 1, ... ,
  label2 k m :: tau k }|\footnote{In the following, we shorten a
  sequence like |label2 1 1, ... , label2 1 n| to |label3 1 1 n| and an
  optional type is superscripted, e.g., |label3 1 1 n :: tau 1|
  becomes |label_tau 1 1 n|, for abbreviation purposes.}
 we get the following transformation rule. %

\todo{Introduce abbreviation for records}

\begin{tf}
\leavevmode
\begin{center}
\AXC{|tau 1 ... tau k| $\in \Theta$}
\AXC{|Rec| $\not \in \Theta$}
\AXC{|Rec| $\not \in \Psi$}
\TIC{$(\Theta,\Psi): 
|type Rec =| \left\{
   \begin{array}{l l}
     ~~|label_tau 1 1 n|\\
     , ~\dots\\
     , ~|label_tau k 1 m|
\end{array}\right\}$
$\rightsquigarrow$ |data Rec = Rec|
$\underbrace{\tau_1 \cdots \tau_1}_\text{n-times} \cdots \underbrace{\tau_k \cdots
  \tau_k}_\text{m-times}$}
\DP
\end{center}
\end{tf}

As a precondition for this transformation rule, we demand that the
types, which we use in the record defintion, are known types of
the given environment. %
We denote the set of known types as $\Theta$.
Furthermore, since the name of the defined record type is used
as the name for the generated data type and constructor, the name has
to be unique it the given environment as well. %
That is, |Rec| is neither allowed to be an element of the set of types
$\Theta$, nor an element of the set of constructors $\Psi$. %

The next step is to generate the corresponing lens function for every
field of the given record. %

%format (label1 i) = "f_{" i "}"
%format LensType a b = "Lens_{" a "\rightarrow" b "}"
%format (get_ a) = "get_" a
%format (set_ a) = "set_" a
\begin{tf}
\leavevmode
\begin{center}
\AXC{|label3 1 1 n, ... , label3 k 1 m| $\not \in \Phi$}
\AXC{|type LensType a b = (a -> b, a -> b -> a)|}
\AXC{\eqref{f_lens}}
\TIC{$\Phi:
|type Rec =| \left\{
   \begin{array}{l l}
     ~~|label_tau 1 1 n|\\
     , ~\dots\\
     , ~|label_tau k 1 m|
\end{array}\right\}$
$\rightsquigarrow$
$\begin{array}{l l}
|label3 1 1 n :: LensType Rec (tau 1)|\\
|(label1 1) = (label1 (get_ 1),label1 (set_ 1))|
\end{array}
,~\dots~,
\begin{array}{l l}
|label3 k 1 m :: LensType Rec (tau k)|\\
|label1 k = (label1 (get_ k),label1 (set_ k))|
\end{array}
$
}
\DP
\end{center}
\end{tf}

We demand all names of the new defined record fields to be unique
in the given environment, that is, functions with the same name are
not allowed.\footnote{This requirement is also mentioned in the KICS2
  Manual \citeyearpar[p. 22]{kics2Manual}, but a missing feature in the current implementation.} %
Therefor, we introduce $\Phi$ as the set of all function
names and if any record field is an element of that set, the
preconditition is not fulfilled, thus, the transformation cannot be
pursued and fails. %
In order to make the derivation rule more readable, we introduce a
type synonym for lenses |type LensType a b = (a -> b, a -> b -> a)| for
further usage, but we do not generate the lens type synonym in our
record transformation for simplicity reasons only. %
There is still one piece missing in this transformation, the generated
functions still need a proper function body. %
For every generated function in the previous step, we generate a
function body with helper functions |label1 get| and |label1 set|. %

%format (val i) =  "val_"i
%format (v3 a b c) = "val_{" a "_{" b "\dots" c "}}"
%format label3 a b c = "f_{" a "_{" b "\dots" c "}}"

%format label = "f"
%format -.- = "~\cdots~"

\begin{code}
label1 i = (label1 get,label1 set)
  where
   label1 get (Rec _ -.- val i -.- _)                = val i
   label1 set (Rec (v3 1 1 n) -.- v3 k 1 m) (val i)  = Rec (v3 1 1 n) -.- val i -.- v3 k 1 m
\end{code}

This transformation generates the corresponding lens function for each
field of a record type, where |label1 i| is the current generated
field. %
We generate the lens function in two steps: first, we define 
two local functions |label1 get| and |label1 set| and combine them to
a pair of getter and setter function, i.e. a lens, in a second step. %
