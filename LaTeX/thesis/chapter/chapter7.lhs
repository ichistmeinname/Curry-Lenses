\chapter{Conclusion}


\section{Challenges}\label{sec:chall}

During the testing phase of our put-based lens libraries, we ran into
complications due to Curry's internal structure of |Int| values in
combination with the built-in search mechanism. %
In the following, we give a detailed insight of Curry's built-in
search capabilities with an example and the consequential problems. %
We also present two approaches to solve the upcoming
problem for the exemplary lens definition. %

\subsection{Curry's built-in search abilities}

In this section we work with the put-based lens library that we
presented in Section~\ref{sec:putImpl}. %
As a quick reminder: we have the following interface for put-based
lenses that generate a corresponding |get|-function. %

\begin{spec}
type Lens a b = a -> b -> a

put :: Lens a b ->  a -> b -> a
put lens s v = lens s v

get :: Lens a b -> a -> b
get lens s | put s v == s = v
 where v free
\end{spec}

In order to compare our lenses with related approaches, we want to
define equivalent lenses to the ones presented by \cite{biForFree}. %
We pick |putHalve| as exemplary lens definition, which is defined as
follows. %

\begin{code}
putHalve :: [a] -> [a] -> [a]
putHalve xs xs' | length xs' == n = xs' ++ drop n xs
 where
  n  = length xs `div` 2
\end{code}

|putHalve| takes two lists, a source list and a view list, and
concatenates the second list with the second half of the first list. %
Valid view lists have half the length of the source list, if
otherwise, the function yields |failed|, i.e., no result is
produced. %


In the following, our aim is to generate a get function for
|putHalve|. %
The wished |get| function is equivalent to the following definition
|halve|. %

\begin{code}
halve :: [a] -> [a]
halve xs = take (length xs `div` s) xs
\end{code}

With help of our interface, we can derive a corresponding get function
by simply calling |get| with |putHalve| and our source value. %

\begin{code}
getHalve = get putHalve
\end{code}

Unfortunately, this easy task evolved into a long investigation of
Curry's built-in search mechanism as well as the internal
representation of |Int| values and their interaction with lists. %

First of all, a function call like |getHalve [(),()]| does not
terminate. %
The problem is the combination of the internal representation of lists
and numbers; they do not harmonise well. %
This effect is triggered by the usage of |length|. %
The free variable |v| corresponds to |xs'| in the definition of
|putHalve|, i.e., the system guesses values for |xs'|. %
In order to be more precise, the system needs to guess lists of type
|()| for |xs'| and check if their length is the same as |length
[(),()] `div` 2|. %

In order to focus on the cause of the problem., we can simplify our definition of
|putHalve|. %

\begin{code}
putHalveSimple :: [a] -> [a]
putHalveSimple xs' | length xs' == 1 = xs' ++ drop 1 [(),()]
\end{code}

Curry uses binary numbers as representation for numbers plus
additional information about the algebraic sign and to represent |0|
as well. %
The data structure |Nat| defines constructors for binary numbers and
|BinInt| is the overall representation wrapping the binary numbers. %

\begin{code}
data BinInt = Neg Nat | Zero | Pos Nat
data Nat = IHi | O Nat | I Nat
\end{code}

If we now replace the usage of |Int| values with the internal
representation with |BinInt|, we end up with the following function. %

\begin{code}
putHalveSimple :: [a] -> [a]
putHalveSimple xs' | length xs' == Pos IHi = xs' ++ drop 1 [(),()]

length' :: [a] -> BinInt
length' []     = Zero
length' x:xs = inc (length xs)
\end{code}

Additionally, we define an axuilary function |length'| that computes
the length of a given list and yields a |BinInt|. %

\subsubsection*{The Problem}

So, how do the evaluation steps look like when we want to compute
the length of a list?

\numberson
\numbersright
\begin{code}
length :: [a] -> BinInt
length []     = Zero
length x:xs = inc (length xs)

inc :: BinInt -> BinInt
inc Zero        = Pos IHi
inc (Pos n)     = Pos (succ n)
inc (Neg IHi)   = Zero
inc (Neg (O n)) = Neg (pred (O n))
inc (Neg (I n)) = Neg (O n)
\end{code}
\numbersoff
\numbersreset

We can directly compute the result for an empty list, but
for a non-empty list we build a sequence of |inc|-operations,
e.g. |inc (inc Zero)| for a two-valued list, |inc (inc (inc Zero))|
for a three-valued list etc. %
In order to take this investigation
one step ahead, we need to look at the definition of |inc|, where only
lines 5 and 6 are of further interest. %
We give the evaluation of zero to two consecutive |inc| function call
in Figure~\ref{fig:incEval}. %
\numberson
\numbersright

% definitions for equation environment
\def\commentbegin{\quad\{\ }
\def\commentend{\}}

\begin{figure}
(1)
\begin{spec}
Zero == Pos IHi

==

 False
\end{spec}

(2)
\begin{spec}
inc (Zero) == Pos IHi

==

Pos IHi == Pos IHi

==

True
\end{spec}

(3)
\begin{spec}
inc (inc Zero)) == Pos IHi

==

inc (Pos IHi) == Pos IHi

==

Pos (succ IHi) == Pos IHi

==

Pos (O IHi) == Pos IHi

==

False
\end{spec}

% (4)
% \begin{spec}
% inc (inc (inc Zero))) == Pos IHi

% ==

% inc (inc (Pos IHi)) == Pos IHi

% ==

% inc (Pos (succ IHi)) == Pos IHi

% ==

% Pos (succ (succ IHi)) == Pos IHi

% ==

% Pos (succ (O IHi)) == Pos IHi

% ==

% Pos (I IHi) == Pos IHi

% ==

% False
% \end{spec}
\caption{Evaluation of |inc| for an increasing number of function
  calls}
\label{fig:incEval}
\end{figure}
\numbersoff
\numbersreset

The attentive reader may have already noticed that the definition of
|inc| is strict in its first argument and does not propagate its
constructor. %
The successor function, |succ|, on binary numbers is also strict. %
Unfortunately, the |length| function cannot be implemented in a way
that is sufficient to propagate a constructor, because it is
problematic to map the empty list to a value of type |Nat|. %
We discuss the problem concerning |Nat| in more detail later. %

In the end, the |length| function evalutes the whole list to determine
its length, which leads to non-evaluation when guessing a list with a
specific length. %
The built-in search for free variables in KICS2 can be translated in
nested injections of |?|-operations, where every constructor of the
given type is a possible guess and arguments of constructors are also
free variables. %
We illustrate the built-in search of our example in
Figure~\ref{fig:lengthEval}. %

\numberson
\numbersright
\begin{figure}
\begin{spec}
length v == Pos IHi
  where v free

==

length ([] ? _x2:xs) == Pos IHi
  where _x2,xs free

==

(length [] ? length _x2:xs) == Pos IHi
  where _x2, xs free

==

length [] == Pos IHi ? length _x2:xs == Pos IHi
  where
_x2,xs free

==

Zero == Pos IHi ? length _x2:xs == Pos IHi
  where _x2,xs
free

==

False ? length _x2:xs == Pos IHi
  where _x2,xs free

==

False ? inc (length xs) == Pos IHi
  where xs free

==

False ? inc (length [] ? length _x4:ys) == Pos IHi
  where _x4,ys free

==

False ? inc length [] == Pos IHi  ? inc (length _x4:ys) == Pos IHi
  where _x4,ys free

==

...
\end{spec}
\caption{Guessing a list with size one}
\label{fig:lengthEval}
\end{figure}
\numbersreset
\numbersoff

For every free variable of type |[a]| both constructors are possible
values, therefore, both expressions are introduced with the
|?|-operator. %

In the end, the built-in search collects all possible values and works
henceforth with a set of values, i.e., every list of the resulting set
is used for further function calls.
For our example, we get the following results in the interactive
environment of KiCS2. %

\begin{code}
{v = []} False
{v = [_x2]} True
{v = [_x2,_x4]} False
...
\end{code}

As we said in the beginning, the internal structure for lists and
numbers do not harmonise well - how can we solve the problem that
|putHalve| does not terminate with the current implementation? %
We will present two different approaches in the following two
subsections. %

% Beforehand, we exchange the usage of |BinInt| by |Nat| to see, if this
% little change will do the difference. %
% Up to now, it seems that the built-in search cannot terminate, because
% the |Pos| constructor takes too long to be propagated. %

% \begin{code}
% lengthNat :: [a] -> Nat
% lengthNat [_]      = IHi
% lengthNat (_:y:ys) = succ (lengthNat (y:ys))
% \end{code}

% \begin{figure}
% \begin{spec}
% lengthNat v == IHI
%   where v free

% -->

% lengthNat ([] ? _x2:xs) == IHi
%   where _x2,xs free

% -->

% (lengthNat [] ? lengthNat (_x2:xs)) == IHi
%   where _x2,xs free

% -->

% lengthNat [] == IHi ? lengthNat (_x2:xs) == IHi
%   where _x2,xs
% free

% -->

% False ? lengthNat (_x2:xs) == IHi
%   where _x2,xs free

% -->

% False ? lengthNat (_x2:([] ? (_x4:ys))) == IHI
%   where _x2,_x4,ys free

% --> .. -->

% False ? lengthNat [_x2] == IHI ? lengthNat (_x2:_x4:ys) == IHi
%   where _x2 _x4,ys

% -->

% False ? IHi == IHi ? succ (lengthNat (_x4:ys)) == IHi
%   where _x4,ys free

% -->

% False ? True ? succ (lengthNat (_x4:([] ? (_x6:zs)))) == IHi
%   where _x4,_x6,zs

% -->

% ...
% \end{spec}
% \end{figure}

\subsubsection*{Approach One: Peano numbers}

As a first attempt, we use an alternative data structure with an unary
representation for numbers: peano numbers. %

\begin{code}
data Peano = Zero | S Peano

lengthPeano :: [a] -> Peano
lengthPeano = foldr (const S) Z
\end{code}

% lengthPeano []     = Z
% lengthPeano (x:xs) = S (lengthPeano xs)

Peano numbers are represented with a constructor for |Zero| and a
successor constructor |S Peano|. %
The corresponding length function, |lengthPeano| introduces an
|S|-constructor for every element of the list and yields |Zero| for an
empty list. %
Let us now take a look at the simplified implementation of
|putHalvePeano|, which uses peano numbers instead of |Int| values. %

\begin{code}
putHalvePeano :: [a] -> [a]
putHalvePeano xs' | lengthPeano xs' == S Z = xs' ++ [()]
\end{code}

We show the evaluation steps of the the expression |lengthPeano v == S
Z where v free | in Figure~\ref{fig:lengthPEval}. %

\phantomsection
\numberson
\numbersright
\begin{figure}
\begin{spec}
lengthPeano v == S Z
  where v free

==

lengthPeano ([] ? _x3:xs) == S Z
  where _x3,xs free

==

(lengthPeano [] ? lengthPeano (_x3:xs)) == S Z
  where _x3,xs free

==

lengthPeano [] == S Z ? lengthPeano (_x3:xs) == S Z
  where _x3,xs free

==

Z == S Z ? S (lengthPeano xs) == S Z
  where xs free

==

Z == S Z ? lengthPeano xs == Z
  where xs free

==

False ? lengthPeano [] ? lengthPeano (_x4:_x5) == Z
  where _x4,_x5 free

==

False ? lengthPeano [] == Z ? lengthPeano (_x4:_x5) == Z
  where _x4,_x5 free

==

False ? Z == Z ? S (lengthPeano _x5) == S
  where _x5 free

==

False ? True ? False
\end{spec}
\caption{Guessing a list with size one with peano numbers}
\label{fig:lengthPEval}
\end{figure}
\numbersreset
\numbersoff
%
The actual evaluation of the given expression yields the following result in KiCS2's
interactive environment. %

\begin{code}
> lengthPeano v == S Z where v free
{v = []} False
{v = [_x3]} True
{v = (_x3:_x4:_x5)} False
\end{code}

Moeover, the expression |putHalve [(),()] v == [(),()] where v free|
evaluates to |{v = [()]} True|.

The main difference to the first implementation is that length can
propagate the constructor at the front of the remaining evaluation. %
That is, the nested |?|-operators only occur as the argument of a
sequence of |S|-constructors, which leads to a terminating search. %
Line 25-28 of the example in Figure~\ref{fig:lengthPEval} show that no
further guesses for free variables are necessary, because the partial
evaluation of |S n| can never be evaluated to |Z|. %
Hence, the expression fails and the evaluation terminates. %

\subsubsection*{Approach Two: Binary List Representation}

The second approach is to choose another list representation. %
In particular, we are interested in a representation that behaves well
with the internal |BinInt| data structure. %
Therefor, we use the following definition of binary lists. %

\begin{code}
data L a = LIHi a | LO (L (a,a)) | LI (L (a,a)) a
data BinaryList a = Empty | NonEmpty (L a)
\end{code}

We define the data structure for non-empty lists that corresponds to
binary numbers. %
|LIHi a| is a list with one element, |LO (L (a,a))| represents a list
with at least two elements, and |LI (L (a,a)) a| is the constructor
for an at least three-valued list. %
Since this data structure has no representation for an empty list, we
introduce an additional data type |BinaryList| that wraps a
constructor |NonEmpty| around the list representation |L a|. %
The second constructor |Empty| represents empty lists. %

\begin{code}
lengthBList :: BinaryList a -> BinInt
lengthBList Empty           = Zero
lengthBList (NonEmpty list) = Pos (lengthL list)
 where
  lengthL :: L a -> Nat
  lengthL (LIHi _) = IHi
  lengthL (LO l)   = O (lengthL l)
  lengthL (LI l _) = I (lengthL l)
\end{code}

With the given type |BinaryList| we have a special constructor for
non-empty lists with an inner representation.  Therefore, we can
propagate |Pos| for a non-empty list without evaluating the actual
inner list that |NonEmpty| is holding. %
The list structure reflects which |Nat|-constructor to use, so that
the constructor is again propagated to the front of the expression. %

We define a corresponding version |putHalveBinaryList| with our new
representation of lists. %

\begin{code}
putHalveBinaryList :: BinaryList a -> BinaryList a -> BinaryList a
putHalveBinaryList xs' | lengthBList xs' == Pos IHi = xs' ++ [()]
\end{code}

In Figure~\ref{fig:lengthBEval}, we show the evaluation steps for the expression
|lengthBList v == Pos IHi where v free|. %

\numberson
\numbersright
\begin{figure}
\begin{spec}
lengthBList v == Pos IHi
  where v free

==

lengthBList (Empty ? NonEmpty xs) == Pos IHi
  where xs free

==

(lengthBList Empty ? lengthBList (NonEmpty xs)) == Pos IHi
  where xs free

==

lengthBList Empty == Pos IHi ? lengthBList (NonEmpty xs) == Pos IHi
  where xs free

==

Zero == Pos IHi ? Pos (lengthL xs) == Pos IHi
  where xs free

==

False ? Pos (lengthL (LIHI _x2 ? LO _x2 ? LI _x2 _x3) ) == Pos IHi
  where _x2,_x3 free

==

False ? (lengthL (LIHI _x2 ? LO _x2 ? LI _x2 _x3) == Pos IHi
  where _x2,_x3 free

==

False  ? (lengthL (LIHi _x2)
       ? lengthL (LO _x2)
       ? lengthL (LI _x2 y)) == Pos IHi
  where _x2,_x3 free

==
\end{spec}
\end{figure}
\todo{Stupid floating figure!}
\begin{figure}
\ContinuedFloat
\begin{spec}
False  ? lengthL (LIHi _x2) == IHi
       ? (lengthL (LO _x2) ? lengthL (LI _x2 _x3)) == Pos IHi
  where _x2,_x3 free

==

False ? IHi == IHi ? (O (lengthL _x2) ? I (lengthL _x2 _x3)) == Pos IHi
  where _x2,_x3 free

==

False ? True ? O (lengthL _x2) == IHi ? I (lengthL _x2 _x3) == Pos IHi
  where _x2,_x3 free

==

False ? True ? False ? False
\end{spec}
\caption{Guessing a binary list with size one}
\label{fig:lengthBEval}
\end{figure}
\numbersoff
\numbersreset

In the end, the expression |putHalveBinaryList v == [(),()] where v free|
yields |{v = NonEmpty (LIHi ())} True|. %

% \subsubsection{The Long and Short of It}

% The usage of free variables and Curry's built-in search have a wide
% range of applications. %
% Unfortunately, the overall environment of Curry is still in a
% experimental stage; 
% We encoutered a problem with more lens definitions, but we limit the
% number of examples in order to go into more detail. %

\section{Summary and Results}

This thesis addresses the topic of bidirectional programming and
lenses in particular. %
The topic is not new in the area of computer science in general nor in
research of programming languages. %
However, we gain a new view on lenses by using a functional logic
programming language like Curry. %
Related approaches concentrate on defining a new infrastructure that
fits bidirectional programming perfectly -- programming languages like
Boomerang and VDL -- or aims one specific domain -- lenses for
relations, strings or trees. %
The advantage of using an exisiting programming language is that the
programmer is in a familiar setting and we can have a wide range of
lens definitions that are not limited to a specific context. %

There exists also promising and well-studied work on
bidirectionalisation techniques that

\begin{itemize}
\item side-product: reactivation of EasyTest, test library for lens
  laws, automated test generator
\item WUILenses
\item printer-parser
\item two small libraries for lenses
\item 
\end{itemize}
\section{Outlook}
\begin{itemize}
\item static analysis for laws \cite{validityCheck}
\item record transformation for KiCS2
\item bidirectionalisation of corresponding get function
\end{itemize}
