%format "///" = "\vdots"
% definitions for equation environment
\def\commentbegin{\quad\{\ }
\def\commentend{\}}
\chapter{Conclusion}\label{ch:conclusion}
% \epigraph{\itshape{``The scientific man does not aim at an immediate result. He does not
% expect that his advanced ideas will be readily taken up. His work is
% like that of the planter — for the future. His duty is to lay the
% foundation for those who are to come, and point the way.''}}
% {\textsc{Nikola Tesla}}
\epigraph{\itshape{``We can only see a short distance ahead, but we
    can see plenty there that needs to be done.''}}
{\textsc{Alan Turing}}
\noindent
We want to conclude this thesis with a summary of our work, the
highlights of our accomplished results, and an outlook on future
work. %
Upcoming challenges include further research on Curry's built-in
search component, and further improvements of our lens
implementation. %
Additionally, we propose the integration of lenses in the KiCS2
compiler in the context of record type declarations as well as the
reimplementation of the
@WUI@\footnote{\url{http://www-ps.informatik.uni-kiel.de/kics2/lib/CDOC/WUI.html}}
library. %

\section{Summary and Results}

This thesis addresses the topic of bidirectional programming and
lenses in particular. %
Even though this topic has been investigated in great detail in the
past, we gain a new view on lenses by using a functional logic
programming language like Curry. %

Related approaches concentrate on defining a new infrastructure that
fits bidirectional programming perfectly -- programming languages like
Boomerang and VDL -- or targets a specific domain -- lenses for
relations, strings, or trees. %
In this thesis, we do not create a new programming language, but use
Curry and leverage its capabilities regarding nondeterminism, and the
built-in search to gain a new bidirectionalisation approach for
lenses. %
Due to its similarities to Haskell, our approach in Curry affords a
familiar setting for programmers of both languages. %
Furthermore, we can have a wide range of lens definitions that are not
limited to a specific context, but use all facets of the underlying
language, e.g., algebraic data types, higher-order functions,
recursion, and also existing libraries. %

There also exists promising and well-studied work on
bidirectionalisation techniques for get-based lenses that can be used
in Haskell. %
In this thesis, we decided not to follow this traditional approach,
but adopt the idea of put-based lenses. %
Whereas the get-based approach lacks an unique bidirectionalisation of
a suitable put function, we have the possibility to formulate a
sophisticated update strategy in the put-based approach. %
We have implemented two libraries for put-based lenses: one library
pursues a combinatorial approach; the other library follows the idea
of bidirectionalisation in the broadest sense and generates a
corresponding get function on the fly. %
Though the combinatorial approach guarantees well-behavedness of the
underlying lenses, we prefer the usage of the second library for two
reasons. %
Firstly, we are not limited to use a predefined set of combinators. %
Secondly, we had a hard time to get our head around defining more
complex lenses with the predefined combinators; the adopted interface
was not very intuitive to use in practice. %

Moreover, we developed a new notion of nondeterministic lenses and
adopted the existing lens laws to be suitable for a nondeterministic
setting. %
In our opinion, nondeterministic lenses enhance the application of
bidirectional programming to new areas that were not applicable
before. %
We implemented prototypical lenses to unify the specification of
pretty-printers and parsers. %
On top of these printer-parsers, we developed lenses to facilitate a
layout-preserving replacement for a given pretty-printer
specification. %

Another well-suited application for lenses are record type
declarations. %
We proposed a concept for transforming record type declarations in
Curry into a set of lenses. %
We define a lens for each field of the given record to provide a
selector, and an update function by using get and put, respectively. %

As a side-product of testing our implementation, we reactivated the
testing framework @EasyTest@. %
We developed our own testing interface for lens laws to generate
testing values based on @EasyTest@ as well as an automated test
generator. %

\section{Future Directions}

We have several topics for future work in mind. %
Firstly, due to our heavy usage of Curry's built-in search, we ran
into some complications for function definitions that are too
strict. %
These complications are not lens-specific, but a general problem worth
investigating. %
Secondly, we discuss future work in the context of lenses that include
enhancements, and further applications. %

\subsection{Strictness Problems When Searching For Solutions}\label{subsec:strict}

During the testing phase of our put-based lens libraries, we ran into
complications due to Curry's internal structure of integer values in
combination with lists. %
We can isolate these complications to a general problem with the
built-in search: we cannot guess an argument of a function that
evaluates in a strict manner, i.e., the argument needs to be evaluated
completely and no information about the result can be propagated
beforehand. %

In this subsection, we give a detailed insight of Curry's built-in
search capabilities with an example that needs to guess a list with a
specific length. %
We also present two approaches to solve the upcoming problem. %
Both approaches try to harmonise the combination of inter values and
lists. %
The first approach uses an alternative structure for lists; the second
approach uses an alternative structure for integer values. %
However, we think it would be an interesting topic of its own to
investigate function definitions that are too strict to be applicable
for the built-in search component. %

In the following, we work with the put-based lens library that we
presented in Section~\ref{sec:implPut}. %
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
  where n  = length xs `div` 2
\end{code}

|putHalve| takes two lists -- a source list and a view list -- and
concatenates the second list with the second half of the first list. %
Valid view lists have half the length of the source list, if
otherwise, the function yields |failed|, i.e., no result is
produced. %

In the following, our goal is to generate a get function for
|putHalve|. %
The desired |get| function is equivalent to the following definition
of |halve|. %

\begin{code}
halve :: [a] -> [a]
halve xs = take (length xs `div` 2) xs
\end{code}

With the help of our interface, we can derive a corresponding get
function by simply calling |get| with |putHalve| and our source
value. %

\begin{code}
getHalve = get putHalve
\end{code}

Unfortunately, this easy task evolved into a long investigation of
Curry's built-in search mechanism as well as the internal
representation of integer values, and the interaction of integer
values with lists. %

\subsubsection*{The Problem}

First of all, a function call like |getHalve [(),()]| does not
terminate. %
The problem is the combination of the internal representation of lists
and integer values; they do not harmonise well. %
This effect is triggered by the usage of |length|. %
The free variable |v| in the definition of |get| corresponds to |xs'|
in the definition of |putHalve|, i.e., the system guesses values for
|xs'|. %
In order to be more precise, the system needs to guess lists of type
|()| for |xs'| and checks if their length is the same as |length
[(),()] `div` 2|. %
We can evalute the expression |getHalve [(),()]| as follows. %

\begin{spec}
    getHalve [(),()]
==  get putHalve [(),()]
==  putHalve [(),()] v == [(),()] where v free
\end{spec}
\begin{spec}
==  putHalve [(),()] v | length v == n = v ++ drop n [(),()]
      where n  = length [(),()] `div` 2
==  putHalve [(),()] v | length v == n = v ++ drop n [(),()]
      where n = 1
==  putHalve [(),()] v | length v == 1 = v ++ [(),()]
\end{spec}

In order to focus on the cause of the problem, we reduce our
definition of |putHalve| to this explicte example of a list with two
elements. %

\begin{code}
putHalveSimple :: [a] -> [a]
putHalveSimple xs' | length xs' == 1 = xs' ++ drop 1 [(),()]
\end{code}

If we evaluate an expression with a free variable, e.g.,
|putHalveSimple v where v free|, Curry's built-in search component
converts integer value to its internal representation. %
In particular, Curry uses binary numbers as representation for integer
values, plus additional information about the algebraic sign to
represent negative values, positive values, and |0|. %
The data structure |Nat| defines constructors for binary numbers and
|BinInt| is the overall representation, wrapping the binary numbers. %

\begin{code}
data BinInt = Neg Nat | Zero | Pos Nat
data Nat = IHi | O Nat | I Nat
\end{code}

If we replace the usage of integer values in our defintiion of
|putHalveSimple| with the internal representation of |BinInt|, we end
up with the following function. %

\begin{code}
putHalveSimple :: [a] -> [a]
putHalveSimple xs' | length xs' == Pos IHi = xs' ++ drop 1 [(),()]

length' :: [a] -> BinInt
length' []      = Zero
length' (x:xs)  = inc (length' xs)
\end{code}

Additionally, we use a function |length'| that computes the
length of a given list using |BinInt| as well. %
The auxiliary function |inc| increments a |BinInt| value. %

\numberson
\numbersright
\begin{code}
inc :: BinInt -> BinInt
inc Zero         = Pos IHi
inc (Pos n)      = Pos (succ n)
inc (Neg IHi)    = Zero
inc (Neg (O n))  = Neg (pred (O n))
inc (Neg (I n))  = Neg (O n)
\end{code}
\numbersoff
\numbersreset

\subsubsection*{Guessing a List with a Specific Length}

So, how do the evaluation steps look like when we want to compute
the length of a list?

We can directly compute the result for an empty list, but
for a non-empty list we build a sequence of |inc|-operations,
e.g., |inc (inc Zero)| for a two-valued list, |inc (inc (inc Zero))|
for a three-valued list etc. %
In order to take this investigation
one step ahead, we need to look at the definition of |inc|, where only
lines 2 and 3 are of further interest. %
We give the evaluation of zero to two consecutive |inc| function calls
in Figure~\ref{fig:incEval}. %
\numberson
\numbersright
\begin{figure}[h!]
\begin{minipage}{0.12\textwidth}
(1)
\end{minipage}
\begin{minipage}{0.85\textwidth}
\begin{spec}
    Zero == Pos IHi
==  False
\end{spec}
\end{minipage}
\begin{minipage}{0.12\textwidth}
(2)
\end{minipage}
\begin{minipage}{0.85\textwidth}
\begin{spec}
    inc (Zero) == Pos IHi
==  Pos IHi == Pos IHi
==  True
\end{spec}
\end{minipage}
\begin{minipage}{0.14\textwidth}
(3)
\end{minipage}
\begin{minipage}{0.85\textwidth}
\begin{spec}
    inc (inc Zero) == Pos IHi
==  inc (Pos IHi) == Pos IHi
==  Pos (succ IHi) == Pos IHi
==  Pos (O IHi) == Pos IHi
==  False
\end{spec}
\end{minipage}
\caption{Evaluation of |inc| for an increasing number of function
  calls}
\label{fig:incEval}
\end{figure}
\numbersoff
\numbersreset

The attentive reader may have already noticed that the definition of
|inc| is strict and does not propagate its constructor. %
The successor function on binary numbers, |succ|, is also strict. %
Hence, in the current implementation with integer values, the list
that Curry guesses is evaluated completely due to the usage of
|length|. %
The |length| function evaluates the whole list to determine its
length; this leads to the construction of lists with increasing length
when using a free variable. %
The built-in search for free variables in KiCS2 can be translated in
nested injections of |?|-operations, where every constructor of the
given type is a possible guess and arguments of constructors are also
free variables. %
For every free variable of type |[a]| both constructors are possible
values, therefore, both expressions are introduced with the
|?|-operator. %
We illustrate the built-in search of our example in
Figure~\ref{fig:lengthEval}. %
%
\numberson
\numbersright
\begin{figure}[h!]
\begin{spec}
    length' v == Pos IHi where v free
==  length' ([] ? _x2:xs) == Pos IHi where _x2,xs free
==  (length' [] ? length _x2:xs) == Pos IHi where _x2, xs free
==  length' [] == Pos IHi ? length' _x2:xs == Pos IHi where _x2,xs free
==  Zero == Pos IHi ? length _x2:xs == Pos IHi where _x2,xs free
==  False ? length' _x2:xs == Pos IHi where _x2,xs free
==  False ? inc (length' xs) == Pos IHi where xs free
==  False ? inc (length' [] ? length' _x4:ys) == Pos IHi where _x4,ys free
==  False ? inc length' [] == Pos IHi ? inc (length' _x4:ys) == Pos IHi
      where _x4,ys free
==  ...
\end{spec}
\caption{Guessing a list with length one}
\label{fig:lengthEval}
\end{figure}
\numbersreset
\numbersoff

The built-in search collects all possible values and works
henceforth with a set of values, i.e., every list of the resulting set
is used for further function calls.
For our example, we get the following results in the interactive
environment of KiCS2. %

\begin{spec}
> length' v == Pos IHi where v free
{v = []} False
{v = [_x2]} True
{v = [_x2,_x4]} False
///
\end{spec}

In summary, the internal structure for lists and numbers do not
harmonise well, we cannot guess the length of a list. %
Unfortunately, the |length| function cannot be implemented in a way
that is sufficient to propagate a constructor, because it is
problematic to map the empty list to a value of type |Nat|. %
How can we solve the problem that |putHalve| and |putHalveSimple|,
respectively, does not terminate? %

% \subsubsection*{Approach One: Natural Numbers}

% As the very first approach, we exchange the usage of |BinInt| by |Nat|
% to see, if this little change will make a difference. %

% \begin{code}
% lengthNat :: [a] -> Nat
% lengthNat [_]      = IHi
% lengthNat (_:y:ys) = succ (lengthNat (y:ys))
% \end{code}

% Up to now, it seems that the built-in search cannot terminate, because
% the |Pos| constructor takes too long to be propagated. %
% We test the expression |lengthNat v == IHI where v free| and give the
% evaluation steps in Figure~\ref{fig:lengthEvalNat}. %

% \begin{figure}[p]
% \begin{spec}
% lengthNat v == IHI where v free

% ==

% lengthNat ([] ? _x2:xs) == IHi where _x2,xs free

% ==

% (lengthNat [] ? lengthNat (_x2:xs)) == IHi where _x2,xs free

% ==

% lengthNat [] == IHi ? lengthNat (_x2:xs) == IHi where _x2,xs free

% ==

% False ? lengthNat (_x2:xs) == IHi where _x2,xs free

% ==

% False ? lengthNat (_x2:([] ? (_x4:ys))) == IHI where _x2,_x4,ys free

% ==

% ///

% ==

% False ? lengthNat [_x2] == IHI ? lengthNat (_x2:_x4:ys) == IHi
%   where _x2 _x4,ys

% ==

% False ? IHi == IHi ? succ (lengthNat (_x4:ys)) == IHi
%   where _x4,ys free

% ==

% False ? True ? succ (lengthNat (_x4:([] ? (_x6:zs)))) == IHi
%   where _x4,_x6,zs

% ==

% ///
% \end{spec}
% \caption{Guessing a list with length one with natural numbers}
% \label{fig:lengthEvalNat}
% \end{figure}

% However, the expression does not terminate for |Nat| values either. %
% We have to think of more sophisticated changes to successfully
% evaluate the expression and to get actual bindings for the free
% variable. %

\subsubsection*{Approach One: Peano Numbers}

As a first attempt, we use an alternative data structure with an unary
representation for integer values: peano numbers. %

\begin{code}
data Peano  = Zero
            | S Peano
\end{code}

Peano numbers are represented with a constructor for |Zero| and a
successor constructor |S Peano|. %
The corresponding length function, |lengthPeano|, introduces an
|S|-constructor for every element of the list and yields |Zero| for an
empty list. %

\begin{code}
lengthPeano []     = Z
lengthPeano (x:xs) = S (lengthPeano xs)
\end{code}

Let us take a look at the simplified implementation of |putHalvePeano|
that uses peano numbers instead of integer values. %

\begin{code}
putHalvePeano :: [a] -> [a]
putHalvePeano xs' | lengthPeano xs' == S Z = xs' ++ [()]
\end{code}

We evaluate the the expression |lengthPeano v == S Z where v free | in
Figure~\ref{fig:lengthPEval}. %
%
\numberson%
\numbersright%
\begin{figure}[h!]
\begin{spec}
    lengthPeano v == S Z where v free
==  lengthPeano ([] ? _x3:xs) == S Z where _x3,xs free
==  (lengthPeano [] ? lengthPeano (_x3:xs)) == S Z where _x3,xs free
==  lengthPeano [] == S Z ? lengthPeano (_x3:xs) == S Z where _x3,xs free
==  Z == S Z ? S (lengthPeano xs) == S Z where xs free
==  Z == S Z ? lengthPeano xs == Z where xs free
==  False ? lengthPeano [] ? lengthPeano (_x4:_x5) == Z
      where _x4,_x5 free
==  False ? lengthPeano [] == Z ? lengthPeano (_x4:_x5) == Z
      where _x4,_x5 free
==  False ? Z == Z ? S (lengthPeano _x5) == Z where _x5 free
==  False ? True ? False
\end{spec}
\caption{Guessing a list with length one with peano numbers}
\label{fig:lengthPEval}
\end{figure}
\numbersreset
\numbersoff
%
The actual evaluation of the given expression yields the following result in KiCS2's
interactive environment. %

\begin{spec}
> lengthPeano v == S Z where v free
{v = []} False
{v = [_x3]} True
{v = (_x3:_x4:_x5)} False
\end{spec}

For this implementation, the evaluation terminates after three
steps. %
The get direction of |putHalve| also yields convenient results: the
expanded version of the get direction yields the successful binding;
the actual function |getHalve| yields only the resulting value as
desired. %

\begin{spec}
> putHalve [(),()] v == [(),()] where v free
v = [()]} True

> getHalve [(),()]
[()]
\end{spec}

The main difference to the first implementation is that |lengthPeano|
can propagate the constructor of the underlying data structure -- |S|
or |Z| -- to the front of the remaining evaluation. %
As the guessed list grows, the newly introduced |?|-operators occur
only as the argument of an |S|-constructor. %
This construction finally leads to a terminating search, because once
we overreach the number of |S|-constructors, the remaining argument of
that constructor is not evaluated anymore. % %
Lines 12-13 of the example in Figure~\ref{fig:lengthPEval} show that
no further guesses for free variables are necessary, because the
partial evaluation of |S n| can never be evaluated to |Z|. %
Hence, the expression |S n == Z| evaluates to |False| and the
evaluation of the whole expression terminates. %

\subsubsection*{Approach Two: Binary Lists}
The second approach is to choose an alternative representation for
lists. %
In particular, we are interested in a representation that behaves well
with the internal |BinInt| data structure. %
Therefore, we use the following definition of binary lists. %

\begin{code}
data L a = LIHi a | LO (L (a,a)) | LI (L (a,a)) a
data BinaryList a = Empty | NonEmpty (L a)
\end{code}

We define a data structure for non-empty lists that corresponds to
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

The definition of the length function |lengthBList| that computes the
length of a |BinaryList| with the |BinInt| data structure benefits
from underlying structure of the given list. %
The |BinaryList| has a special constructor for non-empty lists with an
inner representation. %
Therefore, we can propagate |Pos| for a non-empty list without
evaluating the actual list, i.e., the argument of the
|NonEmpty|-constructor. %
The three different constructors of the binary list structure reflect
which |Nat|-constructor to use, such that the constructor is
propagated to the front of the expression, again. %

We define a corresponding version |putHalveBinaryList| with our new
representation of lists. %

\begin{code}
putHalveBinaryList :: BinaryList a -> BinaryList a -> BinaryList
putHalveBinaryList xs'
  | lengthBList xs' == Pos IHi =xs' ++ listToBinaryList [()]
\end{code}

The auxiliary function |listToBinaryList :: [a] -> BinaryList a|
converts a given traditional list representation to the
corresponding binary list. %

Once again, we want to test our solution by running the get direction
of the lens for a list of length one. %
We reduce the query to the expression |lengthBList v == Pos IHi where
v free|, which yields the following result. %

\begin{spec}
> lengthBList v == Pos IHi where v free
{v = Empty} False
{v = NonEmpty (LIHi _x2)} True
{v = NonEmpty (LO _x2)} False
{v = NonEmpty (LI _x2 _x3)} False
\end{spec}

The evaluation terminates after computing four values to bind the free
variables; a detailed evaluation of the expression is given in
Figure~\ref{fig:lengthBEval}. %
Lines 18-24 illustrate the propagation of the convenient binary
number's constructor for the corresponding constructor of the binary
list quite well. %
In the subsequent lines -- 25-28 -- the evaluation terminates due the
propagated constructors |Pos (I _)| and |Pos (O _)|, respectively,
that can be finally compared with |Pos IHi|. %

\numberson
\numbersright
\begin{figure}[h!]
\begin{spec}
    lengthBList v == Pos IHi where v free
==  lengthBList (Empty ? NonEmpty xs) == Pos IHi where xs free
==  (lengthBList Empty ? lengthBList (NonEmpty xs)) == Pos IHi
      where xs free
==  lengthBList Empty == Pos IHi ? lengthBList (NonEmpty xs) == Pos IHi
      where xs free
==  Zero == Pos IHi ? Pos (lengthL xs) == Pos IHi where xs free
==  False ? Pos (lengthL (LIHI _x2  ? LO _x2
                                    ? LI _x2 _x3) ) == Pos IHi
      where _x2,_x3 free
==  False ? Pos (lengthL (LIHI _x2  ? LO _x2
                                    ? LI _x2 _x3) == Pos IHi
      where _x2,_x3 free
==  False  ? Pos (lengthL (LIHi _x2)  ? lengthL (LO _x2)
                                      ? lengthL (LI _x2 y)) == Pos IHi
      where _x2,_x3 free
==  False  ? Pos (lengthL (LIHi _x2)) == Pos IHi
           ? Pos (lengthL (LO _x2) ? lengthL (LI _x2 _x3)) == Pos IHi
      where _x2,_x3 free
\end{spec}
\phantomcaption
\end{figure}
\begin{figure}
\ContinuedFloat
\begin{spec}
==  False  ? Pos IHi == Pos IHi
           ? Pos (O (lengthL _x2) ? I (lengthL _x2 _x3)) == Pos IHi
      where _x2,_x3 free
==  False  ? True
           ? Pos (O (lengthL _x2)) == Pos IHi
           ? Pos (I (lengthL _x2 _x3)) == Pos IHi
      where _x2,_x3 free
==  False ? True ? False ? False
\end{spec}
\caption{Guessing a binary list with length one}
\label{fig:lengthBEval}
\end{figure}
\numbersoff
\numbersreset

In the end, the expression |get putHalveBinaryList (NonEmpty (LO (L
((),()))))| yields |NonEmpty (LIHi ())| as desired. %
% \subsubsection{The Long and Short of It}

% The usage of free variables and Curry's built-in search have a wide
% range of applications. %
% Unfortunately, the overall environment of Curry is still in a
% experimental stage; 
% We encoutered a problem with more lens definitions, but we limit the
% number of examples in order to go into more detail. %

\subsection{Further Directions}
Unfortunately, our preferred put-based lens library does not guarantee
well-behavedness by construction. %
This lack of well-behavedness has to be tackled in the future. %
We started by defining a test-suite that generates test cases for all
lens definitions of a given module, but the tests still have to be run
manually. %
It would be useful to have static analyses as a replacement
for manual tests. %
\cite{validityCheck} propose two algorithms to check the two essential
laws for put-based lenses -- PutDet and PutStab -- statically. %
However, they define these algorithms on top of a simple, self-defined
language for lenses. %
This language allows only put definitions that are affine and in
treeless form, thus, we cannot directly apply their results in
Curry. %
Nevertheless, we think that their work is a good starting point to get
ideas for statical analyses in the context of put-based lenses. %

Due to the scope of this thesis, we had to lower
our sights regarding record transformations. %
In this thesis, we proposed a series of transformations on record
type declarations to generate lenses as convenient getter and setter
functions instead of the current implementation with syntactical
constructs. %
We have also implemented a prototype that works on the internal
FlatCurry\footnote{\url{http://www.informatik.uni-kiel.de/~curry/flat/}} representation of Curry programs as proof of concept. %
However, we would like to integrate these transformations into the
KiCS2 compiler, and provide a simple lens library with a handful of
primitives. %
For the purpose of these field accessors, it suffices to implement a
simple representation of lenses as a pair of getter and setter
functions. %

Last but not least, we think that the work of \cite{webLenses}
regarding lenses in the context of web development could be very
applicable for Curry. %
In our opinion, lenses perfectly fit the setting of mapping database
entities to user interfaces for two reasons. %
Firstly, these mappings usually project from database entities to user
interfaces, which is a simple and common application for lenses. %
Secondly, possible performance issues of lenses have a smaller impact
in the context of web development, where performance is usually
affected by a communication overhead. %
Curry already provides a library called @WUI@ to specify web user
interfaces; it was implemented by \cite{wui}. %
On top of that, the
\emph{Spicey}\footnote{\url{http://www.informatik.uni-kiel.de/~pakcs/spicey/}}
framework can generate an initial setup of a web-based system from an
entity-relationship description of the underlying data. %
We have already reimplemented the @WUI@ library to use lenses and
changed the \emph{Blog example} provided by Spicey to use our
reimplementation. %
As a future work, we propose to integrate the lens component into the
@WUI@ specification and the Spicey framework in particular. %
