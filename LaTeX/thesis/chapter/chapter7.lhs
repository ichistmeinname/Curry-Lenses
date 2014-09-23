%format "///" = "\vdots"
\chapter{Conclusion}\label{ch:conclusion}
Last but not least, we conclude this thesis with a summary of our work
and our accomplished results. %
We also give an outlook on future work. %
Upcoming challenges include further research on
Curry's built-in search component and further improvements of our lens
implementation. %
Additionally, we propose the integration of lenses in the KiCS2
compiler in the context of record type declarations and the
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
Curry and leverage its capabilities regarding nondeterminism and the
built-in search to gain a new bidirectionalisation approach for
lenses. %
Due to its similarities to Haskell, our approach in Curry affords a
familiar setting for programmers of both languages. %
Furthermore, we can have a wide range of lens definitions that are not
limited to a specific context, but use all facets of the underlying
language like algebraic data types, higher-order functions, recursion,
and also existing libraries. %

There also exists promising and well-studied work on
bidirectionalisation techniques for get-based lenses that can be used
in Haskell. %
In this thesis, we decide not to follows this traditional approach and
adopt the idea of put-based lenses. %
Whereas the get-based approach lacks an unique bidirectionalisation of
a suitable put function, we have the possibility to formulate a
sophisticated update strategy in the put-based approach. %
We have implemented two libraries for put-based lenses: one library
pursues a combinatorial approach; the other library follows the idea
of semantic bidirectionalisation in the broadest sense and generates a
corresponding get function on the fly. %
Though the combinatorial approach guarantees well-behavedness of the
underlying lenses, we prefer the usage of the second library for two
reasons. %
Firstly, we are not limited to use a predefined set of combinators. %
Secondly, we had a hard time to get our head around defining more
complex lenses; the adopted interface was not very intuitive to use in
practice. %

Moreover, we developed a new notion of nondeterministic lenses and
adopted the existing lens laws to be suitable for a nondeterministic
setting. %
In our opinion, nondeterministic lenses enhance the application of
bidirectional programming to new areas that were not applicable
before. %
We implemented prototypal lenses to unify the specification of
pretty-printers and parsers. %
On top of these printer-parsers, we developed lenses to facilitate a
layout-preserving replacement for a given pretty-printer
specification. %

Another well-suited application for lenses are record type
declarations. %
We proposed a concept for transforming record type declarations in
Curry into a set of lenses. %
We define a lens for each field of the given record in order to
provide a selector and an update function, i.e., using get and put,
respectively. %

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
enhancements and further applications. %

\subsection{Strictness Problems When Searching For Solutions}

During the testing phase of our put-based lens libraries, we ran into
complications due to Curry's internal structure of |Int| values in
combination with the built-in search mechanism with regard to
strictness. %
In the following, we give a detailed insight of Curry's built-in
search capabilities with an example and the consequential problems. %
We also present two approaches to solve the upcoming problem for the
exemplary lens definition. %
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
 where
  n  = length xs `div` 2
\end{code}

|putHalve| takes two lists, a source list and a view list, and
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
halve xs = take (length xs `div` s) xs
\end{code}

With the help of our interface, we can derive a corresponding get
function by simply calling |get| with |putHalve| and our source
value. %

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
|()| for |xs'| and checks if their length is the same as |length
[(),()] `div` 2|. %

In order to focus on the cause of the problem., we can simplify our definition of
|putHalve|. %

\begin{code}
putHalveSimple :: [a] -> [a]
putHalveSimple xs' | length xs' == 1 = xs' ++ drop 1 [(),()]
\end{code}

Curry uses binary numbers as representation for numbers, plus
additional information about the algebraic sign, also to represent |0|
as well. %
The data structure |Nat| defines constructors for binary numbers and
|BinInt| is the overall representation, wrapping the binary numbers. %

\begin{code}
data BinInt = Neg Nat | Zero | Pos Nat
data Nat = IHi | O Nat | I Nat
\end{code}

If we replace the usage of |Int| values with the internal
representation of |BinInt|, we end up with the following function. %

\begin{code}
putHalveSimple :: [a] -> [a]
putHalveSimple xs' | length xs' == Pos IHi = xs' ++ drop 1 [(),()]

length' :: [a] -> BinInt
length' []     = Zero
length' (x:xs) = inc (length' xs)
\end{code}

Additionally, we define an auxiliary function |length'| that computes
the length of a given list and yields a |BinInt|. %

\subsubsection*{The Problem}

So, how do the evaluation steps look like when we want to compute
the length of a list?

\numberson
\numbersright
\begin{code}
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
We give the evaluation of zero to two consecutive |inc| function calls
in Figure~\ref{fig:incEval}. %
\numberson
\numbersright

% definitions for equation environment
\def\commentbegin{\quad\{\ }
\def\commentend{\}}

\begin{figure}[p]
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
inc (inc Zero) == Pos IHi

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
The successor function on binary numbers, |succ|, is also strict. %
Unfortunately, the |length| function cannot be implemented in a way
that is sufficient to propagate a constructor, because it is
problematic to map the empty list to a value of type |Nat|. %
We discuss the problem concerning |Nat| in further detail later. %

In the end, the |length| function evaluates the whole list to determine
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
\begin{figure}[p]
\begin{spec}
length v == Pos IHi where v free

==

length ([] ? _x2:xs) == Pos IHi where _x2,xs free

==

(length [] ? length _x2:xs) == Pos IHi where _x2, xs free

==

length [] == Pos IHi ? length _x2:xs == Pos IHi where _x2,xs free

==

Zero == Pos IHi ? length _x2:xs == Pos IHi where _x2,xs free

==

False ? length _x2:xs == Pos IHi where _x2,xs free

==

False ? inc (length xs) == Pos IHi where xs free

==

False ? inc (length [] ? length _x4:ys) == Pos IHi  where _x4,ys free

==

False ? inc length [] == Pos IHi  ? inc (length _x4:ys) == Pos IHi
  where _x4,ys free

==

///
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

\begin{spec}
> length v == Pos IHi where v free
{v = []} False
{v = [_x2]} True
{v = [_x2,_x4]} False
...
\end{spec}

As we have seen in the beginning, the internal structure for lists and
numbers do not harmonise well - how can we solve the problem that
|putHalve| does not terminate with the current implementation? %
We will present two different approaches in the following two
subsections. %

\subsubsection*{Approach One: |Nat|}

As the very first approach, we exchange the usage of |BinInt| by |Nat|
to see, if this little change will make a difference. %

\begin{code}
lengthNat :: [a] -> Nat
lengthNat [_]      = IHi
lengthNat (_:y:ys) = succ (lengthNat (y:ys))
\end{code}

Up to now, it seems that the built-in search cannot terminate, because
the |Pos| constructor takes too long to be propagated. %
We test the expression |lengthNat v == IHI where v free| and give the
evaluation steps in Figure~\ref{fig:lengthEvalNat}. %

\begin{figure}[p]
\begin{spec}
lengthNat v == IHI where v free

==

lengthNat ([] ? _x2:xs) == IHi where _x2,xs free

==

(lengthNat [] ? lengthNat (_x2:xs)) == IHi where _x2,xs free

==

lengthNat [] == IHi ? lengthNat (_x2:xs) == IHi where _x2,xs free

==

False ? lengthNat (_x2:xs) == IHi where _x2,xs free

==

False ? lengthNat (_x2:([] ? (_x4:ys))) == IHI where _x2,_x4,ys free

==

///

==

False ? lengthNat [_x2] == IHI ? lengthNat (_x2:_x4:ys) == IHi
  where _x2 _x4,ys

==

False ? IHi == IHi ? succ (lengthNat (_x4:ys)) == IHi
  where _x4,ys free

==

False ? True ? succ (lengthNat (_x4:([] ? (_x6:zs)))) == IHi
  where _x4,_x6,zs

==

///
\end{spec}
\caption{Guessing a list with size one with natural numbers}
\label{fig:lengthEvalNat}
\end{figure}

However, the expression does not terminate for |Nat| values either. %
We have to think of more sophisticated changes to successfully
evaluate the expression and to get actual bindings for the free
variable. %

\subsubsection*{Approach Two: Peano Numbers}

As a second attempt, we use an alternative data structure with an unary
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
Let us take a look at the simplified implementation of
|putHalvePeano|, which uses peano numbers instead of |Int| values. %

\begin{code}
putHalvePeano :: [a] -> [a]
putHalvePeano xs' | lengthPeano xs' == S Z = xs' ++ [()]
\end{code}

We present the evaluation steps of the the expression |lengthPeano v == S
Z where v free | in Figure~\ref{fig:lengthPEval}. %

\phantomsection
\numberson
\numbersright
\begin{figure}[p]
\begin{spec}
lengthPeano v == S Z where v free

==

lengthPeano ([] ? _x3:xs) == S Z where _x3,xs free

==

(lengthPeano [] ? lengthPeano (_x3:xs)) == S Z where _x3,xs free

==

lengthPeano [] == S Z ? lengthPeano (_x3:xs) == S Z
  where _x3,xs free

==

Z == S Z ? S (lengthPeano xs) == S Z where xs free

==

Z == S Z ? lengthPeano xs == Z where xs free

==

False ? lengthPeano [] ? lengthPeano (_x4:_x5) == Z
  where _x4,_x5 free

==

False ? lengthPeano [] == Z ? lengthPeano (_x4:_x5) == Z
  where _x4,_x5 free

==

False ? Z == Z ? S (lengthPeano _x5) == S where _x5 free

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

Moreover, the expression |putHalve [(),()] v == [(),()] where v free|
evaluates to |{v = [()]} True|.

The main difference to the first implementation is that |lengthPeano| can
propagate the constructor at the front of the remaining evaluation. %
With that, the nested |?|-operators only occur as the argument of a
sequence of |S|-constructors, which leads to a terminating search. %
Line 25-28 of the example in Figure~\ref{fig:lengthPEval} show that no
further guesses for free variables are necessary, because the partial
evaluation of |S n| can never be evaluated to |Z|. %
Hence, the expression fails and the evaluation terminates. %

\subsubsection*{Approach Three: Binary List Representation}

The third approach is to choose another list representation. %
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

With the given type |BinaryList| we have a special constructor for
non-empty lists with an inner representation.  Therefore, we can
propagate |Pos| for a non-empty list without evaluating the actual
inner list that |NonEmpty| is holding. %
The list structure reflects which |Nat|-constructor to use, such that
the constructor is propagated to the front of the expression, again. %

We define a corresponding version |putHalveBinaryList| with our new
representation of lists. %

\begin{code}
putHalveBinaryList :: BinaryList a -> BinaryList a -> BinaryList a
putHalveBinaryList xs' | lengthBList xs' == Pos IHi =
  binaryListToList xs' ++ [()]
\end{code}

The auxiliary functions |binaryListToList :: BinaryList a -> [a]|
converts a given |BinaryList a| to a traditional list representation,
|[a]|. %

Once again, we test the get direction of the lens for a list of size
one. %
We reduce the query to the expression |lengthBList v == Pos IHi where
v free|, which yields the following result. %

\begin{spec}
> lengthBList v == Pos IHi where v free
{v = Empty} False
{v = NonEmpty (LIHi _x2)} True
{v = NonEmpty (LO _x2)} False
{v = NonEmpty (LI _x2 _x3)} False
\end{spec}

The evaluation terminates after calculating four values to bind the
free variables; a detailed evaluation of the expression is illustrated
in Figure~\ref{fig:lengthBEval}. %
In the end, the expression |get putHalveBinaryList [(),()]| yields
|NonEmpty (LIHi ())|. %

\numberson
\numbersright
\begin{figure}
\begin{spec}
lengthBList v == Pos IHi where v free

==

lengthBList (Empty ? NonEmpty xs) == Pos IHi where xs free

==

(lengthBList Empty ? lengthBList (NonEmpty xs)) == Pos IHi
  where xs free

==

lengthBList Empty == Pos IHi ? lengthBList (NonEmpty xs) == Pos IHi
  where xs free

==

Zero == Pos IHi ? Pos (lengthL xs) == Pos IHi where xs free

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

False  ? lengthL (LIHi _x2) == IHi
       ? (lengthL (LO _x2) ? lengthL (LI _x2 _x3)) == Pos IHi
  where _x2,_x3 free

==
\end{spec}
\phantomcaption
\end{figure}
\begin{figure}
\ContinuedFloat
\begin{spec}
False  ? IHi == IHi
       ? (O (lengthL _x2) ? I (lengthL _x2 _x3)) == Pos IHi
  where _x2,_x3 free

==

False  ? True
       ? O (lengthL _x2) == IHi
       ? I (lengthL _x2 _x3) == Pos IHi
  where _x2,_x3 free

==

False ? True ? False ? False
\end{spec}
\caption{Guessing a binary list with size one}
\label{fig:lengthBEval}
\end{figure}
\numbersoff
\numbersreset

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
laws for put-based lenses, PutDet and PutStab, statically. %
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
FlatCurry representation of Curry programs as proof of concept. %
However, we would like to integrate these transformations into the
KiCS2 compiler and provide a simple lens library with a handful of
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
