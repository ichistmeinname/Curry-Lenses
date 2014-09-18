\chapter{Summary}

\section{Challenges}\label{sec:chall}
\subsection{Curry's built-in search abilities}
\todo{This is just a first draft}
We have the following interface for put-based lenses that generate a
corresponding |get|-function for a defined lens. 

\begin{code}
data Lens a b = a -> b -> a
put :: Lens a b ->  a -> b -> a
put lens s v = lens s v

get :: Lens a b -> a -> b
get lens s | put s v == s = v
 where v free
\end{code}

Our aim is to generate a |get|-Function for |putHalve|:

\begin{code}
putHalve :: [a] -> [a] -> [a]
putHalve xs xs' | length xs' == n = xs' ++ drop n xs
 where
  n  = length xs `div` 2
\end{code}

|putHalve| takes to lists, a source list and a view list, and
concatenates the second list with the second half of the first
list. Valid view lists have half the length of the source list, if
otherwise, the function yields |failed|, i.e., no result is produced.
(BeginHint: the wished |get| function is equivalent to

\begin{code}
halve :: [a] -> [a]
halve xs = take (length xs `div` s) xs
\end{code}
EndHint)

With help of our interface, we can derive a corresponding|get| function by simply
calling |get| with |putHalve| and our source value.

\begin{code}
getHalve = get putHalve
\end{code}

Unfortunately, a function call like |getHalve [(),()]| does not terminate.
The problem is the combination of the internal list and
and numbers data structures, they do not harmonize well. This effect
is triggered by the usage of |length|. The free variable |v|
corresponds to |xs'| in the definition of |putHalve|,i.e., the system
guesses values for |xs'|. To be more precise, the system needs to
guess lists of type |()| for |xs'| and check, if their length is the
same as |div length [(),()]  2|. For the specific example above, we
can simplify our definition of |putHalve| in order to focus on the
problematic part.

\begin{code}
putHalveSimple :: [a] -> [a]
putHalveSimple xs' | length xs' == 1 = xs' ++ drop 1 [(),()]
\end{code}

If we now consider the internal structure for |Int| values, we end up
with the following function

\begin{code}
putHalveSimple :: [a] -> [a]
putHalveSimple xs' | length xs' == Pos IHi = xs' ++ drop 1 [(),()]
\end{code}
with respect to the internal data structure |BinInt|.

\begin{code}
data BinInt = Neg Nat | Zero | Pos Nat
data Nat = IHi | O Nat | I Nat
\end{code}

\subsubsection*{Guessing lists with a specific length}

So, how do the evaluation steps look like, when we want to compute
the length of a list?

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

That is, for an empty list, we can directly compute the result, but
for a non-empty list we build a sequence of |inc|-operations,
e.g. |inc (inc Zero)| for a two-valued list, |inc (inc (inc Zero))|
for a three-valued list etc. In order to take this investigation
one step ahead, we need to look at the definition of |inc|, where only
lines 6 and 7 are of further interest.

% definitions for equation environment
\def\commentbegin{\quad\{\ }
\def\commentend{\}}

\begin{figure}
(1)
\begin{spec}
Zero == Pos IHi --> False
\end{spec}

(2)
\begin{spec}
inc (Zero) == Pos IHi
-->
Pos IHi == Pos IHi
-->
True
\end{spec}

(3)
\begin{spec}
inc (inc Zero)) == Pos IHi
-->
inc (Pos IHi) == Pos IHi
-->
Pos (succ IHi) == Pos IHi
-->
Pos (O IHi) == Pos IHi
-->
False
\end{spec}

(4)
\begin{spec}
inc (inc (inc Zero))) == Pos IHi
-->
inc (inc (Pos IHi)) == Pos IHi
-->
inc (Pos (succ IHi)) == Pos IHi
-->
Pos (succ (succ IHi)) == Pos IHi
-->
Pos (succ (O IHi)) == Pos IHi
-->
Pos (I IHi) == Pos IHi
-->
False
\end{spec}
\end{figure}

The attentive reader may have already noticed that the definition of
|inc| is strict in its first argument and does not propagate its constructor, the successor
function on binary numbers is also strict. Unfortunately, the |length|
function cannot be implemented in a way that is sufficient to
propagate a constructor, because, as we will see later, it is
problematic to map the empty list to a value of type |Nat|.
Therefore, the whole list has to be evaluated in order to determine its length, which leads to
non-evaluation when guessing a list with a specific length. The
built-in search for free variables in KICS2 can be translated in
nested injections of |?|-operations, where every constructor of the
given type is a possible guess and arguments of constructors are also
free variables. For our example, we can illustrate the
built-in search as follows.

\begin{figure}
\begin{spec}
length v == Pos IHi
  where v free

-->

length ([] ? _x2:xs) == Pos IHi
  where _x2,xs free

-->

(length [] ? length _x2:xs) == Pos IHi
  where _x2, xs free

-->

length [] == Pos IHi ? length _x2:xs == Pos IHi
  where
_x2,xs free

-->

Zero == Pos IHi ? length _x2:xs == Pos IHi
  where _x2,xs
free

-->

False ? length _x2:xs == Pos IHi
  where _x2,xs free

-->

False ? inc (length xs) == PosIHi
  where xs free

-->

False ? inc (length [] ? length _x4:ys) == PosIHi
  where
_x4,ys free

-->

False ? inc length [] == Pos IHi  ? inc (length _x4:ys) == PosIHi
  where _x4,ys free

-->

...
\end{spec}
\end{figure}

\begin{code}
{v = []} False
{v = [_x2]} True
{v = [_x2,_x4]} False
...
\end{code}

The given type is |[a]| with two constructors |[]| for an empty list
and |(:) x xs| for non-empty lists with an element |x| and the
remaining list |xs|. In our example, for every free variable of type
|[a]| both constructors are possible values, therefore, both
expressions are introduced with the |?|-operator. The |?|-operator
explicitly inserts non-determinism and is defined as follows.

\begin{code}
(?) :: a -> a -> a
x ? y = x
x ? y = y
\end{code}

In the end, the built-in search collects all possible values and
works henceforth with a set of values, i.e., every list of the resulting
set is used for further function calls.

As we said in the beginning, the internal structure for lists and
numbers do not harmonise well - how can we solve the problem that
|putHalve| does not terminate with the current implementation? We will
present two different approaches. Beforehand, we exchange the usage of
|BinInt| by |Nat| to see, if this little change will do the
difference. Up to now, it seems the built-in search cannot termine,
because the |Pos| constructor takes to long to be propagated in
front of the expression.

\begin{code}
lengthNat :: [a] -> Nat
lengthNat [_]      = IHi
lengthNat (_:y:ys) = succ (lengthNat (y:ys))
\end{code}

\begin{figure}
\begin{spec}
lengthNat v == IHI
  where v free

-->

lengthNat ([] ? _x2:xs) == IHi
  where _x2,xs free

-->

(lengthNat [] ? lengthNat (_x2:xs)) == IHi
  where _x2,xs free

-->

lengthNat [] == IHi ? lengthNat (_x2:xs) == IHi
  where _x2,xs
free

-->

failed ? lengthNat (_x2:xs) == IHi
  where _x2,xs free

-->

failed ? lengthNat (_x2:([] ? (_x4:ys))) == IHI
  where _x2,_x4,ys free

--> .. -->

failed ? lengthNat [_x2] == IHI ? lengthNat (_x2:_x4:ys) == IHi
  where _x2 _x4,ys

-->

failed ? IHi == IHi ? succ (lengthNat (_x4:ys)) == IHi
  where _x4,ys free

-->

failed ? True ? succ (lengthNat (_x4:([] ? (_x6:zs)))) == IHi
  where _x4,_x6,zs

-->

...
\end{spec}
\end{figure}

\subsubsection{Peano numbers}
At first, we use another data structure for numbers that has an unary representation, i.e. we will
use peano numbers.

\begin{code}
data Peano = Zero | S Peano

lengthPeano :: [a] -> Peano
-- lengthPeano = foldr (const S) Z
lengthPeano []     = Z
lengthPeano (x:xs) = S (lengthPeano xs)
\end{code}

Peano numbers are represented with a constructor for |Zero| and a
successor constructor |S Peano|. The corresponding |length| function
introduces an |S|-constructor for every element of the list and yields
|Zero| for an empty list.
Let us now take a look at the simplified implementation
of |putHalvePeano| that uses peano numbers instead of |Int|
values and works on a list with two elements.

\begin{code}
putHalvePeano :: [a] -> [a]
putHalvePeano xs' | lengthPeano xs' == S Z = xs' ++ [()]
\end{code}

\begin{figure}
\begin{spec}
lengthPeano v == S Z
  where v free

-->

lengthPeano ([] ? _x3:xs) == S Z
  where _x3,xs free

-->

(lengthPeano [] ? lengthPeano (_x3:xs)) == S Z
  where _x3,xs free

-->

lengthPeano [] == S Z ? lengthPeano (_x3:xs) == S Z
  where _x3,xs free

-->

Z == S Z ? S (lengthPeano xs) == S Z
  where xs free

-->

Z == S Z ? lengthPeano xs == Z
  where xs free

-->

False ? lengthPeano [] ? lengthPeano (_x4:_x5) == Z
  where _x4,_x5 free

-->

False ? lengthPeano [] == Z ? lengthPeano (_x4:_x5) == Z
  where _x4,_x5 free

-->

False ? Z == Z ? S (lengthPeano _x5) == S
  where _x5 free

-->

False ? True ? False
\end{spec}
\end{figure}

In the end, the expression yields the following result in KICS2:

\begin{code}
{v = []} False
{v = [_x3]} True
{v = (_x3:_x4:_x5)} False
\end{code}

That is, the expression |putHalve [(),()] v == [(),()] where v free|
evaluates to |{v = [()]} True|.

The main difference to the first implementation is that length can
propagate the constructor at the front of the remaining
evaluation. That is, the nested |?|-operators only occur as the argument of  a sequence of
|S|-constructors, which leads to a terminating search. The last line
of the example shows that no further guesses for free variables are
necessary, because the partial evaluation of |S n| can never be
evaluated to  |Z|, hence, the expression fails and the evaluation terminates.

\subsubsection{Binary List Representation}

The second approach is to choose another list representation, more
precisely, a representation that behaves well with the internal
|BinInt| data structure. We define binary lists as follows.

\begin{code}
data L a = LIHi a | LO (L (a,a)) | LI (L (a,a)) a
data BinaryList a = Empty | NonEmpty (L a)
\end{code}

At first, we define the data structure for non-empty lists that
corresponds to binary numbers, where |LIHi a| is a list with one
element, |LO (L (a,a))| represents a list with at least two elements,
and |LI (L (a,a)) a| is the constructor for an at least three-valued
list. Since this data structure has no representation for an empty
list, we introduce an additional data type |BinaryList| that wraps a
constructor |NonEmpty| around the list representation |L a| and
consists of a constructor |Empty| for an empty list, respectively.

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

With the given type |BinaryList| we can utilize that we have a special
constructor for non-empty lists with an inner
representation. Therefore, we can propagate |Pos| for an non-empty
list without evaluating the actual inner list that |NonEmpty| is
holding. Furthermore, the list structure reflects which
|Nat|-constructor to use, so that, the constructor is again propagated
to the front of the expression. Again, let us see the evaluation in
action for the |putHalve| example.

\begin{code}
putHalveBinaryList :: BinaryList a -> BinaryList a -> BinaryList a
putHalveBinaryList xs' | lengthBList xs' == Pos IHi = xs' ++ [()]
\end{code}

The following evaluation steps through the expression |lengthBList v
== Pos IHi where v free| that is essential for the evaluation of |get
putHalveBinaryList| applied to an arbitrary list.

\begin{figure}
\begin{spec}
lengthBList v == Pos IHi
  where v free

-->

lengthBList (Empty ? NonEmpty xs) == Pos IHi
  where xs free

-->

(lengthBList Empty ? lengthBList (NonEmpty xs)) == Pos IHi
  where xs free

-->

lengthBList Empty == Pos IHi ? lengthBList (NonEmpty xs) == Pos IHi
  where xs free

-->

Zero == Pos IHi ? Pos (lengthL xs) == Pos IHi
  where xs free

-->

False ? Pos (lengthL (LIHI _x2 ? LO _x2 ? LI _x2 _x3) ) == Pos IHi
  where _x2,_x3 free

-->

False ? (lengthL (LIHI _x2 ? LO _x2 ? LI _x2 _x3) == IHi
  where _x2,_x3 free

-->

False ?  (lengthL (LIHi _x2) ? lengthL (LO _x2) ? lengthL (LI _x2 y)) == IHi
  where _x2,_x3 free

-->

False ? lengthL (LIHi _x2) == IHi ? (lengthL (LO _x2) ? lengthL (LI
_x2 _x3)) == IHi
  where _x2,_x3 free

-->

False ? IHi == IHi ? (O (lengthL _x2) ? I (lengthL _x2 _x3)) == IHi
  where _x2,_x3 free

-->

False ? True ? O (lengthL _x2) == IHi ? I (lengthL _x2 _x3) == Pos IHi
  where _x2,_x3 free

-->

False ? True ? False ? False
\end{spec}
\end{figure}

In the end, this expression yields the following result in KICS2:

\begin{code}
{v = Empty} False
{v = (NonEmpty (LIHi _x2))} True
{v = (NonEmpty (LO _x2))} False
{v = (NonEmpty (LI _x2 _x3))} False
\end{code}

That is, the expression |putHalveBinaryList v == [(),()] where v free|
yields |{v = NonEmpty (LIHi ())} True| and |get putHalveBinaryList
[(),()] | yields |NonEmpty (LIHi ())|, respectively.

\section{Results}
\begin{itemize}
\item side-product: reactivation of EasyTest, test library for lens laws, automated test generator
\end{itemize}
\section{Outlook}
\begin{itemize}
\item static analysis for laws \cite{validityCheck}
\item record transformation for KiCS2
\item bidirectionalisation of corresponding get function
\end{itemize}
