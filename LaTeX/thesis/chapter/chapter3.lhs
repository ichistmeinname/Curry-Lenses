\chapter{Introduction to Curry}\label{ch:curry}

The main implementations we present in this thesis are programmed
with Curry. %
All programs are compiled with KiCS2\footnote{In particular, we use
  KiCS2 version 0.3.1.}, the most recent compiler for Curry that
compiles to Haskell as presented by \cite{kics2}. %
We also use KiCS2's interactive environment to evaluate our
examples. %

Curry is a functional logic programming language similar to
Haskell~\citep{haskell98}, and created by an international development
team to provide an environment, mostly for research and teaching. %
In the following, we assume the reader is au fait with Haskell,
especially its syntax and its type system as well as general
functional concepts like algebraic data types, polymorphism,
higher-order functions and lazy evaluation. %
Hence, we focus on features that are specific to Curry. %
Besides the mentioned functional features, Curry also provides
nondeterminism and free variables as typical characteristics of logic
programming languages. %

In the remainder of this chapter, we will introduce these two logic
features with a series of examples. %
We add explanations to further features of Curry on demand as they
appear in the remainder of this thesis. %

\section{Nondeterminism}
 
In Curry, we define a function with a set of rules. %
As an example, we define a function to yield the first and the last
element of a list, respectively. %

\begin{spec}
head :: [a] -> a
head (x:_) = x

last :: [a] -> a
last [x] = x
last (_:xs) = last xs
\end{spec}

The definition of |head| works just fine in Curry and yields the first
element of a given list. %
Unfortunately, we have to be more careful with overlapping rules in
function de-finitions. %
Instead of matching from top to bottom like in Haskell, Curry
evaluates each matching rule. %
Thus, the definition of |last| is nondeterministic, because a list
with one element matches the first and second rule. %
However, if we use |last| on an exemplary list, we get the desired
result. %

\begin{spec}
> last [1..10]
10
\end{spec}

In the last step of evaluating |last|, we have the expression |last
[10]|, which matches to both given rules. %
In the case of the first rule, we can apply the right-hand side and
yield |10| as result. %
For the second rule, we make an additional function call to the
remaining list. %
However, the expression |last []| does not match for any rule and
silently fails. %
Thus, the expression |last [10]| evaluates to |10|, because a failure
does not yield any result. %

This notion of failure is slightly different to errors in Haskell. %
For example, the expression |head []| raises an error in Haskell --
like |*** Exception: Prelude.head: empty list|, but in Curry the
expression has no result, which is expressed with |!| in the
interactive environment of KiCS2. %

\begin{spec}
> head []
!
\end{spec}

We can use these kind of failures in our program as well by using
|failed :: a|. %
|failed| is a function of Curry's Prelude and has a polymorphic
type. %
Thus, in the case of |head|, we can make the following adjustments in
order to fail for an empty list. %

\begin{spec}
head' :: [a] -> a
head' []    = failed
head' (x:_)  = x
\end{spec}

As an additional example for overlapping rules, we
define a function |member| that nondeterministically yields an element
of a given list. %
We can use an approach similar to the implementation of |last|. %

\begin{spec}
member :: [a] -> a
member (x:_) = x
member (_:xs) = member xs
\end{spec}

Instead of matching for a singleton list in the first rule, we match
for all lists with at least one element. %
Thus, we yield the head element of the list for each recursion step. %

\begin{spec}
> member "Curry"
'C'
'u'
'r'
'r'
'y'
\end{spec}

Furthermore, Curry provides a special operator |?| to introduce
nondeterminism; this choice operator yields one of its arguments
nondeterministically. %

\begin{spec}
(?) :: a -> a -> a
x ? _ = x
_ ? y = y
\end{spec}

With this operator at hand, we can rewrite our implementation of
|member| without using overlapping rules. %

\begin{spec}
member' :: [a] ->a
member' (x:xs) = x ? member' xs
\end{spec}
\begin{spec}
member'' :: [a] -> a
member'' = foldr1 (?)
\end{spec}

The first example unifies the original rules into one rule by using
the choice operator. %
Finally, we beautify this implementation and use |foldr1| instead of
an explicite recursive definition in the second example. %

\section{Free Variables}
The second logic feature of Curry that we want to discuss in greater
detail is free variables. %
Free variables are unbound variables that can be used as data
generators. %
For instance, assume that we have the first part of a list --
|[(),()]|, and want to generate the missing suffix to create the list
|[(),(),()]|. %

\begin{spec}
> [(),()] ++ xs == [(),(),()] where xs free
{xs = []} False
{xs = [()]} True
{xs = (():_x3:_x4)} False
\end{spec}

The free variable |xs| is denoted as such with the keyword |free| and
has the same scope as locally defined functions. %
In order to evaluate the given expression, Curry's built-in search
system generates a series of lists. %
Similar to the evaluation of a nondeterministic expression, we get a
series of results. %
The first component of the result is the binding of the occurring free
variables -- surrounded by curly brackets. %
The evaluated expression is the second component. %
In our example, Curry generates a series of lists starting with the
empty list and stopping for lists that have three or more elements. %
We do not go into further detail here, and postpone further
explanations to Section~\ref{sec:chall}.

The important message to get across here is that we can use Curry's
built-in search capabilities in combination with free variables to use
\emph{generate-and-test} methods in function definitions. %
For example, we can give an additional implementation of |last| by
using free variables. %

\begin{spec}
last' :: [a] -> a
last' xs | _ ++ [y] == xs = y
  where y free
\end{spec}

In this example, we use an anonymous free variable, declared with an
underscore, |_|. %
Anonymous free variables are a syntactical abbreviation for |let x
free in x|. %
If we do not use the binding of the free variable in the remainder of
our expression, we can declare it anonymously. %
The idea of the implementation is to generate the given list in two
steps: an anonymous list for the prefix and a single element to
concatenate at the end of the list. %
Thus, we have the last element at our fingertips and can easily yield
it as result if the condition holds. %
