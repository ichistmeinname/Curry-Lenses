\chapter[About Bidirectional Transformations]{About Lenses and Other Bidirectional Shenanigans}\label{ch:BiTrans}

In the setting of data synchronisation and data transformation, many
approaches are error-prone and cumbersome to maintain. %
This problem arises from the application of unidirectional
programming, where bidirectional programming would be way more
suitable. %
Typical examples for such problems are widespread and can be found in
several areas of Computer Science: serialisation or synchronisation
processes, e.g. transforming Safari's bookmarks to be suitable for
Firefox~\citep{boomerang}\todo{check reference again}; printer and
parsers that harmonise in a meaningful way (see
Section~\ref{sec:printerParser}; tight connection between user
interfaces and the underlying data~\citep{constraintMaintainers}. %

In this chapter, we introduce the notion of bidirectional
transformations and give the necessary fundamentals to dive deeper
into the topic of bidirectional programming. %
The first section covers the first attemps of bidirectional
programming and its origin from databases. %
Furthermore, we discuss the basic functionality of bidirectional
transformations. %
In the subsequent section we talk about a generalisation named lenses,
the most important part of this section involves the underlying laws
that apply to lenses as well as first examples of lens definition to
get a better intuition of their usage. %
% As preliminary for further chapters, we cover some algebraic
% properties of lenses, which constitute as a minor disadvantage in a
% more practical setting later. %
% Last but not least, we present some applications that are build on
% lenses. %

\section{Bidirectional Programming}\label{sec:biProg}

Typical problems that are based on bidirectional transformations are
handled with two separate functions. %
That is, one function maps the concrete value to the abstract
representation and one function serves as backward direction, from the
abstract value to the concrete representation. %
This approach is rather error-prone and tedious to maintain, because,
firstly, we have to keep the two functions in sync by hand in order to
guarantee correctness; secondly, changes in one of the representations
affects both functions due to the round-tripping rules that have to be
obeyed. %
This unidirectional programming mechanism is well-studied and many
programmers are familiar with this paradigm. %
In contrast, bidirectional programming is a new approach on a specific
domain of problems, which becomes more and more popular in different
areas of computer science. %
Software engineering, programming languages, databases and graph
transformations are some of the current fields of computer science
that participate in research activities concerning bidirectional
transformations. %
For a more detailed introduction to the cross-discipline of
bidirectional transformation, we recommend the work
of~\cite{biTPerspective}. %
In the remainder of this thesis, we focus on bidirectional
transformation from the perspective of the programming language
community. %

So, what is the new, challenging feature of bidirectional programming
that keeps researchers busy? %
A bidirectional transformations does not consist of two functions like
in the unidirectional way, but of one function that can be read
forward and backward. %
In the literature, we distinguish between a forward function |get :: A ->
B| and a backward function |put :: B -> A|; |A| is most commonly
called the source and |B| is the view. %
The naming convention originates from applications in databases. %
These two functions form, in the easiest approach, a bijection from
|A| to |B| and back. %
We visualise this idea of two types and their corresponding
transformation functions in Figure~\ref{fig:bijective}. %

\begin{figure}[h]
\begin{center}
  \includegraphics[width=0.75\textwidth]{../images/bx3iVC_newFormat.pdf}
\end{center}
\caption{Bijective relation between two types |A| and |B|}
\label{fig:bit}
\end{figure}%

In the next section, we discuss lenses, a more general approach of
bidirectional transformation, which, additionally, are one of the most
popular forms in bidirectional programming. %
A statement about the status quo of bidirectional programming is
postponed to Chapter~\ref{ch:impl} and Section ~\ref{sec:implPut},
where we disucess several implementation approaches. %
Another good source for further reading is the paper
by~\cite{biPApproaches} that compares three different approaches of
bidirectional programming with one another. %

\section{Lenses}\label{sec:lenses}
Lenses describe bidirectional transformations that originate in
databases as introduced by \cite{viewUpdate}. %
In the setting of lenses, the |get| function describes a
transformation from |A| to |B|, in most applications |B| is a part
of |A|, and information are discarded from |A| to |B|, respectively. %
On the other hand, the |put| function synchronises a given,
potentially updated, view with respect to the original source. %
A popular example from databases shows the correspondences quite well:
we have a database with a set of data |S| and a query that yields a
table |T| that matches the given criteria. %
The query is the forward transformation |get|. %
In a second step, we modify the resulting table, because we recognise
a misspelled name field or suchlike, which yields to a updated table
|T'|. %
We definitely want to propagate the update back to our database; this
is where the |put| function comes into play. %
The |put| function synchronises our changes of the view, |T'|, with
the original database set |S|. %
Figure~\ref{fig:bit} illustrates the discussed situation of an updated
view, which is synchronised with its original source. %

%
\begin{figure}[h]
\begin{center}
  \includegraphics[width=0.75\textwidth]{../images/bx8iVC.pdf}
\end{center}
\caption{Bidirectional transformations consist of a pair of
    functions: |get| and |put|}
\label{fig:bit}
\end{figure}%


Furthermore, we distinguish two characteristics of lenses:
\emph{symmetric} and \emph{asymmetric} lenses. %
The typical case is an asymmetric setting. %
As we stated at the beginning of the section, in most applications the
view is a subset of the source. %
That is, the |get| function discards some information, when it
transforms a source of type |A| to a view of type |B|. %
The names \emph{asymmetric} and \emph{symmetric} describes the focus
on the given pair of source and view. %
In an asymmetric setting, we only consider changes of the view that
will be propagated back to the source; this restricted view implies
that the given source does not change in the meantime. %
The definition of the |put| function, which we introduced above, needs
to be adapted for the asymmetric setting. %
We want to synchronise the updated view with a source so that we need
|put| to take the initial source as argument as well. %
In a symmetric setting, both sides can be updated, so that the |get|
function takes an additional argument, just as the definition of |put|
above. %
In the following, we will only examine asymmetric lenses in a detailed
manner. %
For a detailed introduction to symmetric lenses, consider to read the
work of~\cite{symmLenses} or the dissertation
of~\cite{symmetricEditLenses}. %
Furthermore, a detailed listing of different properties that are
applicable for lenses can be studied in the work
of~\cite{biTProperties}. %

\subsection*{PutGet Law}\label{subsec:lensesLaws}

So far, we characterised lenses as a bidirectional transformation with
an adapted |put| function, which allows round-tripping behaviour. %
It is important to state that lenses fulfil certain laws. %
The first law states that, if we update a given source with a specific
view and transform the result to a view afterwards, we get the view
that we just put in. %

\begin{equation}\tag{PutGet}
|get (put s v) = v|
\end{equation}

This law is called \emph{PutGet}: we first |put| a new value in our
source and then try to |get| it out again. %

As an example, let us take a look at a bidirectional transformation
with a pair of |String| and |Integer| as the domain of the source, and
|String| as the view's domain. %
In order to define an appropriate |get| function, we need a get
function of type |(String,Integer) -> String|. %
In Haskell, or Curry, there already exists a function with such a type,
namely |fst|. %

\begin{code}
(sub fst get) :: (String,Integer) -> String
(sub fst get) (str,int) = str
\end{code}

Our function |sub fst get| yields the first component of a pair with
no further changes or adjustments to the value; this definition is
equivalent to |fst|. %
The put function has the type |put :: (String,Integer) -> String ->
(String,Integer)|; we define a function that sets the first component
of a pair with a given string without further ado. %

\begin{code}
(sub fst put) :: (String,Integer) -> String -> (String,Integer)
(sub fst put) (str,int) newStr = (newStr,int)
\end{code}

In order to test the \emph{PutGet} law, we first run the functions on
example values. %

\begin{spec}
> (sub fst put) ("foo",42) "bar" ("bar",42)
> (sub fst get) ("bar",42)
"bar"
\end{spec}

We can even show that the defined pair of get and put behaves
according to the law for every initial value and additional string. %

\begin{proof}
For all $w$, $v$ and $v'$, where $(v,w)$ is of type |(String,Integer)|
and $v'$ is of type |String|, it holds |(sub fst get) ((sub fst put) (v,w) v') = v'|. %
\def\commentbegin{\quad\{\ } \def\commentend{\}}
\begin{spec}
  (sub fst get) (put (v,w) v')
== {- definition of |(sub fst put)| -}
   (sub fst get) (v',w)
== {- definition of |(sub fst get)| -}
  v'
\end{spec}
\end{proof}

\subsection*{GetPut Law}
In addition to the \emph{PutGet} law, lenses are also supposed to
fulfil a second round-tripping criteria. %
The \emph{GetPut} law states that if we get a view out of a source and
put it back again, the source does not change at all, as if nothing
happend. %
This law can be interpreted as a stability property, that is, a lens
is stabil if nothing \emph{magical} happens during an update or a
selection. %

\begin{equation}\tag{GetPut}
|put s (get s) = s|
\end{equation}

\begin{proof}
With our example above, we obtain the following equation, where for all $w$
and $v$ where $(v,w)$ is of type |(String,Integer)|, it holds
|put (v,w) (get (v,w)) = (v,w)|. %
\def\commentbegin{\quad\{\ }
\def\commentend{\}}
\begin{spec}
  put (v,w) (get (v,w))
== {- definition of |get| -}
  put (v,w) v
== {- definition of |put| -}
  (v,w)
\end{spec}
\end{proof}

\begin{figure}[h]
\begin{minipage}{0.47\textwidth}
  \begin{center}
    \includegraphics[width=\textwidth]{../images/ex1-fst-getput}
  \end{center}
\end{minipage}
	\hfill
\begin{minipage}{0.47\textwidth}
  \begin{center}
    \includegraphics[width=\textwidth]{../images/ex1-fst-putget}
  \end{center}
\end{minipage}
\caption{PutGet Law (left) and GetPut Law (right)}
\label{fig:laws}
\end{figure}

In Figure~\ref{fig:laws}, we illustrate both lens laws; the different
colouring of the view distinguishes the original value of the view and
the new updated value. %
In the pioneering work of~\cite{biTCombinators} in the topic of
bidirectional programming and lenses, a lens is called
\emph{well-behaved} if both laws, the \emph{GetPut} and the
\emph{PutGet} law, hold. %

\subsection*{Partial Lenses}
Furthermore, more and more frameworks for bidirectional
transformations and bidirectional programming languages, respectively,
endorse a weaker notion of the presented \emph{PutGet} and
\emph{GetPut} law. %
In our current notion of the laws, we only consider total |get| and
total |put| functions. %
In practice, most of the time we do not want to work with total
functions only. %
For example, the classical function |head :: [a] -> a| to select the
first element of the list is only partially, because we cannot select
an element for the empty list. %
We can define a lens that uses |head| as definition for its get
direction. %
In order to form a lens, we need a put function as well: the put
direction replaces the head element of the given list with a new
element. %

\begin{spec}
head :: [a] -> a
head []      = error "head is undefined for empty lists"
head (x:xs)  = x

replaceHead :: [a] -> a -> [a]
replaceHead []      y = [y]
replaceHead (_:xs)  y = y:xs
\end{spec}

The given definition for the put direction is total, thus, we can
observe the expected behaviour. %
In addition, we make some test function calls to check if the given
lens definition is reasonable in regard to the lens laws. %

\begin{spec}
> replaceHead [1,2,3,4,5] 42
[42,2,3,4,5]

> replaceHead [] 13
[13]

> head (replaceHead [1,2,3,4,5] 10)
10

> head (replaceHead [] 23)
23

> replaceHead [1,2,3,4] (head [1,2,3,4])
[1,2,3,4]
\end{spec}

The first two test expressions show the behaviour of |head|;
it becomes apparent that |head| never yields an empty list as
result. %
Thus, the GetPut law obviously holds for all possible values. %
The last expression is an example with a non-empty lists, where the
PutGet holds as well. %
However, the get direction of the just defined lens, i.e. |head|, is
not defined for empty lists. %
Thus, the given lens does not fulfil the GetPut law for empty lists. %

\begin{spec}
> replaceHead [] (head [])
"head is undefined for empty lists"
\end{spec}

In order to use partial lenses like proposed by~\cite{biTProperties},
we need to adjust the lens laws by means of partiality. %
In the following, the expression $|(f x)|\downarrow$ is satisfied, if
the function $f$ yields a result for the argument $x$. %
We define the partial version of \emph{PutGet} and \emph{GetPut} in
terms of inference rules. %
That is, if the above condition is not satisfied, the equation below
does not need to be checked and the rule trivially holds. %

%
\begin{equation}\tag{Partial-PutGet}
\dfrac{|(put s v)|\downarrow}{|get (put s v) = v|}
\end{equation}
%
\begin{equation}\tag{Partial-GetPut}
\dfrac{|(get s)|\downarrow}{|put s (get s) = s|}
\end{equation}
%

In our example, we check if |head| is defined for
the given source first.

\begin{spec}
> head []
"head is undefined for empty lists"
\end{spec}

Since this is not the case, we do not apply the put direction, because
the condition only needs to be satisfied, if the first application
yields a valid result. %
Thus, the lens consisting of |head| and |replaceHead|
is a valid lens with respect to the PutGet and Partial-GetPut law. %

As a second example, we define a lens with a put function that is
similar to the well-known function |take :: Int -> [a] -> [a]| and a
corresponding get function, which behaves like the function |length ::
[a] -> Int| in Haskell and Curry, respectively. %

\begin{spec}
take []      _  = []
take (x:xs)  n
   | n == 0     = []
   | n > 0      = x : take xs (n-1)
take _       _  = error "take: negative value"

length []      = 0
length (x:xs)  = 1 + length xs
\end{spec}

As a minor adjustment, we define |sub put take| on positive |Integer|
values only to harmonise better with |sub get length|. %
Similar as before, we can observe that |sub get length| only yields
positive |Integer| values as result, thus, the GetPut law holds
trivially for non-empty and empty lists. %

\begin{spec}
> length [1,2,3,4]
4

> take [1,2,3,4,5] 3
[1,2,3]

> take [1,2,3,4] (length [1,2,3,4])
[1,2,3,4]

> take [] (length [])
[]
\end{spec}

Due to the partial definition of |take|, the defined lens does
not fulfil the PutGet law as we can see from the following
expressions. %

\begin{spec}
> length (take (-3) [1,2,3])
"take: negative value"

> take (-1) []
"take: negative value"
\end{spec}

Nevertheless, our second example is a valid lens with respect to GetPut and
Partial-PutGet. %

\subsection*{PutPut Law}
There is also a third lens law, which is called \emph{PutPut}. %
A lens satisfies the \emph{PutPut} law if we run two consecutively
|put| operations on a source with two different views, but only the
second |put| matters. %
That is, we can formulate this law with the following equation.  %

\begin{equation}\tag{PutPut}
|put (put s v) v' = put s v'|
\end{equation}

In most applications, the \emph{PutPut} does not play an important
role, because the preconditions are too strong. %
That is, plenty of constructive well-behaved lens are not very
well-behaved. %
For example the last lens we defined changes the source list
dependent on the given view element, thus, two consecutive calls to
the put function with different view values yield different results. %

\begin{spec}
> take (take [1,2,3,4,5] 1) 3
[1]

> take [1,2,3,4,5] 3
[1,2,3]

> take [1,2,3,4,5] 1
[1]

> take [1] 3
[1]
\end{spec}

In the test expression, we start with the list |[1,2,3,4,5]| and
reduce it to just the first element, i.e., |take [1,2,3,4,5]
1| yields [1]. %
The second application of put reduces the list to the first three
elements; since the list only contains one element, we get |[1]| as
result again. %
The PutPut law states that two consecutive calls have the same effect
as just the latter one. %
In our case, the second put application to the source list
|[1,2,3,4,5]| yields the first three elements, i.e. the resulting list
is [1,2,3], which differs from the result with consecutive put
calls. %

Nevertheless, the PutPut law can be applicabale for a number of
convenient lens definitions. %
For example, the lens consists of |head| and |replaceHead| obeys the
PutPut law. %
In the put direction, we replace the head of a given list; thus, for
two consecutive |replaceHead| actions, only the latter matters. %

\begin{spec}
> replaceHead (replaceHead [1,2,3,4] 13) 42
[42,2,3,4]
\end{spec}

% \subsection{Algebraic properties}\label{subsec:lensesAlgebraic}
% \todo{Limitations concerning injectivity of get-functions!}

% \section{Applications}\label{sec:applications}
% \begin{itemize}
% \item Boomerang \cite{boomerang}
% \item HaXML \cite{haxml}
% \item Model transformation \cite{parsing1}
% \end{itemize}
