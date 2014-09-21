\chapter{Introduction}

In the last 10 years, the topic of bidirectional programming gained
interest in many areas of computer science. %
The work of \cite{biTCombinators} set the trend for bidirectional
transformations in the field of programming languages. %
Broadly speaking, bidirectional transformations are programs that can
run forwards and backwards. %
Most common applications are synchronisation processes where two
similar data structures maintain the same information and need to be
kept in synch, and serialisation processes where two structures are
converted into each other. %

A broad term in the context of bidirectional transformations are
lenses. %
In general, a lens is a pair of functions that operate on two
structures $S$ and $V$ that are defined as follows. %
The value of type $S$ represents the source of the lens and $V$ is the
type of the so-called view. %
%
\begin{align*}
get:~&S \rightarrow V\\
put:~&S \times V \rightarrow S
\end{align*}
%
In the context of lenses, we have a |get| function that describes the
forward direction of a lens and yields the view for a given source. %
The |put| function, which corresponds to the backward direction,
describes the symmetric situation, but needs an additional source as
argument. %
That is, given an original source, the |put| function can produce an
updated source for a given view. %

However, traditional unidirectional programming languages are not
well-suited for bidirectional transformations. %
The programmer has to main of both functions -- get and put --
individually. %
A lot of recent work on bidirectional programming consist of new
programming languages that fit the setting perfectly. %
In these languages, the programmer does not define a transformation
with two functions, but uses predefined combinators to build more
complex transformations. %
Furthermore, many approaches are designed to define only the get
function and the corresponding put function is derived from that
definition. %

\section{Goals and Contributions}

The goal of this thesis is to explore the usage of a functional logic
programming language like Curry for bidirectional programming. %

Recent approches make heavily usage of functional programming
languages like Haskell or define their own functional language that
fits a specific domain of bidirectional programming, e.g. string data,
tree data or relations. %
Curry offers functional features like higher-order function, lazy
evaluation and algebraic data types that enable us to reuse most of
the existing ideas in the context of bidirectional programming. %
On the other hand, Curry is a logic programming language that
automatically provides us with the ability to read function definitions
forward and backwards. %
We can use logic features like free variables in combination with the
built-in search capabilities to define a bidirectional transformations
directly in Curry. %
In particular, we pursue the following goals in this thesis. %

\begin{itemize}
\item We want to evaluate the recent work in the area of bidirectional
  programming that gives enough leeway for follow-up work and with
  regard to their applicability to Curry. %
  As the result of this evaluation, we intend to implement one
  library for lenses that exploits Curry's functional and logic
  features to gain new insights of bidirectional transformations. %
\item A a logic programming language, Curry supports nondeterministic
  function definitions. %
  Thus, we would like to investigate the context of nondeterminism in
  combination with lenses. %
  Is a nondeterministic setting applicable in the context of
  bidirectional programming? %
  Do we need to rethink certain properties of lenses when we allow
  nondeterministic get and put functions? %
\item On top of the implemented lens library, we want to examine
  useful applications for lenses in practical examples. %
  Recent work shows that lenses are highly applicable in context of
  field accessors for algebraic data types. %
  Thus, we want to investigate lenses for record type declarations in
  Curry. %
\end{itemize}

\section{Structure}

The remainder of this thesis is structured as followed. %
Chapter~\ref{ch:curry} and Chapter~\ref{ch:biTrans} provide
introductions to preliminaries of this thesis. %
We give a short introduction to Curry, which we use for the main
implementation of these thesis. %
Readers familar with Curry can skip this introduction and go right to
Chapter~\ref{ch:biTrans}. %
In subsequent chapter, we give a more detailed introduction of
bidirectional programming and lenses in particular. %
We discuss fundamental properties of lenses and the different kinds of
lenses that are defined in related work in Section~\ref{sec:lenses}. %

In Chapter~\ref{ch:impl} we present a detailed insight into related
work and different approaches for bidirectional programming. %
We divide the chapter into two subcategories, Section~\ref{comb}
discusses combinatorial approaches and Section\ref{bi} deals with
related work that use bidirectionalisation techniques. %

Chapter~\ref{ch:curryImpl} and Chapter~\ref{ch:studies} form the main
part of this thesis. %
First, we present two implementations for lenses in Curry: a put-based
combinatorial approach in Section~\ref{sec:implComb} and another
simple put-based library that generates its corresponding get function
in Section~\ref{sec:implPut}. %
On top of the second implementation, we study an application of lenses
in Section~\ref{sec:printerParser} unifies the specification of
pretty-printer and parser into one lens definition %
In Section~\ref{sec:records}, we propose a series of transformations
to generate lenses as field accessors when defining record type
declarations in Curry. %

Finally, we discuss emerging challenges with Curry and conclude in
Chapter~\ref{ch:conclusion}. %