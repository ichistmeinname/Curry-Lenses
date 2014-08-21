\chapter{Different Implementation Approaches}\label{ch:Impl}

\todo{rephrase} Bidirectional programming is a rising topic in the
field of computer science, and many different approaches exist to
tackle the problem. %
These approaches come from different disciplines of computer science
like databases, graph transformation, programming languages and
interface design. %
This section summarises the main two approaches and highlights
differences as well as some details. %

The main two techniques to work with bidirectional transformations are
combinatorial languages and bidirectionalisation. %
Most commonly, a combinatorial language is defined as a DSL in a
general purpose programming language or as a new programming language,
and provides a set of primitives, which can be combined to define
complex structures. %
The definitions of these primitives mostly consist of both functions,
the get and the appropriate put function. %
In contrast to this approach, the bidirectionalisation technique takes
an uni-directional function and synthesises this function to a
bidirectional one. %

The remainder of the chapter introduces combinatorial and
bidirectionalisation approaches for lenses; both approaches have two
subcategories, because the implementation can either focus on defining
a |get| function or a |put| function. %
In this context, we will discuss advantages and disadvantages of
defining a get function and present a first proposal by
\cite{putCombinators} to set the focus on the |put| function. %

\section{Combinatorial lenses}\label{sec:Comb}
The first combinatorial technique is the pioneer work by
\cite{biTCombinators}, who designed a domain-specific programming
language to define bidirectional transformations on tree-structured
data. %
Foster et al formulate fundamental laws concerning lenses\footnote{We
  already presented these laws in Section \ref{sec:Laws}, in
  particular, PutGet, GetPut and PutPut.}, combine these laws with the
intuitive behaviour of lenses, and use fundamental tools from domain
theory to also define lenses by recursion; they lay the focus of the
design of their language on robustness and ease of use. %
That is, their language guarantees well-behaved lens definitions and
the totality of the primitive transformations with the integrated type
system. %
The underlying type system, and with that the type safety, is the main
contribution to the field of bidirectional programming. %
The authors state close connections with topics from the database
community: lenses are a well-known abstraction in databases concerning
tables and queries, and the \emph{update translation under a constant
  complement} introduced by \cite{viewUpdate} tackles problems
concerning definedness \todo{precision?} and continuity, whereas the
property of well-behaved lenses corresponds to \emph{update
  translators}\todo{citation}. %

In their publication, Foster et al. define a handful of primitive lens
combinators for trees, and the combination of these primitive lenses
results in a powerful abstraction to describe transformations. %
The most important primitives are the composition, identity and
constant lens. %
These definitions work on arbitrary data structures, whereas all other
combinators specialise on tree data structures only. %
The defined transformations are closely related to tables and views
from databases: a transformations maps a concrete structure into an
abstract view, and maps a possibly modified abstract view together
with the original concrete structure back to a modified concrete
structure. %

In the DSL, the user defines the forward transformation in a
straight-forward fashion, whereas the backward transformation is the
result of reading the definition from right to left. %

The following expression shows a tree with two labels, |fst| and
|snd|, representing a pair, which corresponds to a pair |(42,"Hello
World")| in Haskell. %
\[  aPair = \left\{ \begin{array}{l}
      ~~\text{fst} \rightarrow 42\\
      ,~\text{snd} \rightarrow \text{"Hello World"}
    \end{array}
  \right\}
\]

As an example, we define a lens that yields the first component of a
pair, like the one we defined above. %
Foster et al. use $\nearrow: S \Leftrightarrow V \times S \rightarrow
V$ and $\searrow:~ S \Leftrightarrow V \times (S,B) \rightarrow S$ as
representation for |get| and |put| functions, respectively, where $S
\Leftrightarrow V$ is a lens with a source of type $S$ and a view of
type $V$.\footnote{Foster et al. use $C$ and $V$ as representative for
  the concrete and abstract value, respectively.} %
We can use the predefined tree combinator |filter p d| to keep
particular children of the tree, where |p| describes the set of names
that we want to keep in the tree, and |d| is used for the |put|
direction as a default value for missing information. %
In the end, we get the following expression to extract the first
component of a given pair.\footnote{We represent the empty tree as
  |{}| and the empty set as $\emptyset$ in order to distinguish
  between these two values.} %
\label{filter:fstGet}
\begin{align*}
  & ~|(filter {fst} {})| \nearrow |aPair| \\
  =& ~|(filter {fst} {})| \nearrow \left\{ \begin{array}{l}
      ~~\text{fst} \rightarrow 42\\
      ,~\text{second} \rightarrow \text{"Hello World"}
    \end{array}
  \right\}\\
  =& ~\left\{ \begin{array}{l}
      \text{fst} \rightarrow 42\\
    \end{array}
  \right\}
\end{align*}

As a second example, we use the same lens to change the first
component of our pair to |13|, i.e., apply the |put| function. %
\label{filter:fstPut}
\begin{align*}
  & ~|(filter {fst} {})| \searrow |(13,aPair)| \\
  =& ~|(filter {fst} {})| \searrow \left(|13|,~
    \left\{ \begin{array}{l}
        ~~\text{fst} \rightarrow 42\\
        ,~\text{snd} \rightarrow \text{"Hello World"}
      \end{array}
    \right\}\right)\\
  =& ~\left\{ \begin{array}{l}
      ~~\text{fst} \rightarrow 13\\
      ,~\text{snd} \rightarrow \text{"Hello World"}
    \end{array}
  \right\}
\end{align*}

The work of Foster et al origins in the Harmony
project\footnote{\url{https://alliance.seas.upenn.edu/~harmony/old/}}
\citeyearpar{relationalLenses,harmonyOverview,harmonyManual}, a
generic framework to synchronise tree-structured data; an ongoing
example throughout their work is the synchronisation of different
browser bookmarks, calendar and address book formats. %
They continued their work on lenses with a project called
Boomerang\footnote{\url{https://alliance.seas.upenn.edu/~harmony/}}
\citeyearpar{boomerang} focuses on string data instead of
tree-structure data; the developer team of
\emph{Augeas}\footnote{\url{http://augeas.net}} uses the Boomerang
language as a framework for their configuration API. %
As well as in their first work, their language is based on a type
system to guarantee well-behaved lenses and total lenses and a rich
set of lens combinators to define powerful transformations on
strings. %
\todo{check statements about type safety again; does only hold for
  primitive lenses; user-defined functions are only checked at run
  time}\\

Other combinatorial approaches for lenses exist, they all focus on
specifying a |get| function, and the appropriate |put| function is
then propagated through the definition of the used |get|
combinators. %
For example, \cite{pointfree} designed a proint-free DSL in Haskell,
in which the programmer also defines the |get| transformation only. %
\cite{putback} are the first to propose to use the |put| definition
instead. %
It seems quite obvious that both, the forward and the backward
function of a bidirectional transformation, can be used for
bidirectionalisation. %
Nevertheless, so far, the current techniques pursue the idea of Foster
et al. %

In the work of Fisher et al., it becomes apparent that typical
problems of |get| definitions are the ambiguity of the derived |put|
functions. %
That is, in several cases it exists more than one appropriate |put|
function to correspond with the |get| definition. %
As we discussed before, these problems concerning ambiguity arrive
when the defined |get| function is not injective. %
This ambiguity can be eliminated when we define the |put| direction
instead. %
Fisher et al. show in their work that the corresponding |get| function
for a defined |put| function is unique if certain requirements apply
to the |put| function. %
They prepare their theorem with some transformations on the
\emph{PutGet} and \emph{GetPut} laws; instead of the classical
representation, they express their requirements based on the |put|
definition only. %
As \cite{putback} state in their technical report, the \emph{PutGet}
law can be reformulated as injectivity property of the |put|
function. %
For that purpose, let us recapitulate the \emph{PutGet} law. %
As a first step, we can express the equation in a more functional
manner. %
\[
  |get (put s v) = v|
\]

The view |v| occurs on both sides of the equation, here, eta reduction
comes to the rescue in order to simplify the equation. %
\[\tag{PutGet'}
  |get . put s = id|
\]

The |(.)| operator defines function composition, that is, the equation
\emph{PutGet'} says that |get| is a left inverse for |put s| for all
sources |s|. %
Furthermore, a function |f :: A-> B| is injective if and only if there
exists a function |g :: B -> A| such that |g . f = (id :: A -> A)|. %
In the \emph{PutGet'} equation above, we have the function |put s :: V
-> S| that corresponds to |f| and the counterpart |get :: S - > V| as
the equivalent to |g|. %
The identity function in the equation above is obviously of type |id
:: V -> V|, because we eta-reduced the view argument |v :: V|, thus,
|V| must be the resulting type as well. %

In the end, we can express the identity property of the first round-
tripping rule \emph{GetPut} with the use of the |put| function only. %
Thus, we postulate |put s| to be injective for all sources |s|. %
\[\tag{PutInj}
  |s'| \in |put s v| \wedge |s'| \in |put s v'| \Rightarrow |v = v'|
\]

Similar to the eta reduction for the \emph{PutGet} law, we can rewrite
the \emph{GetPut} law as well. %
It is a bit more complicated to rewrite the equation
\[
  |put s (get s) = s|
\]
because of the two usages of the variable |s|. %
In order to simplify the equation, we need to use a pair as argument,
then we can apply |put| to this argument. %
The resulting argument is a function depending on |s|. %
The notion of using tuples instead of multiple arguments is called
\emph{currying} and \emph{uncurrying} respectively. %
In this case, we need to apply the function |uncurry :: (a -> b -> c)
-> (a, b) -> c| to the |put :: S -> V -> S| function in order to get a
modified function |put' = uncurry put :: (S,V) -> S| that takes a pair
of |S| and |V| as its argument. %
With this neat function in hand, we can express a point-free version
of the \emph{GetPut} law.
\[ 
|put' (\s -> (s, get s)) = id|
\]

For this equation, we can conclude that |put'|, i.e., |uncurry put|,
has a right inverse. %
That is, |put'| is surjective for all sources |S|, because a
surjective function |f :: A -> B| if and only if it exists a function
|g :: B -> A| such that |f . g = id :: B -> B| holds. %
\[\tag{PutSurj}
  \forall |s| \in |S| ~\exists |s'| \in |S|: |put' (s', get s') = s|
\]
Actually, this equation only holds for total |put| function, because
the equation requires to be fulfilled for all values |s| of the
resulting type |S|. %
Fisher et al. lay out idempotence of |`put` v| for all views |v| as
additional requirement for well-behaved lenses. %
\[\tag{PutTwice}
  |s'| \in |put s v| \Rightarrow |s'| = |put s' v|
\]

Furthermore, Fisher et al. verified that there is only one |get|
function for an arbitrary |put| function, which obeys \emph{PutInj}
and \emph{PutTwice}\footnote{In later work of \cite{validityCheck}
  these both properties are called \emph{PutDetermination} and
  \emph{PutStability} respectively}, and this |get| function can be
determined with the following equation.
\[\tag{relation between |get| and |put|}
  |get s = v| \Leftrightarrow |s = put s v|
\]

% format LensType s v = "Lens_{" s "~\rightarrow~" v "}" format
% LensType_ m s v = "Lens^{" m "}_{" s "~\rightarrow~" v "}" format
% LensPG s v = s "~\Leftarrow~" v format LensPG_ m s v = s
% "~\Leftarrow_{" m "}~" v

As a next step, \cite{putCombinators} developed a put-based language
in their subsequent work. %
In this work, they present a general design of put-based language as
well as an implementation of an embedded DSL for Haskell. %
The main idea of the put-based language is to provide a handfull of
combinators, which allows two define the |put| function of a lens. %
The |put| function of a lens defines the synchronisation strategy
between a modified view and a given source. %
In order to provide a wide scope of such strategies, the put-based
language is based on functions with monadic effects. %
A lens is represented as |type LensPG_ m s v = Maybe s -> v -> m s|,
where |m| denotes a monadic constraint. %
Depending on the given instance of the monad, the programmer can
influence the synchronisation behaviour. %
For example, we can program with traditional lenses without monadic
effects by using the |Identity| monad.

\label{IdentityMonad}%
\begin{spec}
data Identity a = Identity { runIdentity :: a }

instance Monad Identity where
  return valA          = Identity valA
  Identity valA >>= f  = f valA

type LensPG s v = LensPG_ Identity s v
\end{spec}

The put-based language is built upon a handfull of combinators, which
are inspired by the combinators of Foster et al., e.g., identity and
constant lens as well as lenses for filter, composition, products,
sums and conditionals. %
The language assures well-behavedness \todo{This is not an existing
  word.} by construction, that is, all combinators, including
composition, form well-behaved lenses and, thus, the composition of
predefined combinators form well-behaved lenses as well. %
Additionally, the Haskell library provides functions to define custom
lenses. %
Due to the lack of statical checks concerning well-behavedness, the
user can use the function |checkGetPut| and |checkPutGet|\footnote{In
  the associated paper, Fisher et al. use the name |enforceGetPut|
  instead.} to check for the corresponding lens laws at runtime. %

We can rebuild the example given above in terms of the put-based
language. %
The language provides a combinator |addfst| to add a left element to
the current source in order to create a pair. %
\begin{spec}
addfst :: (Maybe (sub s 1, v) -> v -> m (sub s 1)) -> LensPG_ m (sub s 1,v) v
addfst f = enforceGetPut put'
  where
   put' s v = f s v >>= \(sub s 1) -> return (sub s 1,v)
\end{spec}

The first argument of |addfst| is a function to create the second
component of the pair from the given source and view. %
We can use this combinator to define a lens |label fst GetPut| that
projects a pair to its first component. %
\begin{spec}
label fst GetPut :: LensPG_ m (sub s 1,v) v
label fst GetPut =
  addfst (\s v -> maybe (fail "Undefined") (\ (sub s 1,_) -> return . fst) s)
\end{spec}

If there is no source available, we cannot do anything meaningful
without losing generality, thus, we just throw an error.\footnote{The
  function |fail| is part of the Monad type class, thus, we can
  implement a mechanism to catch such errors.} %
Otherwise, we use |fst| to select the first component of the given
pair. %

\begin{spec}
> get (label fst GetPut) (42,"Hello World")
42
> put (label fst GetPut) (42,"Hello World") 13
(13,"Hello World")
\end{spec}

We will discuss the actual implementation in Section
\ref{sec:ImplComb} in more detail, because the Haskell library
\emph{putlenses}\footnote{\url{http://hackage.haskell.org/package/putlenses}},
which implements the ideas of the presented paper by
\cite{putCombinators}, forms the basis of an implementation in Curry
that we review later.

% \begin{itemize}
% \item pioneer work by \cite{biTCombinators} $\checkmark$

% \item put combinators \cite{putCombinators}
%   \begin{itemize}
%   \item get functions in general not injective: many possible
%     corresponding put functions exist to form a well-behaved lens
%   \item \emph{PUTINJ}: |put s| is injective for any source |s|,
%     i.e., $s' \in |put s v| \wedge s' \in |put s v'| \Rightarrow v =
%     v'$
%   \item \emph{PUTTWICE}: $s' \in |put s v| \Rightarrow s' = |put s'
%     v|$
%   \end{itemize}

%   \begin{itemize}
%   \item \emph{PutDeterminiation}: $\forall s,s',v,v' . |put s v =
%     put s' v'| \rightarrow v = v'$
%   \item \emph{PutStability}: $\forall s . \exists v . |put s v = s|$
%   \item validity - A put function is valid if and only if it
%     satisfies the \emph{PutDeterminiation} and \emph{PutStability}
%     properties
%   \end{itemize}

% \end{itemize}

\section{Bidirectionalisation}\label{sec:bi}
% \begin{itemize}
% \item syntactic restraints (like variable use and nested function
%   calls) vs polymorphic restraints and no updates to shape

% \item cost / difference function, minimal cost / difference is
%   chosen
% \item syntactic derivation via view complement
%   \cite{viewComp}$\checkmark$

% \item semantic derivation using relational parametricity
%   \cite{biForFree} $\checkmark$
% \item and additional enhancements: \cite{biForFreeImprove}
%   $\checkmark$ and \cite{semRevisited} $\checkmark$
% \item and semantic approach with agda, i.e., dependent types
%   \cite{semDependentTypes}

% \item combination of both \cite{synSemComb} $\checkmark$
% \end{itemize}

Bidirectionalisation is the process of transforming uni-directional
into bidirectional functions. %
In the following, we present two techniques to bidirectionalise a
uni-directional get function: the first technique by \cite{viewComp}
syntactically derives a put function for a given get function, whereas
the second approach takes a more semantic approach to generate an
appropriate put function at runtime. %
Both techniques have their advantages and disadvantages, so that the
authors also worked out a combined approach, which yields results at least as good
as the better one of the two techniques. %

\subsection{Syntactic Bidirectionalisation}
Matsuda et al. introduce a general first-order functional language
called \emph{VDL}. %
Their language has two syntactical restrictions, which we have to keep in
mind when talking about derivable functions: defined functions have to
be affine and treeless. %
A function definition is affine, if every variable on the left hand side
is used at most once on the right hand side of the definition;
treeless characterises function definitions without immediate data
structures. %
Nevertheless, VDL allows function definitions using arbitrary
algebraic data structures, e.g., lists and trees. %
The user defined uni-directional get funcions, and VDL automatically
derives appropriate put functions. %
The presented derivation algorithm
is based on a similiar approach in the field of databases, but follows
a syntactical approach, whereas the original idea is a rather semantic process. %
As a first step, VDL derives a \emph{view complement function} $f^c: S
\rightarrow V'$ for a get\footnote{Matsuda et. al call |get| functions
  view functions instead.} function $f: S \rightarrow V$. %
Matsuda et. al require the view complement function $f^c$ to be
injective when tupled with the view function $f$, i.e., $(f \triangle
f^c)$ is injective for a get function $f$ and its complement $f^c$. %

\begin{align*}
  f:&~ S \rightarrow V \tag{get function}\\
  f^c:&~ S \rightarrow V' \tag{view complement function}\\
  f ~\triangle~ f^c:&~ S \rightarrow (V \times V'), \tag{tupled function}\\
  (f ~\triangle~ f^c)&~ |valS| = |(f valS,g valS)|
\end{align*}
As last step, an inverse transformation is performed on the pair to
obtain the put function. %
\[\tag{derived put function}
  put_{<f,f^c>} (s,v) =  (f ~\triangle~ f^c)^{-1} (v,f^c s)
\]

In the end, the put function can be derived if the paired function and
its inverse can be derived effectively. %

The authors give an algorithm to automatically derive a view
complement function for a given get function. %
The algorithm uses so-called \emph{context notation}. %
A context is a tree of the form $K \square_1 \cdots \square_n$, and
such a context can be filled with expressions $e_!,\dots,e_n$, which
is denoted as $K ~[e_1,\cdots,e_n]$ where every $\square_i$ is
replaced by the corresponding $e_i$ for every $i \in
\{1,\cdots,n\}$. %
We can write any expression in treeless form as a context with
function calls and variables. %
In order to derive a view complement function, we analyse the rule of
a given get function, which right-hand side is denoted as a context,
and construct a corresponding rule for the complement function. %
That is, for each rule $r_i$ of a get function, where $r_i$ is the
$i$-th rule of the function definition, with the form
\[
f(p_1,\dots,p_n) \equiv
K[f_1(x_{1_1},\dots,x_{1_m}),\dots,f_n(x_{n_1},\dots,x_{n_k})]
\] we construct a rule $r_i^c$ for the complement, such that every
context $K$ is mapped to a new constructor $B_r$, every function call
$f_i(x_{i_1},\dots,x_{i_j})$ has a corresponding complement function
$f^c_i(x_{i_1},\dots,x_{i_j})$ and every variable used as argument on
left-hand side that is not used on the right-hand side is part of the
context for the complement rule. %
In the end, we have
\[ f^c(p_1,\dots,p_n) \equiv
B_r(f^c_1(x_{1_1},\dots,x_{1_m}),\dots,f^c_n(x_{n_1},\dots,x_{n_k}),y_1,\dots,y_l)
\]
as the derived rule for the complement, where $y_1,\dots,y_l$ are the
missing variables on the left-hand side of $r_i$. %

Let us take a look at our running example again; for a get function
\[fst_{get} (x,y) = x \] we can derive a corresponding definition with
one rule, where $B_1$ is a newly defined constructor with one argument
$y$. %
\[
fst^c_{get} (x,y) = B_1(y)
\]

The variable $y$ is introduced on the left-hand side of $fst_{get}$,
but missing on the right-hand side, thus, the complement function
"remembers" the missing piece, the second component $y$, in order to
reproduce a valid pair in the put direction. %
As a bonus, the authors also present an improvement in order to derive
a minimal complement function, which we do not discuss here. %

The next step is to form a pair of the get function and its
counterpart. %
We can easily define this pair in an extension of VDL that allows
tuples and local function definitions, i.e., where-clauses, but we
have to keep the restrictions concerning treeless and affine functions
definions in mind. %
Let $r_i$ be a rule of a get function $f$ and its complement $r^c_i$
of the following form. %
\[
f(p_1,\dots,p_n) \equiv
K[f_1(y_{1_1},\dots,y_{1_l}),\dots,f_n(y_{n_1},\dots,y_{n_k}),
x_1,\dots,x_m]
\]
\[
f^c(p_1,\dots,p_n) \equiv
K'[f^c_1(y_{1_1},\dots,y_{1_l}),\dots,f^c_n(y_{n_1},\dots,y_{n_k}),
x_1,\dots,x_m]
\]

The algorithm constructs a rule $r^{\triangle}_i$ with fresh
variables $t_i,t'_i$ for $i \in \{1,\dots,n\}$ used in a where-clause
to obey the requirement concerning treelessness.
\todo{fix latex for alignment}
\begin{align*}
  f^{\triangle}(p_1,\dots,p_n&) \equiv
  (K[t_1,\dots,t_n,x_1,\dots,x_m],K'[t'_1,\dots,t'_n,x_1,\dots, x_m])\\
  \text{\textbf{where }}\quad\quad&\\
  (t_1,t'_1) \equiv&~
  f^{\triangle}_1(y_{1_1},\dots,y_{1_j})\\
  \vdots\quad\quad&\quad\quad\quad\quad\vdots\\
  (t_n,t'_n) \equiv&~
  f^{\triangle}_n(y_{n_1},\dots,y_{n_j'})
\end{align*}

In order to gain the complete definition for $f^{\triangle} = f
\triangle f^c$, all rule pairs for the get function and its
complement, respectively, have to be considered. %
For our example get function |sub fst get|, we do not need to
introduce local function definitions, because the rules are clear of
function calls, and just use variables. %
\[
fst_{get}^{\triangle}(x,y) = fst_{get} ~\triangle~ fst_{get}^c(x,y) = (x,B_1(y))
\]

As the last step of the complement derivation, we calculate an inverse
for the resulting pair of the previous step, i.e.,
$(f^{\triangle})^{-1}$. %
Fortunately, we exactly know how a rule of $(f^{\triangle})^{-1}$
looks like, thus, we can assume that the given rule $r^{\triangle}_i$
has the following form. %
\begin{align*}
  f^{\triangle}(p_1,\dots,p_n&) \equiv (e_1,e_2)\\
  \text{\textbf{where }}\quad\quad&\\
  (t_1,t'_1) \equiv&~
  f^{\triangle}_1(y_{1_1},\dots,y_{1_j})\\
  \vdots\quad\quad&\quad\quad\quad\quad\vdots\\
  (t_n,t'_n) \equiv&~
  f^{\triangle}_n(y_{n_1},\dots,y_{n_j'})
\end{align*}

In order to inverse the given rule, we swap the left-hand and the
right-hand side of that rule and every function call needs to be
inversed as well. %
The inversion leads to the rule $(r^{\triangle}_i)^{-1}$, where we
apply the inversion for every function $(f_i)^{-1}$
recursively. %
\begin{align*}
  (f^{\triangle})^{-1}(e1,e2) \equiv (p_1,&\dots,p_n)\\
  \text{\textbf{where }}\quad\quad \quad\quad \quad&\\
  (y_{1_1},\dots,y_{1_j}) \equiv&~
  (f^{\triangle}_1)^{-1} (t_1,t'_1)\\
  \vdots\quad\quad&\quad\quad\quad\quad\vdots\\
  (y_{n_1},\dots,y_{n_j}) \equiv&~
  (f^{\triangle}_n)^{-1} (t_n,t'_n)\\
\end{align*}

When we apply this transformation to our example function
$fst_{get}^{\triangle}$, we can finally derive the appropriate put
function with the definition given above. %
\begin{align*}
(fst_{get}^{\triangle})^{-1}(x,B_1(y)) =&~ (x,y)\\
put_{<fst_{get},fst^c_{get}>} ((x,y),v)
=&~ (fst_{get} ~\triangle~ fst_{get}^c)^{-1} (v,fst_{get}^c (x,y))\\
=&~ (fst_{get} ~\triangle~ fst_{get}^c)^{-1} (v,B_1(y))\\
=&~ (v,y)
\end{align*}

There are two details that we did not examine so far: a determinism
property for the inverse transformation and further requirements for
the complement function. %
The inverse transformation is not guaranteed to be deterministic, it
is possible to generate equations with overlapping left-hand sides. %
In the case of nondeterministic programs, a backtracking search
becomes necessary, though, \cite{synSemComb} state that it would be
preferable to only obtain deterministic programs. %
Furthermore, the complement, which we derive in the first step, must
be injective and minimal with respect to a collapsing order, which
needs to be defined. %
Fortunately, injectivity is decidable in VDL and the proposed
algorithm is sound and complete. \\

\subsection{Semantic Bidirectionalisation}
On the other hand, \cite{biForFree} introduces an approach for
semantic bidirectionalisation using free theorems to prove consistency
conditions. %
Voigtl\"ander defines a function |bff :: (forall a. [a] -> [a]) ->
(forall a. [a] -> [a] -> [a])| in Haskell, which first argument is a
polymorphic get function and yields the appropriate put function. %
In contrast to the syntactic approach we studied before, the resulting
put is a functional value which is semantically equivalent to a
syntactical derived put function. %
The advantage is that we have less language restrictions, we can use
Haskell as our language of choice instead of a sublanguage; the |bff|
function takes any Haskell function of appropriate type as its
argument. %
Then again, the semantic approach limits the range of function on
other respects: any get function that changes the shape of its
elements fails due to non-trackable updates. %
The defined |bff| function is defined on lists, but the approach is
also applicable for all data structures, which have shape and content,
i.e. which apply to the category of containers as defined by
\cite{containers}. %

The approach utilises the fact that the get function is polymorphic
over its first arguments, i.e. the container's element. %
Therefore, we can assume that it does not depend on any concrete
element of its container, but only on positional information, which
are independent of the element's values. %
The use of free theorems allows us to inspect the effect of the |get|
transformation without knowing about the explicit implementation. %

The definition of |bff| simulates its first argument, i.e. the get
function, on an arbitrary container, like for example a list of
|Integer| if we use a polymorphic list, |[a]|, as container. %
The container to simulate shares its shape property with the given
container, which is the second argument of |bff|; in the example of
lists, the simulation list and the given list need to be of the same
length. %
Every value in the simulated container has a corresponding value in
the given container. %
As an example, we take a look at a get function that selects the first
element of a list and yields an element with just that element. %
%
\begin{spec}
sub get fst :: [a] -> [a]
(sub get fst) (x:_) = [x]
\end{spec}%
%
We want to update the source |list = [Left 10, Left 12, Right True, Left 13] ::
[Either Int Bool]| with the view |[Right False] :: [Either Int
Bool]|. %

\begin{spec}
bff (sub get fst) [Left 10, Left 12, Right True, Left 13] [Right False]
\end{spec}%
%
The |bff| function constructs a mapping for every element of the given
list, each index position of the list is mapped to its element. %
the given list. %
\begin{spec}
mapping :: [a] -> [(Int,a)]
mapping = zip [0..]
     -- = zip [0..] [Left 10, Left 12, Right True, Left 13]
     -- = [(0,Left 10),(1,Left 12),(2,Right True), (3,Left 13)]
\end{spec}%
%
As a second step, we get a mapping from the simulated view and the
originally updated view, when we apply the |get| function to the
simulated list and map the result to the given view. %

\begin{spec}
mapping2 :: ([a] -> [a]) -> [Int] -> [a] -> [(Int,a)]
mapping2 getF = zip' . getF 
      -- = zip' ((sub get fst) [0,1,2,3]) [Right False]
      -- = zip' [0] [Right False]
      -- = (0,Right False)
  where
   zip' :: [Int] -> [a] -> [(Int,a)]
   zip' []      _      = []
   zip' _       []     = []
   zip' (i:is)  (x:xs)  | isNothing (i `lookup` zs)  = (i,x) : zs
                        | otherwise                  = zs
    where
     zs = zip' is xs
\end{spec}

We combine both mappings with precedences to the second: if we find a
value im both mappings, we choose the one from the view. %

\begin{spec}
mapping3 :: [(Int,a)] -> [(Int,a)] -> [(Int,a)] 
mapping3 m1 m2 = union' m1 m2
            -- = [(0,Right False),(1, Left 12), (2, Right True), (3, Left 13)]

union' :: [(i,a)] -> [(i,a)] -> [(i,a)]
union'  xs            []      = xs
union'  []            ys      = ys
union'  (x@(i,_):xs)  ys  | isNothing (i `lookup` ys)  = (x,y) : union' xs ys
                          | otherwise                  = union' xs ys
\end{spec}

As last step, every element in the container we used for simulation is
replaced by its associated value according to the combined mapping. %
With all the ground prepared, we can define the |bff| function given in the paper. %

\begin{spec}
bff :: (forall a. [a] -> [a]) -> (forall a. [a] -> [a] -> [a])
bff (sub get f) s v = map (fromJust .flip lookup (mapping3 m1 m2)) m1
                    -- = [Right False, Left 12, Right True, Left 13]
  where
   m1 = mapping s
   m2 = mapping2 (sub get f) (map snd m1) v
\end{spec}

Voigtl\"ander defines two additional functions, |sub bff EQ| and |sub
bff ORD|, which use the function of the type classes |Eq| and |Ord|
respectively. %
In order to apply his approach for a |get| function that duplicates
elements, the defined mapping fails because of its simple
definition. %
In a more practical mapping, equivalent elements in the original
container need to map to the same element in the arbitrary container
that we need for simulation. %
In this case, we need to compare the elements within the container,
this is where the |Eq| type class comes into play. %
For the function |sub bff Ord|, the mapping needs a similar, but
rather complicated and more technical, adjustment in order to allow
the use of free theorems
again. \\

\subsubsection[Generalisation to Higher-Order Functions]{Generalisation for the Three Functions |bff|, |sub bff EQ| and |sub bff ORD|}

As an enhancement for semantic bidirectionalisation, \cite{semRevisited}
presented a generalisation that extends the range of |get| function to
higher-order functions that are not expressed by type classes, or
depend on different type classes than |Eq| and |Ord|. %
Instead of three single functions, like in Voigtl\"ander's work, Wang
and Najd define a function
%format (sup a n) = "\bar{" a "}^{" n "}"
\[
|bffBy  :: (forall a. (sup a n) -> [a] -> [a]) -> (sup a n -> Bool) -> (sup i n -> Bool) -> ([a] -> [a] -> [a])|
\]
\todo{search for better alternatives regarding overline and bar}
that takes an observer function as
second argument that gives rise to equivalence properties of the
elements; the third argument is an observer function for the simulated source.\footnote{In their paper, Wang and Najd define a more general observer functions |obs :: sup x n -> Z|, where |Z| is an arbitrary, but monomorphic type.
For simplicity reasons, we omit the generality and use |Bool|, as we do not want to distinguish uppercase and lowercase type variables like in the paper.
Furthermore, we use the same abbreviation as Wang Najd in their paper and write |f :: sup a n| for a function |f :: |$~\underbrace{|a -> ... -> a|}_\text{n-times}$ with |n| repetitions of the type |a|.}
The approach uses these observer functions to build the mappings as in
the original approach. %
These mappings are called observation tables here, and generalise the
explicite usage of different functions for different type class
dependencies. %

As a first modification to the original approach, instead of mapping
between indices and elements of a list, the map just hold two copies
of each element. %
That is, the original and its copy form a pair, where the first
component stays constant and the second one is used for updates. %

\begin{spec}
sub mapping by :: [a] -> ([a],[a])
(sub mapping by) m = zip m m
\end{spec}

This approach has the advantage, that we can use the equivalence function
for both components of the pair and do not need to adjust the function
to work on |Integer|, which are introduced for the mapping in the
original approach.\footnote{This modification is used for functions with an |Eq| or |Ord| type class context, i.e., for cases where we would use |sub bff EQ| and |sub bff ORD|, respectively, in the original approach. For all other cases, i.e., when we use |bff|, the mapping is build as in the orignal approach.} %

Similiar to before, the next step simulates the get function on the
source list, and creates a new mapping with the resulting view and the
given updated view. %

\begin{spec}
sub mapping2 by :: (a -> a -> Bool) -> (i -> i -> Bool) -> [i] -> [a] -> [(a,a)]
(sub mapping2 by) eqA eqI is as
   |  length is == length as =
       if and [not (i `eqI` j) or (x `eqA'` y) | (i,x) <- zs, (j,y) <- zs]
         then zs
         else error "Inconsistent update!"
  where
   zs = zip is as
\end{spec}

The first and second argument are equivalence functions; the first function compares the elements of the actual source list and the other function is used for comparisions on the keys of the given mapping. %
In the case of the used mapping function |sub mapping by|, we do not need to dinstinguish between these functions. %
The important step for the new approach is to check the resulting map in regard to the so-called \emph{Map Invariant}. %
This invariant says that a valid map `zs :: [(i,x)]` must satisfy the following property for two observer functions |f :: sup x n -> Bool| and |g :: sup i n -> Bool|. %

\begin{align*}
\forall (i_1, x_1) \dots (i_n,x_n) \in zs. f x_1 \dots x_n \equiv i_1 \dots i_n \tag{Map Invariant}
\end{align*}

The invariant requires all pairs of the given map to be equal in regard to the two observer function. %
In particular, depending on the arity of the observer functions, all combinations of pairs in the map have to fulfill this property. %
In order to check the invariant property for our map, we define a function to apply two given observer functions according to the defintion above. %

\begin{spec}
checkInv :: (sup a n -> Bool) -> (sup i n -> Bool) -> [(i,a)] -> [(i,a)]
checkInv f g zs  | all check (sub comb n) = zs
                 | otherwise              = error "Invariant broken!"
 where
  (sub comb n) = nub (map (take n) (permutations zs))
  check ys =  let (is,as) = unzip ys
              in (sub uncurry n) f ((sub tuple n) as) == (sub uncurry n) g ((sub tuple n) is)
\end{spec}

Here, |sub comb n| is a list of all combinations of pairs in the given map, |sub uncurry n| is a function to uncurry a |n|-th tuple, and |sub tuple n| is the corresponding function to convert a list into an |n|-th tuple; we omit the definitions for brevity. %
Furthermore, we combine both mappings with precedences to the second one, like in the original approach. %
We combine two pairs of the given maps if the used keys, i.e., the first components, are equal with regard to the given equivalence function. %
For the overall definition, we have to keep in mind to check the map invariant for the result as well. 

\begin{spec}
sub mapping3 by :: (i -> i -> Bool) -> [(i,a)] -> [(i,a)] -> [(i,a)]
(sub mapping3 by) eqI m1 m2 = unionBy' eqI m1 m2

unionBy' :: (i -> i -> Bool) -> [(i,a)] -> [(i,a)] -> [(i,a)]
unionBy' _    xs            []  = xs
unionBy' _    []            ys  = ys
unionBy' eqI  (x@(i,_):xs)  ys
    | not (all (i `eqI`) ys)  = x : unionBy' eqI xs ys
    | otherwise               = unionBy' eqI xs ys
\end{spec}

There are still some missing parts that we need to discuss, like the locally defined equality functions |eqA|, |eqI|. %
Najd and Weng present three different equality functions, where each function has its own dis- and advantages. %
They distinguish between observable, structura, and physical equivalence and we refer to their paper for further study. %
As mentiond before, in our example we do not distinguish between keys and values of the given map, that is, we only need to define the equality function for the values of the source and use it for the keys as well. %

Additionally, we need to adapt the |lookup| function that was used in the original approach to select the final values for the updated source. %
We lookup the values with regard to the given equality function for the keys of the map. %

\begin{spec}
lookupBy :: (i -> i -> Bool) -> [(i,a)] -> i -> Maybe a
lookupBy _ []         _  = Nothing
lookupBy p ((j,x):xs) i
  | p i j      = Just x
  | otherwise  = lookupBy p xs i
\end{spec}

In the end, we can derive a corresponding put function with the following definition. %

\begin{spec}
(sub bff by) getBy f g s v = map (lookupBy eqI m3) is
   where
    is = map fst m1
    m1 = mapping1 s
    m2 = checkInv f g (mapping2 eqA eqI (getBy f is) v)
    m3 = checkInv f g (mapping3 eqI m1 m2)
    eqA = undefined
    eqI = eqA
\end{spec}

In order to round up this approach, we examine the usage of the |bffBy| function for physical equivalence, i.e., |eqA (i,x) (j,y) = i == x && (j,y)|. %

\begin{spec}
bffBy  filter
       ((== "fst") . fst)
       ((== "fst") . fst)
       [("snd",17),("fst",13)]
       [("fst",42)]
  = map (lookupBy ((== "fst") . fst) m3 [("snd",17),("fst",13)]
     where m3 =  [  (("snd",17),("snd",17))
                 ,  (("fst",13),("fst",42)) ]
  = [("snd",17),("fst",42)]
\end{spec}

\subsubsection{Adaption for Monomorphic Functions}

As a second enhancement, \cite{biForFreeImprove} introduce a type
class to extend the range of |get| functions to monomorphic
transformations. %
The main idea is to provide a type class |PackM delta alpha mu| to
convert monomorphic functions into polymorphic ones.\footnote{The following code needs several langue extension to run accordingly: Rank2Types, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, ExistentialQuantification, FlexibleContexts.} %

\begin{spec}
class (Pack delta alpha, Monad mu) => PackM delta alpha mu where
  liftO :: Eq beta => ([delta] -> beta) -> [alpha] -> mu beta
class Pack delta alpha | alpha -> delta where
  new :: delta -> alpha
\end{spec}

The type variable |delta| represents the type of the concrete data
structure, whereas |alpha| is the type of the abstracted value. %
The last type variable |mu| is the used monad, which tracks the transformation on values of the concrete
structure called; this tracking data is called observation history. %
The additional type class |Pack delta alpha| constructs labels to
track information regarding the location of values within the concrete
structure. %
In short, the approach replaces monomorphic values in the definition
of the |get| function, which are, for example, used for comparisons,
with polymorphic values. %
The following function is quite similar to Example \ref{filter:fstGet}, but it is defined on lists instead of trees; it selects the first element
with a label named |"fst"| from the given list. %

\begin{spec}
(sub get fst) :: [String] -> [String]
(sub get fst) []     = []
(sub get fst) (v:vs)
  | v == "fst"  = [v]
  | otherwise   = (sub get fst) vs
\end{spec}

In order to execute such a function in its get direction, the authors define a special
function with |PackM| context. %
The following definition works on lists but any other
polymorphic data type is possible as well. %

\begin{spec}
instance Pack delta (Identity delta) where
  new = Identity

instance PackM delta (Identity delta) Identity where
  liftO p x     = Identity (p (map runIdentity x))

get  :: (forall alpha. forall mu . PackM delta alpha mu
     => [alpha] -> mu [alpha])
     -> [delta]
     -> [delta]
get h s =  let Identity v = h (fmap Identity s)
           in fmap runIdentity v
\end{spec}

In the get direction, we do not want to track any information about
the mapping of abstract and concrete values, thus, the underlying
monad is instantiated to the Identity monad\footnote{See Section
  \ref{IdentityMonad} for the definition of the Identity monad.}. %
Our example get function needs to be rewritten in order to obey the |PackM| context. %
That is, the check needs to be lifted into the |PackM| type class and because of the underlying monad, we need to wrap our result into a monadic value as well. %

\begin{spec}
(sub get fst) :: forall alpha mu beta . PackM (String,beta) alpha mu => [alpha] -> mu [alpha]
(sub get fst) [] = return []
(sub get fst) (val:vs) = do   
   b <- liftO2 (\x y -> fst x == fst y) val (new ("fst",undefined))
   if b  then return [val]
         else getFst vs

liftO2 p x y  = liftIO (\ [x,y] -> p x y) [x,y]
\end{spec}

For the rewritten version of the get function, we can apply the it to an example list |[("fst",13),("snd",21)]|. %
In order to get a better insight of the ongoing operation, we evaluate the following expression with more intermediate steps. %

\begin{spec}
sub get example  = get (sub get fst) [("fst",13),("snd",21)]
                 = fmap   runIdentity 
                          (runIdentity ((sub get fst) (fmap  Identity
                                                             [("fst",13), ("snd",21)])))
                 = fmap runIdentity (runIdentity (Identity [Identity ("fst",21)]))
                 = [("fst",21)]
\end{spec}

For the put direction, the approach constructs polymorphic values from the original monomorphic
values, and does not instantiate type variables when used in
comparisons in order to fulfill the requirements to use free theorems. %
Like in the original approach, we first construct a mapping to track
information about the location of the source values when applying the
get function. %

\begin{spec}
data Loc alpha = Loc { body :: alpha, location :: Maybe Int }

assignLocs :: [delta] -> [Loc delta]
assignLocs xs = zipWith (\ x i -> Loc x (Just i)) xs [0..]

instance Pack delta (Loc delta) where
  new x = Loc x Nothing
\end{spec}

Here, the location is stored in a self-defined data structure, where
every element of the list is mapped to its index. %
Additionally, if a put function inserts a new value during the update,
there is no location information for this value in the source;
therefore, the author model the assigned location to be optional
within the view structure, i.e. |Maybe Int|. %

\begin{spec}
sub assign example  = assignLocs [("fst",17),("snd",21)]
                    = [Loc ("fst",17) (Just 0), Loc ("snd",21) (Loc 1)]
\end{spec}

Additionally, the authors use a writer monad to actually track the
observation history. %
In order to lift a function to be an observer function, the writer monad keeps track of the
function and its arguments, which are unwrapped from the |Loc| data type first. %
The relevant observation history is also modelled as a data structure; the
structure depends on an observation function, a list of arguments and
and the resulting value for the application of the function to its arguments. %
That way, each result can be checked, when the update is executed in a
later step, if an updated value does not confirm to the observation
history, the update fails. %


\begin{spec}
data Result alpha = forall beta . Eq beta => Result ([alpha] -> beta) [alpha] beta

data Writer eta beta = Writer { runWriter :: (beta, [Result eta]) }

instance Monad (Writer alpha) where
  return x = Writer (x, [])
  Writer (x,sub res 1) >>= f =
    let Writer (y,sub res 2) = f x
    in Writer (y,sub res 1 ++ sub res 2)

instance PackM delta (Loc delta) (Writer (Loc delta)) where
  liftO p x = Writer (p' x, [Result p' x (p' x)])
    where p' = p . map body
\end{spec}

As a next step, we apply the get function to our dummy list with the
locations associated to the original source elements. %
This time, the underlying monad is not the Identity, but the Writer
monad we just defined, thus, we can track all relevant information
regarding the observation function. %
In our example, we only track the the first element because the
remaining elements are not even touched, hence, we can be sure that
they do not change during the update. %

\begin{spec}
Writer (sub upd example,sub history example)  =
   (sub get fst) (sub assign example)
=  (sub get fst)  [ Loc ("fst",17) (Just 0)
                  , Loc ("snd",21) (Loc 1)]
= Writer   ([Loc ("fst",17) (Just 0)]
           ,[Result  (\ [x,y] -> x == y)
                     [Loc ("tree",13) (Just 0), Loc "fst" Nothing]
                     True])
\end{spec}%

We use the generated list of location information for the source and the
updated view, i.e. |sub upd example|, as a new mapping. %
The implementation requires both lists to be of the same size,
otherwise the function fails because of a shape mismatch. %
If the two lists have the same size, we create a mapping between the
elements of the source list and its index in corresponding location
information. %
The mapping needs to be consistent, if the same element occurs
repeatedly, each occurrence needs to map to the same location as before;
otherwise the construction fails because of inconsistency. %

\begin{spec}
matchViews :: Eq delta => [Loc delta] -> [delta] -> [(Int,delta)]
matchViews locVs vs
  | length locVs vs  = makeUpd (zip locVs vs)
  | otherwise        = error "Shape mismatch"

makeUpd :: Eq delta => [(Loc delta, delta)] -> [(Int,delta)]
makeUpd = foldr f []
 where
  f (Loc x (Just i), y) u =
     maybe  ((i,y) : u)
            (\ y' -> if y == y' then u else error "Inconsistent Update")
            (lookup i u)
  f (Loc x Nothing, y) u  | x == y     = u
                          | otherwise  = error "Update of Constant"
\end{spec}

In our example, the element of the updated view is matched with its previous occurences in the source list, yielding the following new mapping. %

\begin{spec}
sub matchViews example  =  let Writer (vs,res) = (sub writer example)
                           in matchViews vs [("fst",42)]
                        = makeUpd (zip [Loc "fst" (Just 0)] [("fst",42)])
                        = [(0,("fst",42))]
\end{spec}

With the updated mapping and the observation history in mind, we actually want to execute the update. %
Similar to the original apporach, we have a mapping between the
original list and their index position in that list, |sub assign example|. %
In addition, we have the history with the location information for
some elements. %
Thus, we lookup the given position in the mapping, and change the
element for the corresponding location information if we do find a
match; otherwise the information remains unchanged. %

\begin{spec}
update :: [(Int,delta)] -> Loc delta -> Loc delta
update upd (Loc x Nothing)   = Loc x Nothing
update upd (Loc x (Just i))  = maybe  (Loc x (Just i))
                                      (flip Loc (Just i))
                                      (lookup i upd)
\end{spec}

This update function is important to check the observation history for consistency, and to run the actual modifaction on the source. %
In the final version of the appropriate put function, we use the previous defined functions, and finally, update the given source list, if the history check succeeds. %

\begin{spec}
put :: Eq delta => (forall alpha. forall mu. PackM delta alpha mu => [alpha] -> mu [alpha]) -> [delta] -> [delta] -> [delta]
put h s v
   | checkHist (update upd) hist  = fmap (body . update upd) locs
   | otherwise                    = error "Inconsistent History"
  where
   locs                = assignLocs s
   Writer (res, hist)  = h locs
   upd                 = matchViews res v
\end{spec}

The history check first applies the update to the underlying value
and then runs the observation function for the possibly changed value
and its arguments. %
The resulting value must be the same
as the one stored in the history. %

\begin{spec}
checkHist :: (Loc delta -> Loc delta) -> [Loc delta] -> Bool
checkHist updF = all (\ (Result p xs r) -> p (map updF xs) == r)
\end{spec}

At the end, we can apply this polymorphic put function to our monomorphic get function to update a source for a given view. %
In order to round up the ongoing example, we update the source list
|[("fst",17),("snd",21)]| with the modified view |[("fst", 42)]|. %

\begin{spec}
(sub put fst)  = put (sub get fst) [("fst",17),("snd",21)] [("fst",42)]
               = if   checkHist  (update (sub upd example))
                                 (sub history example)
                      then fmap (body . update (sub upd example)) (sub assign example)
                      else error "Inconsistent History"
               = fmap  (body . update [(1,("fst",42))])
                       [ Loc ("fst",17) (Just 0)
                       , Loc ("snd",21) (Just 1) ]
               = [("fst",42),("snd",21)]
\end{spec}%

\subsection[Combined Approach]{Combining Semantic and Syntactic Bidirectionalisation}

It becomes apparent that both approaches have their pros and cons,
naturally, \cite{synSemComb} proposed a combination that uses the
semantic as well as the syntactic bidirectionalisation. %
The combined approach uses each technique for their area of expertise:
the semantic derivation for content updates and the syntactic
derivation for shape-changing transformations. %
The authors categorise the two techniques as follows: syntactic
bidirectionalisation is used as black box whereas semantic
bidirectionalisation is similar to a glass box. %
That is, the semantic bidirectionalisation approach can be more
powerful if it we refactor the transformation in order to plug-in a
syntactic technique; then, shape-changing transformations can be
handled. %
The presented combination is general enough to allow any syntactic
approach to be plugged-in, which is discussed by \cite{enhanceSem} in
more detail. %
As a minor drawback, the range of |get| definitions covered by the
combined approach is limited by both factors: only linear and treeless
functions are allowed because of the usage of the syntactic
bidirectionalisation, and we can only use polymorphic functions in
order to use the semantic bidirectionalisation technique. %
Fortunately, the presented enhancements and extensions to semantic
bidirectionalisation does consort well with the combined approach. %
That is, we can use the more general function |bffBy| in combination
with specified observer functions for semantic bidirectionalisation
and turn monomorphic functions into polymorphic ones with the monadic
extension to gain a wider range of possible |get| functions. %
In the end, the combined approach performs never worse than one of the
two approaches by themselves. %
The semantic bidirectionalisation on its own has difficulties in
shape-changing update, but are covered with the combined approach,
whereas the syntactic approach operates on specialised programs now,
which can lead to better results. %
In addition, the semantic bidirectionalisation uses free theorems also
to prove consistency conditions. %
We discussed the syntactical bidirectionalisation, which formulates
its derivation on the ground of the \emph{GetPut} and \emph{PutGet}
law, in contrast, Voigtl\"ander proves, with the help of free
theorems, for each of his function definitions, |bff|, |sub bff EQ|
and |sub bff ORD|, that they obey the lens laws. %
That is, instead of a correctness-by-construction approach, the laws
are verified by hand. \\

\section{Get-Lenses vs Put-Lenses}\label{sec:GetVsPut}
% Get
% \begin{itemize}
% \item get is intuitive
% \item underspecified put, when only get is defined
% \item non-injective get functions
% \end{itemize}
% Put
% \begin{itemize}
% \item generated get is unique \cite{putback}
%   \begin{itemize}
%   \item requires putback functions to be affine and in treeless
%     form, that is, each view variable is used at most once and no
%     intermediate data structures are used in definitions
%   \item this class of functions has similarities to tree transducers
%   \item assumes only total functions
%   \item hybrid compositional approach, but focus on designing
%     language to specify various primitive putback functions over
%     algebraic data structures
%   \item validity of putback transformations - A put is valid, if
%     there exists a get such that \emph{GetPut} and \emph{PutGet} are
%     satisfied
%   \item Uniqueness of get - Given a put function, there exists at
%     most one get function that forms a well-behaved BX
%   \end{itemize}
% \item better suited for implementation with Curry
% \end{itemize}

\section{Implementation in Curry}\label{sec:Impl}


\subsection{Combinatorial Lens Library}\label{sec:ImplComb}


\subsubsection{Examples}\label{sec:ImplCombEx}


\subsection{Put-Lenses Library}\label{sec:ImplPut}


\subsubsection{Examples}\label{sec:ImplPutEx}
