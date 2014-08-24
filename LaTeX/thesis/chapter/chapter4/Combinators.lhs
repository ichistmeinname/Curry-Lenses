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