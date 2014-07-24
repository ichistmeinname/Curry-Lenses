\chapter{Different implementation approaches}

\todo{rephrase}
Bidirectional programming is a rising topic in the field of computer
science, and many different approaches exist to tackle the problem,
these approaches come from different disciplines of
computer science like databases, graph transformation, programming
languages and interface design. %
This section summarises the main two approaches and highlights
differences as well as some details. %

The main two techniques to work with bidirectional transformations are
combinatorial languages and bidirectionalisation. %
Most commonly, a combinatorial language is defined a DSL in a general purpose
programming language or as a new programming language, and provides a
set of primitives, which can be combined to define complex
structures. %
The definitions of these primitives mostly consist of both functions,
the get and the appropriate put function. %
In contrast to this approach, the bidirectionalisation technique takes
an uni-directional function and synthesises this function to
bidirectional one. %
In the remainder of the chapter introduces combinatorial and
bidirectionalisation approaches for lenses; both approaches have two
subcategories, because the implementation can either focus on defining a get
function or a put function. %
In this context, we will discuss advantages and disadvantages of
defining a get function and present a first proposal by
\cite{putCombinators} to set the focus on the put function. %

\section{Combinatorial lenses}
The first combinatorial technique is the pioneer work by \cite{biTCombinators}, who
designed a domain-specific programming language to define
bidirectional transformations on tree-structured data. %
Foster et al formulate fundamental laws
concerning lenses, combine these laws with the intuitive
behaviour of lenses, and use fundamental tools from domain theory to also
define lenses by recursion; they lay the focus of the design of their language on
robustness and and ease of use. %
That is, their language guarantees well-behaved lens definitions and the totality
of the primitive transformations with the integrated type system. %
The underlying type system, and with that the type safety, is the main
contribution to the field of bidirectional programming. %
The authors state close connections with topics from the database
community: lenses are a well-known abstraction in databases concerning
tables and queries, and the \emph{update translation under a constant
  complement}\todo{citation?} tackles problems concerning definedness(precision?)
and continuity, whereas the property of well-behaved lenses corresponds to
\emph{update translators}\todo{citation}. %

In their paper, Foster et al. define a handful of primitive lens
combinators for trees, and the combination of these primitive lenses results in a powerful
abstraction to describe transformations. %
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

The work of Foster et al origins in the Harmony
project\footnote{\url{https://alliance.seas.upenn.edu/~harmony/old/}} \citeyearpar{relationalLenses,harmonyOverview,harmonyManual}, a generic framework to synchronise
tree-structured data; an ongoing example throughout their work is the
synchronisation of different browser bookmarks, calendar and address book formats. %
They continued their work on lenses with a project called
Boomerang\footnote{\url{https://alliance.seas.upenn.edu/~harmony/}} \citeyearpar{boomerang} focuses on string data instead of
tree-structure data; the developer team of
\emph{Augeas}\footnote{\url{http://augeas.net}} uses the Boomerang
language as a framework for their configuration API. %
As well as in their first work, their language is based on a type
system to guarantee well-behaved lenses and total lenses and a rich set of
lens combinators to define powerful transformations on strings. %
\todo{check statements about type safety again; does only hold for
  primitive lenses; user-defined functions are only checked
  at run time}

Other combinatorial approaches for lenses exist, they all focus on
specifying a |get| function, and the appropriate |put| function is then
propagated through the definition of the used |get| combinators. %
For example, \cite{pointfree} designed a proint-free DSL in Haskell, in
which the programmer also defines the |get| transformation only. %
\cite{putCombinators}  are the first to propose to use the |put|
definition instead. %
It seems quite obvious that both, the forward and the backward
function of a bidirectional transformation, can be used for
bidirectionalisation. %
Nevertheless, so far, the current techniques follow the path of Foster et
al. %
It becomes clear in the work of Fisher et al. that typical problems in
|get| definitions are the ambiguity of the derived |put|
functions. %
That is, in several cases there is more than one appropriate |put|
function to correspond with the |get| definition. %
As we discussed before, these problems concerning ambiguity arrive
when the defined |get| function is not injective. %
This ambiguity can be eliminated when we define the |put| direction
instead. %
Fisher et al. show in their work that the appropriate |get| function
for a defined |put| function is unique if certain requirements apply
to the |put| function. %
They prepare their theorem with some transformations on the
\emph{PutGet} and \emph{GetPut} laws; instead of the classical
representation, they formulate their requirements based on the |put|
definition only. %

As \cite{putback} state in their technical report, the \emph{PutGet} law
can be reformulate as injectivity property of the |put| function. %
That is, |put s| is injective for all sources |s|.

\begin{equation}\tag{PutInj}
|s'| \in |put s v| \wedge |s'| \in |put s v'| \Rightarrow |v = v'|
\end{equation}

Instead of the \emph{GetPut} law, they lay out idempotence of |`put`
v| for all views |v| as additional requirement for well-behaved lenses. %

\begin{equation}\tag{PutTwice}
|s'| \in |put s v| \Rightarrow |s'| = |put s' v|
\end{equation}

Furthermore, they verified that there is only one |get|
function for an arbitrary |put| function, which obeys
\emph{PutInj} and \emph{PutTwice}\footnote{In later work of
  \cite{validityCheck} these both
  properties are called \emph{PutDeterminiation} and
  \emph{PutStability} respectively}, and this |get| function can be
determined with the following equation.

\begin{equation*}\tag{Definition of |get| by means of |put|}
|get s = v| \Leftrightarrow |s = put s v|
\end{equation*}

% \begin{itemize}
% \item pioneer work by \cite{biTCombinators} $\checkmark$

% \item put combinators \cite{putCombinators}
% \begin{itemize}
% \item get functions in general not injective: many possible corresponding
%       put functions exist to form a well-behaved lens
% \item \emph{PUTINJ}: |put s| is injective for any source |s|, i.e., $s' \in |put
%   s v| \wedge s' \in |put s v'| \Rightarrow v = v'$
% \item \emph{PUTTWICE}: $s' \in |put s v| \Rightarrow s' = |put s' v|$
% \end{itemize}

% \begin{itemize}
% \item \emph{PutDeterminiation}: $\forall s,s',v,v' . |put s v = put s'
%   v'| \rightarrow v = v'$
% \item \emph{PutStability}: $\forall s . \exists v . |put s v = s|$
% \item validity - A put function is valid if and only if it satisfies
%   the \emph{PutDeterminiation} and \emph{PutStability} properties
% \end{itemize}

% \end{itemize}

\section{Bidirectionalisation}
% \begin{itemize}
% \item syntactic restraints (like variable use and nested function
%   calls) vs polymorphic restraints and no updates to shape

% \item cost / difference function, minimal cost / difference is chosen
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
authors also worked out a combined approach, which is at least as good
as the better one of the two techniques. %

Matsuda et al. introduce a general first-order functional language
called \emph{VDL}. %
\emph{VDL} has two syntactical restrictions, which we have to keep in
mind when talking about derivable functions: defined functions have to
be affine and treeless. %
In an affine function definition, every variable on the left hand side
is used at most once on the right hand side of the definition;
treeless characterises function definitions without immediate data
structures. %
Nevertheless, VDL allows function definitions using arbitrary
algebraic data structures, e.g., lists and trees. %
VDL then automatically derives appropriate put functions for these
uni-directional get functions. %
As a first step, VDL derives a complement function, secondly, the get
function and the complement form a pair, which must be injective. %
At last step, an inverse transformation is performed on the pair. %
In the end, the put function can be derived if the paired function and
its inverse can be derived effectively. %
There are two details, that we did not examine so far: determinism
property for the inverse transformation and further requirements for
the complement function. %
The inverse transformation is not guaranteed to be deterministic, it is
possible to generate equations with overlapping left-hand sides. %
In the case of nondeterministic programs, a backtracking search
becomes necessary, though, the authors state that it would be
preferable to only obtain deterministic programs\cite{synSemComb}. %
Furthermore, the complement, which we derive in the first step, must
be injective and minimal with respect to a collapsing order, which
needs to be defined. %
Fortunately, injectivity is decidable in VDL and the proposed algorithm
is sound and complete. %

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
i.e. which apply to the category of containers as defined by \cite{containers}. %

The approach utilises the fact that the get function is polymorphic
over its first arguments, i.e. the container's element. %
Therefore, we can assume that it does not depend on any concrete
element of its container, but only on positional information, which
are independent of the elements values. %
The use of free theorems allows us to inspect the effect of the |get|
transformation without knowing about the explicit implementation.  The
definition of |bff| simulates its first argument, i.e. the get
function, on an arbitrary container, like for example a list of
|Integer| if we use |[a]| as container. %
The container to simulate shares its shape property with the given
container, which is the second argument of |bff|; in the example of
lists, the simulation list and the given list need to be of the same
length. %
Every value in the simulated container has a corresponding value in
the given container. %
As a second step, we get a mapping from the simulated view and the
originally update view, when we apply the |get| function to the
simulated and the given container respectively. %
We combine both mappings with precedences to the second: if we find a
value im both mappings, we choose the one from the view. %
In the end, every element in the container we used for simulation are
replaced by their associated values according to the combined
mapping. %
Voigtl\"ander defines two additional functions, |bff_EQ| and |bff_ORD|,
which use the function of the type classes |Eq| and |Ord|
respectively. %
In order to apply his approach for a |get| function that duplicates
elements, the defined mapping fails because of its simple
definition. %
In a more practical mapping, equivalent elements in the original
container need to map to the same element in the arbitrary container
that we need for simulation. %
In this case, we need to compare the elements within the container,
this is where the |Eq| type class comes into play.  For the function
|bff_Ord|, the mapping needs a similar, but rather complicated and
more technical, adjustment in order to allow the use of free theorems
again. %

As an enhancement of the semantic approach, \cite{semRevisited}
presented a generalisation that extends the range of |get| function to
higher order functions that are not expressed by type classes, or
depend on different type classes than |Eq| and |Ord|. %
Instead of three single functions, like in Voigtl\"ander's work, Wang
and Najd define a |bffBy| function that takes and observer function as
first argument that gives rise to equivalence properties of the
elements. %
The approach uses these observer functions to build the mappings as in
the original approach. %
These mappings are called observation tables here, and generalise the
explicite usage of different functions for different type class
dependencies. %

As a second enhancement, \cite{biForFreeImprove} introduce a type
class to extend the range of |get| functions to monomorphic
transformations. %
The main idea is to provide a type class |PackM delta alpha mu| to
convert polymorphic functions into monomorphic ones. %
The type variable |delta| represents the type of the concrete data
structure, whereas |alpha| is the type of the abstracted value. %
The last type variable |mu| is the used monad, which tracks the
observation made by the transformation on values of the concrete
structure. %
In short, the approach replaces monomorphic values in the definition
of the |get| function with polymorphic values. %
These polymorphic values are constructed from the original monomorphic
values. %
In contrast to the original approach, equal values in the original
container are not mapped to the same values in the simulated version,
that is, the authors consider a more traditional positional mapping. %

In addition, the semantic bidirectionalisation uses free theorems also
to prove consistency conditions. %
We discussed the syntactical bidirectionalisation, which formulates
its derivation on the ground of the \emph{GetPut} and \emph{PutGet}
law, in contrast, Voigtl\"ander proves, with the help of free
theorems, for each of his function definitions, |bff|, |bff_EQ| and
|bff_ORD|, that they obey the lens laws. %
That is, instead of a correctness-by-construction approach, the laws
are verified by hand. %

It becomes clear that both approaches have their pros and cons,
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

\section{Get-Lenses vs Put-Lenses}
% Get
% \begin{itemize}
% \item get is intuitive
% \item underspecified put, when only get is defined
% \item non-injective get functions
% \end{itemize}
% Put
% \begin{itemize}
% \item generated get is unique \cite{putback}
% \begin{itemize}
% \item requires putback functions to be affine and in treeless form, that
%   is, each view variable is used at most once and no
%   intermediate data structures are used in definitions
% \item this class of functions has similarities to tree transducers
% \item assumes only total functions
% \item hybrid compositional approach, but focus on designing language to
%   specify various primitive putback functions over algebraic data structures
% \item validity of putback transformations - A put is valid, if there
%   exists a get such that \emph{GetPut} and \emph{PutGet} are satisfied
% \item Uniqueness of get - Given a put function, there exists at most one
%   get function that forms a well-behaved BX
% \end{itemize}
% \item better suited for implementation with Curry
% \end{itemize}

\section{Implementation in Curry}
\subsection{Combinatorial Lens Library}
\subsubsection{Examples}
\subsection{Put-Lenses Library}
\subsubsection{Examples}