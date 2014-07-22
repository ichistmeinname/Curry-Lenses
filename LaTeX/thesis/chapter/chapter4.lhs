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
strucutres. %
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
rebustness and and ease of use. %
That is, their language guarantees well-behavedness and the totality
of the primitive transformations with the integrated type system. %
The underlying type system, and with that the type safety, is the main
contribution to the field of bidirectional programming. %
The authors state close connections with topics from the database
community: lenses are a well-known abstraction in databases concering
tables and queries, and the \emph{update translation under a constant
  complement}\todo{citation?} tackles problems concerning definedness
and continuity, whereas the well-behavedness of lenses corresponds to
\emph{update translators}\todo{citation}. %
In their paper, Foster et al. define a handful of primitive lens
combinators for trees, and the combination of these primitve lenses results in a powerful
abstraction to describe transformations. %
The most important primitives are the composition, identity and
constant lens. %
These definitions work on arbitrary data structures, whereas all other
combinators specialise on tree data strucures only. %
The defined transformations are closely related to tables and views
from databases: a transformations maps a concrete structure into an
abstract view, and maps a possibly modified abstract view together
with the orginal concrete structure back to a modified concrete
structure. %
In the DSL, the user defines the forward transformation in a
straight-forward fashion, whereas the backward transformation is the
result of reading the definition from right to left. %
The work of Foster et al origins in the Harmony
project\todo{citiation}, a generic framework to synchronise
tree-structured data; an ongoing example throughout their work is the
synchronisation of different browser bookmarks, calendar and address book formats. %
They continued their work on lenses, a project called
Boomerang\todo{citation} focuses on string data instead of
tree-structure data and is successfully used as a
framework\todo{citation of configuration tool}. %
As well as in ther first work, their language is based on a type
system to guarantee well-behavedness and totality and a rich set of
lens combinators to define powerful transformations on strings. %
\todo{check statements about type safety again; does only hold for
  primitive lenses; user-definied functions are only checked
  statically}
 

\begin{itemize}
\item pioneer work by \cite{biTCombinators} $\checkmark$

\item put combinators \cite{putCombinators}
\begin{itemize}
\item get functions in general not injective: many possible corresponding
      put functions exist to form a well-behaved lens
\item \emph{PUTINJ}: |put s| is injective for any source |s|, i.e., $s' \in |put
  s v| \wedge s' \in |put s v'| \Rightarrow v = v'$
\item \emph{PUTTWICE}: $s' \in |put s v| \Rightarrow s' = |put s' v|$
\end{itemize}
\end{itemize}

\section{Bidirectionalisation}
Bidirectionalisation is the process of transforming uni-directional
into bidirectional functions. %
In the following, we present two techniques to bidirectionalise a
uni-directional get function: the first technique by \cite{viewComp}
syntactically derives a put function for a given get function,
whereas the second approach takes a more semantic approach to
generate an appropriate put function at runtime. %
Both techniques have their advantages and disadvantages, so that the
authors also worked out a combined approach, which is at least as good
as the better one of the two techniques. %

\begin{itemize}
\item syntactic restraints (like variable use and nested function
  calls) vs polymorphic restraints and no updates to shape
\end{itemize}

Matsuda et al. introduce a general first-order functional language
called \emph{VDL}. %
\emph{VDL} has two syntactical restrictions, which we have to keep in
mind when talking about derivable functions: defined functions have to
be affine and treeless. %
In an affine function defintion, every variable on the left hand side is
used at most once on the right hand side of the definition; treeless
characterises function definitions without immediate data structures. %
Nevertheless, VDL allows function definition using arbitrary algebraic data strucutes, e.g., lists and trees. %
VDL then automatically derives appropriate put functions for these uni-directional get
functions. %
As a first step, VDL derives a complement function, secondly, the get
function and the complement form a tupling, which must be injective. %
At last step, an inverse transformation is performed on the tupling. %
In the end, the put function can be derived if the tupled function and
its inverse can be derived effectively. %
There are two details, that we did not examine so far: determinism
property for the inverse transformation and further requirements for
the complement function. %
The inverse transformation is not guarenteed to be determinstic, it is
possible to generate equations with overlapping left-hand sides. %
In the case of nondeterministic programs, a backtracking search
becomes necessary, though, the authors state that it would be
preferabe to only obtain determinstic programs\cite{synSemComb}. %
Furthermore, the complement, which we derive in the first step, must
be injective and minimal with respect to a collapsing order, which
needs to be defined. %
Fortunately, injectivity is decidable in VDL and the propsed algorithm
is sound and complete. %


On the other hand, \cite{biForFree} introduces an approach for
semantic bidirectionalisation.

\begin{itemize}
\item cost / difference function, minimal cost / difference is chosen
\item syntactic derivation via view complement \cite{viewComp}$\checkmark$

\item semantic derivation using relational parametricity
  \cite{biForFree}
  \begin{itemize}
  \item semantic approach inspired by relational parametricity, uses
    free theorems for proving the consistency conditions
  \item higher-order function |bff :: forall a. [a] -> [a]) -> (forall
    a. [a] -> [a] -> [a]| implemented in Haskell that takes a
    polymorphic get function as argument and yields the appropriate
    put function, generic on input and output
  \item the resulting put function is not a syntatical defintion, but
    a functional value semantically equivalent to such a function
  \item no restrictions on a sublanguage, any Haskell function of
    appropriate type can be used as argument
  \item restriction: any update on the shape of the view (length of
    the list) leads to failure
  \item distiguishes between |bff|, |bff_EQ|, |bff_ORD|
  \item considered data structures: shape plus content (Categories of
    container by Th. Altenkirch et al)
  \item GetPut, PutGet laws are proven by free theorems
  \item main idea: use assumption that get is polymorphic over its
    element argument of type |a|, then, its behaviours does not depend
    on any concrete list elements, but only on positional information
  \item every position in the template list |[0..n]| has a mapping in
    g with its associated values in the original source; apply get to
    the template source to get an additional mapping h between the
    template view and the original updated view; combine both mappings
    to a mapping h' with precedence to h, when an integer template
    index is found in both mappings; in the end, fill the positions in
    the template mappin with the associated values according to the
    combined mapping h'
  \item simple approach fails for duplicated elements; solutions:
    elements of the list must be mapped in a more sophistaced way,
    equal elements in the origial list map to the same elements in the
    template
  \end{itemize}
\item and additional enhancements

\begin{itemize}
\item \cite{biForFreeImprove}
\item introduces a type class |PackM delta alpha mu| to turn monomorphic
  into polymorphic transformations; a concrete datatype |delta| is
  abstracted to a type |alpha| and the observerations made by
  transformations by values of type |delta| are recorded by a monad |mu|
\item |liftIO| lifts any observer function |[delta] -> beta| on a concrete
  datatype |delta| to a monadic function |[alpha] -> mu beta| on an
  abstract datatype |alpha| where |beta| is an instance of |Eq|
\item |Eq b| is needed to check validity of updates by comparing the
  observation results
\item polymorphism of |a| permits semantic bidirectionalisation and
  polymorphic |mu| guarantees integrity of the observations results
  recorded in the writer monad
\item consistency check in observation table: the observation results are
  not allowed to be different to possible update results, if they are
  different, the update is rejected; key for application of free
  theorems
\item optional locations for newly created elements that do not have a
  corresponding location in the original list
\item approach in this paper: same values are not mapped to the same
  label, that is, same values are not considered to be duplicates 
\item says its harder to find get functions that are suitable for syntactic bidirectionalisation than semantic
\end{itemize}

\begin{itemize}
\item \cite{semRevisited}
\item generalises Voigtlaender's approach for higher order functions that
  are not expressed by type classes
\item defines one function bff that
  is parametrized over a so-called observer function, and extends the associations
  maps to observer tables
\end{itemize}

\begin{itemize}
\item \cite{enhanceSem}
\item takes combination one step further: any syntactic
  bidirectionalisation approach can be \emph{plugged in} to obtain transformations on shapes
\item generalises approach from lists to arbitrary data types
\item enables bootstrapping in which pluggable bidirectionalisation is
  itself used as a plug-in
\end{itemize}
\begin{itemize}
\item generalises Voigtlaender's approach for higher order functions that
  are not expressed by type classes
\item defines one function bff that
  is parametrized over a so-called observer function, and extends the associations
  maps to observer tables in order to get rid of three seperate
  functions
\end{itemize}
\item and semantic approach with agda, i.e., dependent types
  \cite{semDependentTypes}

\item combination of both \cite{synSemComb}
\begin{itemize}
\item combines syntactic and semantic bdirectionalization
\item divides usage of both approaches by their specialties: semantic for
  content, syntactic for shape
\item inherits limitations in program coverage from both techniques: only
  functions written in first-order language, linear, treeless and
  moreover polymorphic are suitable for this approach
\item \emph{in a meaningful way}/\emph{suitable}: GetPut, PutGet, restricted to a
  defined |put s v'| for PutGet, and that |put s v| should be
  preferably defined for all |s| and |v'| of appropriate type
\item improved updatability: shape-changing update that are not applicable
  for semantic bidirectionalisation; superior to syntactic
  bidirectionalisation on its own in many cases
\item syntactic bidirectonalization as black box
\item semnatic bidirectionalisation as gass box: look into it and refactor
  it to enable a plugging in of the syntactic technique
\item syntactic technique on its own is never worse than the semantic
  technique own its own
\item assumes semantic linearity: for every $n$ |get [0..n]| does not
  contain any duplicates, which clearly is fulfilled, if |get|'s
  syntactic definition is linear: linearity rules out one important
  cause for a potential failure, namely potential equality mismatcg in |v'|
\item key idea: abstracting from lists to length of lists, or more
  generally, from data structures to shapes
\item uses |Nat| instead of |Int|, move from |[a]| to |Nat|: |get|-function gets simpler, no data
  values have to be kept; can lead to injectivity and, hence, to
  simpler complement functions
\item explicit bias: bias to apply when reflecting specific updated views
  back to the source level
\item this bias could be determined on a case-by-case basis, e.g. depending on
  a \emph{diff} between updated view and original view
\item approach does not hold PutPut and undoability law, albeit, the two
  approach on their owd do satisfy these laws
\end{itemize}
\end{itemize}

\subsection{Adapted laws}
\begin{itemize}
\item \emph{PutDeterminiation}: $\forall s,s',v,v' . |put s v = put s' v'|
  \rightarrow v = v'$
\item \emph{PutStability}: $\forall s . \exists v . |put s v = s|$
\item validity - A put function is valid if and only if it satisfies the
  \emph{PutDeterminiation} and \emph{PutStability} properties
\end{itemize}

\section{Get-Lenses vs Put-Lenses}
Get
\begin{itemize}
\item get is intuitive
\item underspecified put, when only get is defined
\item non-injective get functions
\end{itemize}
Put
\begin{itemize}
\item generated get is unique \cite{putback}
\begin{itemize}
\item requires putback functions to be affine and in treeless form, that
  is, each view variable is used at most once and no
  intermediate data structures are used in definitions
\item this class of functions has similarities to tree transducers
\item assumes only total functions
\item hybrid compositional approach, but focus on designing language to
  specify various primitive putback functions over algebraic data structures
\item validity of putback transformations - A put is valid, if there
  exists a get such that \emph{GetPut} and \emph{PutGet} are satisfied
\item Uniqueness of get - Given a put function, there exists at most one
  get function that forms a well-behaved BX
\end{itemize}
\item better suited for implementation with Curry
\end{itemize}

\section{Implementation}
\subsection{Combinatorial Lens Library for Curry}
\subsubsection{Examples}
\subsection{Put-Lenses for Curry}
\subsubsection{Examples}