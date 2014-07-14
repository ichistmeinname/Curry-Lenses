\chapter{Different implementation approaches}

\section{Combinatorial lenses}
\begin{itemize}
\item pioneer work by \cite{biTCombinators}
\item put combinators \cite{putCombinators}
\begin{itemize}
\item get functions in general not injective: many possible corresponding
      put functions exist to form a well-behaved lens
\item \emph{PUTINJ}: |put s| is injective for any source |s|, i.e., $s' \in |put
  s v| \wedge s' \in |put s v'| \Rightarrow v = v'$
\item \emph{PUTTWICE}: $s' \in |put s v| \Rightarrow s' = |put s' v|$
\end{itemize}
\end{itemize}

\section{Bidirectionalization}
\begin{itemize}
\item technique to transform uni-directional functions into
  bidirectional ones
\item syntactic restraints (like variable use and nested function
  calls) vs polymorphic restraints and no updates to shape
\end{itemize}

\subsection{Get-Lenses}
\begin{itemize}
\item get is intuitive
\item underspecified put, when only get is defined
\item non-injective get functions
\end{itemize}

\subsubsection{Possible Implementation}
\begin{itemize}
\item cost / difference function, minimal cost / difference is chosen
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
\item and additional enhancements \cite{biForFreeImprove}
  \cite{semRevisited} \cite{enhanceSem}
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
\item syntactic derivation via view complement \cite{viewComp}

  \begin{itemize}
  \item general first-order functional language: VDL
  \item affine (each variable is used at most once)
  \item treeless (no intermediate data structure is used in the
    definition)
  \item view complement functions ca be automatically derived from
    view functions
  \item inference system for validation of changes in the view
  \item bidirectional transformation between arbitrary algebraic data
    structures (like lists and trees)
  \item three steps: derivation of complement function, tupling and
    inverse transformation
  \item minimal complement
  \item injectivity is decidable in VDL
  \item algorithm to check for injectivity: sound and complete
  \item backward transformation can be derived if the tupled function
    and its inverse can be derived effectively
  \end{itemize}

\item combination of both \cite{synSemComb}
\end{itemize}

\subsection{Put-Lenses}
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

\subsubsection{Implementation}
\subsubsection{Adapted laws}
\begin{itemize}
\item \emph{PutDeterminiation}: $\forall s,s',v,v' . put s v = put s' v'
  \rightarrow v = v'$
\item \emph{PutStability}: $\forall s . \exists v . put s v = s$
\item validity - A put function is valid if and only if it satisfies the
  \emph{PutDeterminiation} and \emph{PutStability} properties
\end{itemize}
\subsubsection{Examples}
