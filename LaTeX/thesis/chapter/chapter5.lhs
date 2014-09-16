\chapter{Lens Implementations in Curry}\label{ch:curryImpl}

In this chapter, we discuss two implementations of lenses in the
functional logic programming language Curry: a combinatorial approach
that focuses on the put function, and a put-based implementation build
upon Curry's built-in search abilities. %
Moreover, we present \emph{nondeterministic lenses}, a conservative
extension that arises naturally in the setting of functional logic
programming with Curry. %

The first implementation is a combinatorial approach that is based on
the Haskell library \texttt{putlenses} introduced in Section
\ref{sec:comb}. %
The original library is built on monadic combinators that include a
get and a put function, but the user only defines the put direction of
a lens when using this library. %
For the Curry implementation, we adapt the underlying monadic approach
by using Curry's built-in nondeterminism as the update strategy. %

As the second implementation, we discuss a very simple libraryy that
builds lenses only with the help of a put definition that does not
uses the combinatorial approach. %
In order to use lenses in the get direction, the library offers
a general get function based on the given put definition and the lens
laws. %
In addition, we discuss a potential get-based approach that follows
the same idea as our implementation, argue about its disadvantages,
and present solutions in related work. %

In the context of nondeterministic lenses, we present adapted versions
of the classical lens laws as well as a handful of lens definitions
well-suited for a nondeterministic setting. %


%include chapter5/CombinatorialLenses.lhs

%include chapter5/PutLenses.lhs

%include chapter5/NondeterministicLenses.lhs


% \section{Get-Lenses vs Put-Lenses}\label{sec:GetVsPut}
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
