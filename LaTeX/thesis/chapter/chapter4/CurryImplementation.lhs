\chapter{Implementation in Curry}\label{ch:impl}

\begin{spec}
type (sub Lens simple) s v = (s -> v, s -> v -> s)

(sub put simple) :: (sub Lens simple) s v -> s -> v -> s
(sub put simple) = snd

(sub get simple) :: (sub Lens simple) s v -> s -> v
(sub get simple) = fst

(sub fst simple) :: (sub Lens simple) (a,b) a
(sub fst simple) = (get', put')
   where
    get' :: (a,b) -> a
    get' (x,_) = x
    put' :: (a,b) -> a -> (a,b)
    put' (_,y) z = (x,z)
\end{spec}

high maintenance efforts (two functions to maintain), error-prone

\begin{spec}
(sub fstInc simple) :: (sub Lens simple) (a,Int) a
(sub fstInc simple) = (get', put')
   where
    get' (x,_) = x
    put' (_,y) x = (x,y+1)
\end{spec}

Do the laws still hold? Manual checks with regard to consistency and validity.\\

State of the art: define get direction and derive corresponding put.
\begin{spec}
type (sub Lens get) s v = s -> v

(sub get get) :: (sub Lens get) s v -> s -> v
(sub get get) = id

(sub put get) :: (sub Lens get) s v -> s -> v -> s
(sub put get) lens s v | (sub get get) s' == v = s'
   where s' free
\end{spec}

Our running example in the setting of get-based lenses. %
\begin{spec}
(sub fst get) :: (sub Lens get) (a,b) a
(sub fst get) (x,_) = x
\end{spec}

Very simple function definition, a very familiar setting for the
programmer. %

\begin{spec}
> (sub get get) (sub fst get) (1,42)
1

> (sub get put) (sub fst get) (1,42) 3
(3,_x1)
\end{spec}

Unfortunately, this get-based implementation is rather simple, say,
too simple. %
If we use |sub fst get| in the put direction, we loose the additional
information of the source pair. %
The problem arises from the definition of |sub get put|: we ignore the
information about the original source, and examine the updated view
instead. %
After all, we do not have any information about the second component
of the given pair, because the defintion of |sub fst get| takes only
the first component under consideration. %
In most cases, the discarded original source leads to an ambiguous put
function; in particular, the above implementation is only applicable
in case of injective get functions as we already pointed out in
Section \ref{subsec:lensesAlgebraic} %
There are several existing ideas to overcome these limitations. %
One of the most popular ideas is to choose the best put function based
on similarities and differences between the original source and its
potential update. %
\cite{stateToDeltaLenses,stateToDeltaLensesAsymmetric,stateToDeltaLensesSymmetric}
follow this approach in their work about \emph{delta lenses}; they
cover asymmetric as well as symmetric lenses. %
The general idea is to distinguish between the computed delta and the
effectively update propagation; the get as well as the put function
take the computed delta under consideration. %
Therefore, delta lenses consists of a get and put function with a
computed delta between original and updated source as an additional
argument. %
Diskin et al. develop a framework on the grounds of algebraic theory,
and this idea of delta lenses is a conservative extension to the
original lens framework, that is, the framework can reproduce the
behaviour of ordinary lenses. %

The example above is rather simple, but more complex lens definitions
exist that involve rearragned elements withing lists, trees, or other
container-line structures. %
Ongoing development deals with more convenient alignments than
traditional positional updates. %
\cite{editLenses} develop


\cite{editLenses} \cite{deltaLenses}. %


\begin{spec}
type (sub Lens put) s v = s -> v -> s

(sub put put) :: (sub Lens put) s v -> s -> v -> s
(sub put put) = id

(sub get put) :: (sub Lens put) s v -> s -> v
(sub get put) lens s v | (sub put put) lens s v' == s = v'
   where v' free
\end{spec}

\subsection{Combinatorial Lens Library}\label{sec:implComb}


\subsubsection{Examples}\label{subsec:implCombEx}


\subsection{Put-Lenses Library}\label{sec:implPut}


\subsubsection{Examples}\label{subsec:implPutEx}


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
