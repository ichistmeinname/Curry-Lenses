\section{Combinatorial Lens Library}\label{sec:implComb}

We already discussed two different approaches on combinatorial
frameworks for lenses in Section \ref{sec:comb} to provide an insight
of recent implementations. %
As a matter of fact, the combinatorial library for Curry is based on
the approach of Fisher et. al, who published the Haskell library
\texttt{putlenses} as a result of their work on lenses. %
In the remainder section, we discuss the underlying implementation
with respect to its original counterpart, and present some of the most
important combinators as representatives. %
In addition, we give some exemplary lens definitions to show the usage
of the library, and the definition of classical lens examples with
the available combinators. %

\subsection{Motivation}
The simplest representation of lenses is a pair of function; one
function for the get direction and one for put. %
We can define such a data structure in Curry in three different ways:
as a data type declaration with an own constructor, as a record
type\footnote{We will not pursue the usage of record types here, but
  seize the concept in Section \ref{sec:records}.}, or as type synonym. %
For the purpose of simplicity, we define lenses with a simple type
synonym for a pair of get and put function. %

\begin{spec}
type (sub Lens simple) s v = (s -> v, s -> v -> s)

(sub put simple) :: (sub Lens simple) s v -> s -> v -> s
(sub put simple) = snd

(sub get simple) :: (sub Lens simple) s v -> s -> v
(sub get simple) = fst
\end{spec}

In addition to the definition of a lens type, we can access the get
and put component of the pair with helper functions |sub get simple|
and |sub put simple|, respectively. %
Next, we define a simple lens with a pair of arbitrary type as source
that projects its first component in the get direction and updates the
first component in the put direction; the update does not effect the
second component. %

\begin{spec}
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

Do the laws still hold? Manual checks with regard to consistency and
validity.

\subsection{Implementation}

\begin{spec}

\end{spec}


\subsection{Examples}\label{subsec:implCombEx}
