\section{Put-Lenses Library}\label{sec:implPut}
\todo{Introduction}


\subsection{Getting in the Way of Productive Results}
State of the art: define get direction and derive corresponding put.

\begin{spec}
type (sub Lens get) s v = s -> v

(sub get get) :: (sub Lens get) s v -> s -> v
(sub get get) = id

(sub put get) :: (sub Lens get) s v -> s -> v -> s
(sub put get) lens s v | (sub get get) s' == v = s'
   where s' frx
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

Unfortunately, this get-based implementation is rather simple, not to say,
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
The initial concept was proposed by \cite{constraintMaintainers},
whose framework of constraint maintainers for user interaction is
sometimes called a pioneer work in the topic of bidirectional
transformations and lenses. %
In his work, he states that the transformations, that for example take
place in UIs, are supposed to be as minimal as possible in respect to
the given constraint; this approach aims to be user-friendly, because
the results of the transformations are more comphrehensible the more
they are related to the initial situation. %

More recently,
\cite{stateToDeltaLenses,stateToDeltaLensesAsymmetric,stateToDeltaLensesSymmetric}
follow this approach in their work about \emph{delta lenses}; they
cover asymmetric as well as symmetric lenses. %
The general idea is to distinguish between the computed delta and the
effectively update propagation; the get as well as the put function
take the computed delta under consideration. %
The computed delta helps to Therefore, delta lenses consist of a get
and put function with a computed delta between original and updated
source as an additional argument. %
Diskin et al. develop a framework on the grounds of algebraic theory,
and this idea of delta lenses is a conservative extension to the
original lens framework, that is, the framework can reproduce the
behaviour of ordinary lenses. %

Additionally, \cite{matchingLenses} put the theory into practice:
their development on a new core language of matching lenses for
strings can be seen as enhancement of their domain-specific language
Boomerang (see Section \ref{sec:comb}). %
The framework parametrises lenses with respect to heuristics in order
to calculate alignments. %
So-called \emph{chunks} are used to label each element of the source
and to recognise these elements, when they are modified with an
updated view. %
As a drawback, the use of such one-to-one mappings as alignment
strategy leads to a positional alignment only. %
That is, every element of the source needs to have a corresponding
element in the view and vice versa; the focus lies on the data and the
original shape is ignored during alignment. %

At this point, the work of \cite{editLenses} and
\cite{deltaLenses,leastChangeLenses} comes into play. %
The former approach develops a theory of \emph{edit lenses}; the main
difference to basic lenses is their focus on changes of structures
similar to the idea behind delta lenses. %
Edit lenses establish the connection between original and updated
source, an approach that does not allow any guessing, but has a strict
rule to apply the resulting alignment.  Hofmann et al. describe these
lenses with a standard mathematical notion of monoids and monoid
actions, where the former corresponds to the description of edits and
the latter describes the actual application of such edits to the given
structure. %
Whereas Diskin et al. merely propose a theoretical framework for
descriptions of changes, Hofmann et al. introduce a more mature
approach with additional combinators, e.g., composition, sums,
products etc, that give rise to brighter area of application. %
Most recently, \cite{symmetricEditLenses} finished his dissertation
about edit lenses in a symmetric setting that gives rise to the latest
developments in that area. %

Pacheco et al. identified that positional updates are only reasonable
for data alignment, but shape alignment needs to be considered
separately. %
Their approach tackles the problem of positional alignment and
introduces an explicite separation of shape and data. %
In their paper, they describe a point-free delta lens language in a
dependent type setting, which is based on their early work of
point-free lenses \citeyearpar{pointfree}. %
They distinguish between horizontal and vertical deltas; the former
describes an update, where source and view values are of different
types, and the latter is special case, which describes updates for
values of the same type. %
Pacheco et al. criticise the lack of shape alignments in related work
on lenses. %
Recent approaches focus on aligning the data of source and view, but
fail to establish a convenient mapping of both shapes. %
This positional alignment leads to less predictable updates regarding
insertion and deletion of elements, which either are not detected or
effect only the end positions of the underlying structure, e.g. new
elements are inserted at the end of a list. %
Thus, the main effort of Pacheco's et al. work are recursion patterns
for horizontal delta lenses, which introduce shape alignments for
combinators like fold and unfold. %

\subsection{Putting it straight}

\begin{spec}
type (sub Lens put) s v = s -> v -> s

(sub put put) :: (sub Lens put) s v -> s -> v -> s
(sub put put) = id

(sub get put) :: (sub Lens put) s v -> s -> v
(sub get put) lens s v | (sub put put) lens s v' == s = v'
   where v' free
\end{spec}


\subsection{Examples}\label{subsec:implPutEx}
