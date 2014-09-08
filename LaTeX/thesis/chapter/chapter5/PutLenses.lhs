\section{Put-Lenses Library}\label{sec:implPut}
In contradiction to most of the works in the area of bidirectional
programming, Fisher et al. set their focus on defining the put
direction instead of a get function. %
With this approach, they want to avoid a whole range of functions that
are not suited for the get direction because of the ambiguity of their
corresponding put function. %
The remainder of this section deals with a very simple implementation
of a lens library in Curry that sets its focus on the put functions as
well, and gives some examples for better comprehension. %
In order to motivate our approach, we start with a discussion about
the current state of the art and why we decided on focussing on the
put function anyway. %

\subsection{Getting in the Way of Productive Results}
As we have seen so far, most libraries and languages tackling the
topic of bidirectional programing define their language by means of
the get direction. %
This view on bidirectional programming arises from the way programmers
are used to define functions. %
Programmers are more familiar with defining a get function -- a
function that selects information of a structure -- rather than
thinking about an appropriate update strategy. %
Therefore, let us take a look at a very simple implementation of a
get-based approach. %

\begin{spec}
type (sub Lens get) s v = s -> v

(sub get get) :: (sub Lens get) s v -> s -> v
(sub get get) = id

(sub put get) :: (sub Lens get) s v -> s -> v -> s
(sub put get) lens s v | (sub get get) s' == v = s'
   where s' free
\end{spec}

We define a type synonym for lenses that is equivalent to the
signature of a get function, i.e., we represent lenses as their get
function. %
This definition leads to a straightforward implementation of a get
function for lenses: we just apply the given lens to a given source. %
However, this implementation of lenses lacks information for the put
direction. %
As a quick reminder: we want to define a function |(sub put get)| that
updates a given source with a modified view with respect to the given
lens. %
The given lens is a function that projects some view from a given
source, thus, in contradiction to our previous approach of a
combinatorial language, here, the given lens has no further
information about the update strategy. %
All we know about the given lens is that is needs to obey to certain
round-tripping laws to be well-behaved: GetPut and PutGet. %
That is, we can define the put definition by means of the get
definition with respect to the PutGet law. %

In order to see this implementation in action, we take look at our
running example in the setting of get-based lenses: we define |fst|
by means of get-based lenses. %

\begin{spec}
(sub fst get) :: (sub Lens get) (a,b) a
(sub fst get) (x,_) = x
\end{spec}

This lens definition is very simple, and constitutes a very familiar
setting for the programmer: we want to project the first component of
a given pair, thus, we ignore the second component and simply yield
the first component. %
If we run exemplary function calls of |sub get get| and |sub put get|,
we get more or less satisfactory results. %

\begin{spec}
> (sub get get) (sub fst get) (1,42)
1

> (sub put get) (sub fst get) (1,42) 3
(3,_x1)
\end{spec}

In the get direction, everything works as expected, but,
unfortunately, the put function yields a free variable as its second
component as result. %
This result leads to the realisation that the get-based implementation
is rather simple, not to say, too simple. %
If we use |sub fst get| in the put direction, we loose the additional
information of the source pair. %
The problem arises from the definition of |sub get put|: we ignore the
information about the original source, and examine the updated view
instead. %
After all, we do not have any information about the second component
of the given pair, because the defintion of |sub fst get| takes only
the first component under consideration. %
In many cases, the discarded original source leads to an ambiguous put
function; in particular, the above implementation is only applicable
in case of injective get functions. %
Why do we need injective functions for this setting? %
An injective function represents a one-to-one mapping, it preserves
distinctness; that is, there are no two elements of the source domain
that map to the same element in the codomain. %
In the case of our example lens |sub fst get|, there are several
source pairs that yield the same result; in fact, every pairs with the
same first component yield the same view. %

\begin{spec}
> (sub get get) (sub fst get) (1,42)
1

> (sub get get) (sub fst get) (1,"Hello World")
1

> (sub get get) (sub fst get) (1,True)
1
\end{spec}

The fact that |sub fst get| is not injective leads to an ambiguity
issue and, hence, to a non-constructive put function. %

In order to make the point more clear, we discuss a second lens
definition that is not injective. %
The common function |head| that projects the first element of the list
yields the same element for lists that start with the same element. %
That is, we can define a get-based lens that behaves exactly as |head|
in the get direction. %

\begin{spec}
(sub head get):: LensGet [a] a
(sub head get) (x:_) = x
\end{spec}

As a fun side-effect, the definition of |sub head get| looks a lot
like |sub fst get|. %
As a matter of fact, these lenses even behave similarly. %

\begin{spec}
> get (sub head get) [1..5]
1

> put (sub head get) [1..5] 13
(13:_x8)
\end{spec}

The get direction behaves flawlessly, however, the problem lies in the
put function, again. %
In this case, the put function introduces a free variable in the
resulting list: the first element of the list is specified to the
given view, i.e., |13|, but the remaining list is unspecified. %
The lack of a one-to-one correspondence between a given list and the
resulting value when applying |sub head get| to that list leads to a
free variable, which can be instantiate to any list and still fits the
requirements of the used lens. %

How do get-based lens definitions look like that are applicable for
our implementation approach? %
As a main requirement, we need to define injective get functions. %
The first observation, which we made in the context of the previous two lens
defintions, is that we cannot ignore any part of the given source
without losing information for the corresponding put function. %
Let us take a look at a injective get function that is defined on
pairs, with |Int|s as its first component and an arbitrary type for
its second component. %

\begin{spec}
(sub incFst get) :: LensGet (Int,b) (Int,b)
(sub incFst get) (x,y) = (x+1,y)
\end{spec}

In the definition of the lens |sub incFst get|, we increment the first
component and do not touch the second one, but, in contrast to the
previous lens definition, we do not ignore the second component, it is
still a part of the resulting view. %
That is, we have a lens that maps pairs to pairs. %

\begin{spec}
> get (sub incFst get) (1,"Hello World")
(2,''Hello World'')

> put (sub incFst get) (1,"Hello World") (2,"Haskell B. Curry")
(1, "Haskell B. Curry")
\end{spec}

The behavior of the lens is rather simple and witness, but in this
case it is injective. %
The aim of this definition is to show that most get functions are not
of real use for defining a lens library, because, on one hand, they
are non-injective and, thus, do not have a uniquely defined corresponding
put function; on the other hand, if the definition is injective, its
behaviour is rather simple and not very usefull. %
We presented typical lenses like |sub head get| and |sub head fst|,
which are still very simple, but already does not comply with the
injectivy requirement. %

In the end, the implementation of such a simple get-based is not
promising; as a short excursion, we discuss related work on get-based
lens implementation and how they tackle the problem of abiguity. %

\subsection*{Excursion: Related Work on Get-Based Lenses}
Most approaches try to build their bidrectional language with respect
to a specific application area, e.g., XML data, strings or databases,
however, in the following, we discuss some approximations for a more
general approach. %

There are several existing ideas to overcome these limitations
regarding ambiguity. %
One of the most popular ideas is to choose the best put function based
on similarities and differences between the original source and its
potential update. %
The initial concept was proposed by~\cite{constraintMaintainers},
whose framework of constraint maintainers for user interaction is
sometimes called a pioneer work in the topic of bidirectional
transformations and lenses. %
In his work, he states that the transformations, that for example take
place in UIs, are supposed to be as minimal as possible in respect to
the given constraint; this approach aims to be user-friendly, because
the results of the transformations are more comphrehensible the more
they are related to the initial situation. %

More recently,~\cite{stateToDeltaLenses,stateToDeltaLensesAsymmetric,stateToDeltaLensesSymmetric}
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

Additionally,~\cite{matchingLenses} put the theory into practice:
their development on a new core language of matching lenses for
strings can be seen as enhancement of their domain-specific language
Boomerang (see Section~\ref{sec:comb}). %
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

At this point, the work of~\cite{editLenses}
and~\cite{deltaLenses,leastChangeLenses} comes into play. %
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
Most recently,~\cite{symmetricEditLenses} finished his dissertation
about edit lenses in a symmetric setting that gives rise to the latest
developments in that area. %

Pacheco et al. identified that positional updates are only reasonable
for data alignment, but shape alignment needs to be considered
separately. %
Their approach tackles the problem of positional alignment and
introduces an explicite separation of shape and data. %
In their paper, they describe a point-free delta lens language in a
dependent type setting, which is based on their early work of
point-free lenses~\citeyearpar{pointfree}. %
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

In this thesis, we do not investigate additional measurement
techniques or applicable restrictions to avoid ambiguious put
functions. %
Instead, we focus on the put direction of lens definitions and search
for an applicable get direction. %
As a first step, we

\subsection{Putting it straight}
\cite{putback} were the first to realise that the ambiguity of a get
function is unavoidable and propose to define lenses
by means of the put function instead. %
Curry's built-in search capabilities form a fruitful ground for a
bidirectional library that focuses on one direction and calculates the
corresponding function for the other direction. %

The pivot of the library is a very simple definition to represent
lenses and its selectors |sub put put|, |sub get put| to use a given
lens in the put and get direction, respectively. %

\begin{spec}
type (sub Lens put) s v = s -> v -> s

(sub put put) :: (sub Lens put) s v -> s -> v -> s
(sub put put) = id

(sub get put) :: (sub Lens put) s v -> s -> v
(sub get put) lens s | (sub put put) lens s v' == s = v'
   where v' free
\end{spec}

The idea of the definition of |sub get put| is symmetric to |sub put
get| from the get-based approach above. %
In this case, we use the PutGet law to search for an appropriate get
function for a given lens definition. %
That is, Curry searches for an updated view that yields the given
source, when we call the lens in the put direction with that source
and the updated view. %


% \subsection{Examples}\label{subsec:implPutEx}
