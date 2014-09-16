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
topic of bidirectional programming define their language by means of
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
The problem arises from the definition of |sub put get|: we ignore the
information about the original source, and examine the updated view
instead. %
After all, we do not have any information about the second component
of the given pair, because the definition of |sub fst get| takes only
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
definitions, is that we cannot ignore any part of the given source
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

The behaviour of the lens is rather simple and witness, but in this
case it is injective. %
The aim of this definition is to show that most get functions are not
of real use for defining a lens library, because, on one hand, they
are non-injective and, thus, do not have a uniquely defined corresponding
put function; on the other hand, if the definition is injective, its
behaviour is rather simple and not very useful. %
We presented typical lenses like |sub head get| and |sub head fst|,
which are still very simple, but already does not comply with the
injectivity requirement. %

In the end, the implementation of such a simple get-based is not
promising; as a short excursion, we discuss related work on get-based
lens implementation and how they tackle the problem of ambiguity. %

\subsection*{Excursion: Related Work on Get-Based Lenses}
Most approaches try to build their bidirectional language with respect
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
the results of the transformations are more comprehensible the more
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
techniques or applicable restrictions to avoid ambiguous put
functions. %
Instead, we focus on the put direction of lens definitions and search
for an applicable get direction. %

\subsection{Putting it straight}
\cite{putback} were the first to realise that the ambiguity of a get
function is unavoidable and propose to define lenses
by means of the put function instead. %
Curry's built-in search capabilities form a fruitful ground for a
bidirectional library that focuses on one direction and calculates the
corresponding function for the other direction. %

The pivot of the library is a very simple definition to represent
lenses by means of the put direction, and its selectors |sub put put|,
|sub get put| to use a given lens in the put and get direction,
respectively. %

%format PutLens = "Lens_{put}"
\begin{spec}
type PutLens s v = s -> v -> s

(sub put put) :: PutLens s v -> s -> v -> s
(sub put put) = id

(sub get put) :: PutLens s v -> s -> v
(sub get put) lens s | (sub put put) lens s v' == s = v'
   where v' free
\end{spec}

The idea of the definition of |sub get put| is symmetric to |sub put
get| from the get-based approach above. %
In this case, we use the PutGet law to define the requirements for an
appropriate get function for a given lens definition. %
That is, Curry searches for an updated view that yields the given
source, when we call the lens in the put direction with that source
and the updated view. %

As usual, we define a lens to project the first component of a pair. %
Actually, In the setting of a put-based lens library, we want to
define a lens to set the first component of a pair. %

\begin{spec}
(sub fst put) :: PutLens (a,b) a
(sub fst put) (_,y) z = (z,y)
\end{spec}

However, this lens definition is supposed to be equivalent to other
examples of |fst| in the setting of lenses. %
Since we have already defined the update strategy in the put
direction, we have to check if the get direction actually yields the
first component of the given pair. %

\begin{spec}
> (sub put put) (sub fst put) (1,2) 13
(13,2)

> (sub get put) (sub fst put) (13,2)
13
\end{spec}

Clearly, the get direction works as expected -- but how exactly does
Curry evaluate this expression? %
In order to examine the question a little more, we take a closer look
at the evaluation steps for the expression |(sub get put) (sub fst
put) (13,2)|. %

\def\commentbegin{\quad\{\ } \def\commentend{\}}
\begin{spec}
  (sub get put) (sub fst put) (13,2)
== {- (1) evaluate |(sub get put)|; replace guard of definition with if-then-else expression -}
  if (sub put put) (sub fst put) (13,2) v' == (13,2) then v' else failed
    where v' free
== {- (2) evaluate (sub put put) to |id| and apply |id| it the given arguments-}
  if (sub fst put) (13,2) v' == (13,2) then v' else failed
    where v' free
== {- (3) evaluate |(sub fst put) (13,2) v'| to |(v',2)|; the free variable is not bound yet-}
  if (v',2) == (13,2) then v' else failed
    where v' free
== {- (4) in order to evaluate |(==)|, the free variable is bound to |13| -}
   {- replace each occurrence of |v'| with |13| -}
  if (13,2) == (13,2) then 13 else failed
== {- (5) evaluate the if-condition to |True| -}
  if True then 13 else failed
== {- (6) -}
  13
\end{spec}

The definition of |sub get put| introduces a free variable that is
bound in the process and represents the value that the function yields
as a result. %
The most important evaluation takes place at step 4, where the
operator |(==)| forces its left argument to be evaluated in order to
reduce whole conditional expression to a boolean value. %
The left argument consists of the free variable, which is then bounded
to the appropriate value that evaluates the condition to |True|. %
In this case, the expression |(v',2) == (13,2)| evaluates to |True| if
both expressions can be reduced to the same value; the second
components of the two pairs are already equivalent, thus, the free
variable |v'| needs to be bound to the first component of the right
pair, i.e., |13|. %

The most important function the library contains in order to define
useful lenses is the composition operator for lenses. %
As before, we define |(sub ((<.>)) put)| as composition of two lenses
|l1 :: PutLens a b| and |l2 :: PutLens b c| to gain a resulting lens
of type |PutLens a c|. %

\begin{spec}
(sub ((<.>)) put) :: PutLens a b -> PutLens b c -> PutLens a c
(sub ((<.>)) put) lAB lBC sA vC = put lAB sA sB
  where sB = put lBC ((sub get put) lAB sA) vC
\end{spec}

In order to see the composition operator in action, we need a second
lens to connect two lenses in series. %
For the purpose of a more useful example, we first define an algebraic
datatype for a contact, like in an address book. %
The context consists of an address and information about the contact's
name, i.e., its first and last name. %

\begin{spec}
type Name     = (String,String)
type Address  = String
data Contact  = Contact Name Address

address :: PutLens Contact Address
address (Contact name _) newAddress = Contact name newAddress

name :: PutLens Contact Name
name (Contact _ address) newName = Contact newName address
\end{spec}

Additionally, we have two lenses to operate on the algebraic type
|Contact|: one selector to change and project the name, the other one
for the address. %
The second lens, |name|, yields the name a contact, which is
represented as a pair. %
In order to define a lens that operates directly on the first name of
a contact, we can compose |name| with |(sub fst put)|. %

\begin{spec}
firstName :: PutLens Contact String
firstName = nameLens <.> (sub fst put)
\end{spec}

For the purpose of showing these lenses in action, we define exemplary
values of type |Contact|, use both selectors to project and change the
name and address, respectively. %

\begin{spec}
contact1 = Contact ("Bob","Dylan") "Folkstreet 13"
name1 = ("Haskell","Curry")

> (sub put get) name contact1
("Bob Dylan")

> (sub put put) name contact1 name1
Contact ("Haskell","Curry") "Folkstreet 13"

> (sub put get) address contact1
"Folkstreet 13"

> (sub put put) address contact1 "Folk Street 39"
Contact ("Bob","Dylan") "Folkstreet 39"
\end{spec}

Furthermore, we apply the composed lens to |contact2| to change only the
first name and set it to |"Haskell B."|. %
We can also project the first name of |contact1| with the composed
lens. %

\begin{spec}
> (sub put put) firstName contact2 "Haskell B."
Contact ("Haskell B.","Curry") "Leibniz Way 21"

> (sub get put) firstName contact1
"Bob"
\end{spec}

\subsection{What About Well-Behavedness?}
Up to now, we have not discussed any requirements for the definition
of lenses in order to guarantee well-behavedness. %
The attentive reader may remember the two important laws: GetPut and
PutGet. %
Because of the fact that we already use underlying equation of the
GetPut law, the definition of put-based lenses in our library
guarantees to fulfil the GetPut law. %
Unfortunately, we cannot make any guarantees in case of the PutGet
law. %
Instead, we provide an additional library to test properties like
PutGet and GetPut. %
In the case of put-based lenses, we can express the requirements for
well-behavedness with the put function only. %
We have already discussed this modification in Section \ref{bi:fisher}
and introduced the laws \emph{PutInj} and \emph{PutTwice}. %
The implementation of our testing library is build on an old version
of
EasyCheck\footnote{http://www-ps.informatik.uni-kiel.de/currywiki/tools/easycheck}. %
EasyCheck is a lightweight library for specification-based testing in
Curry, which is implemented by \cite{easyCheck}.\footnote{The
  implementation of EasyCheck is highly motivated by the work of
  \cite{quickCheck}, who introduced a testing library for Haskell that
  has achieved a very good reputation in the Haskell community and is
  still excessively used.} %
In a nutshell, the library provides function to define specifications
and tests theses specifications by enumerating possible values that
obey the given type dependencies. %
In case of an error, the library provides the tested value that
contravenes the given specification as well as the false result. %

We had to make some adjustment to the implementation because the
latest version of the EasyCheck was written for KiCS, a
predecessor of the currently used and maintained KiCS2 compiler. %
These adjustment cover mostly the renaming of used libraries and
reimplementing modules that are not part of the KiCS2 contribution
anymore. %

In the end, the library \texttt{LensCheck} provides a set of testing
functions in order to check several properties of user-defined
lenses. %
First of all, the library consists of functions |checkGetPut| and
|checkPutGet| to test the traditional two round-tripping rules and an
additional testing function |checkPutPut| for the PutPut law. %
For the purpose of put-based properties as proposed by Fisher et al.,
the library provides testing functions |checkPutDet| and
|checkPutStab|. %
In addition, all five properties can be tested in the context of list
with a especially defined version, e.g., |checkListGetPut| and
|checkListPutDet|.\footnote{We include a function for this special
  case, because it is explicitly recommended in the paper of Fisher et
  al. to use an additional function to exclude the empty list as value.} %

In order to cut a long story short, the library does not prevent the
user to define inaccurate lenses. %
Unfortunately, the only guarantee the library gives is in the context
of PutGet. %
Additional properties have to be checked manually by the user in order
to prevent misbehaved lens definitions. %
