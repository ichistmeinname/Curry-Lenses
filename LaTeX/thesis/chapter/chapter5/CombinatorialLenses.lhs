\section{Combinatorial Lens Library}\label{sec:implComb}

We discussed two different approaches on combinatorial
frameworks for lenses in Section~\ref{sec:comb} to provide an insight
of recent implementations. %
The combinatorial library for Curry is based on
the approach of Fisher \etal{}, who published the Haskell library
\texttt{putlenses} as a result of their work on lenses. %
In the remaining section, we discuss the underlying implementation and
give additional comments on its original counterpart. %
Additionally, we present some of the most important combinators as
representatives. %
Last, we give some exemplary lens definitions to show
the usage of the library, and the definition of classical lens
examples with the available combinators. %

\subsection{Motivation}
The simplest representation of lenses is a pair of functions: one
function for the get direction and one for the put direction. %
We can define such a data structure in Curry in three different ways:
as a data type declaration with its own constructor, as a record
type\footnote{We will not pursue the usage of record types here, but
  seize the concept in Section~\ref{sec:records}.}, or as a type
synonym. %
For simplicity, we define lenses with a simple type synonym for a pair
of get and put functions. %

\phantomsection
\begin{spec}
type (sub Lens Pair) s v = (s -> v, s -> v -> s)

(sub put Pair) :: (sub Lens Pair) s v -> s -> v -> s
(sub put Pair) = snd

(sub get Pair) :: (sub Lens Pair) s v -> s -> v
(sub get Pair) = fst
\end{spec}
\label{ex:lensSimple}

In addition to the definition of a lens type, we can access the get
and put component of the pair with helper functions |sub get Pair| and
|sub put Pair|, respectively. %
Next, we define a simple lens with a pair of arbitrary type as source
that projects its first component in the get direction and updates the
first component in the put direction; the update does not effect the
second component. %

\begin{spec}
(sub fst Pair) :: (sub Lens Pair) (a,b) a
(sub fst Pair) = (get', put')
   where
    get' :: (a,b) -> a
    get' (x,_) = x
    put' :: (a,b) -> a -> (a,b)
    put' (_,y) z = (z,y)
\end{spec}

The definition of this lens is straight-forward, however, while we we
gain from simplicity, we lose in accuracy and maintainability. %
First of all, the given definition does not automatically form a
well-behaved lens, because we did not consider the lens laws
yet.\footnote{In fact, we know from Section~\ref{sec:lenses} that this
  exemplary lens definition is well-behaved.} %
Second, if we modify one of the two functions, we have to make sure
that the other one still harmonises with our modifications. %
That is, we have to check the lens definition manually with regard to
consistency and validity. %
This circumstance is error-prone and requires a high maintenance
effort, because we do not only have to check the lens property once
after definition, but every time we modify one of the lens
components. %
As an example, let us make a slight modification to the put function
and define a lens |fstInc|, which, additionally, increments its first
component in the put direction. %

\phantomsection
\begin{spec}
fstInc :: (sub Lens Pair) (a,Int) a
fstInc = (get', put')
   where
    get' (x,_) = x
    put' (_,y) x = (x+1,y)
\end{spec}
\label{ex:fstInc}

We have only made slight changes to the put function, which only
affect the second component. %
It would be interesting to check if the lens law still hold after our
modifications. %

\begin{spec}
(sub get Pair) fstInc ((sub put Pair) fstInc (1,2) 13)
> 14

(sub put Pair) fstInc (1,2) ((sub get Pair) fstInc (1,2))
> (2,2)
\end{spec}

The first expression checks the behaviour of the PutGet law, which
does not behave as expected; the second expression behaves rather
strangely with respect to the GetPut law. %
A very simple change breaks both laws, because we forgot to consider
both functions. %
We can easily fix this bug by changing the right-hand side of the get
direction to |x - 1|. %
However, this simple approach is not satisfactory at all. %
On the one hand, it does not \emph{feel} bidirectional, because we
still maintain two unidirectional functions in disguise. %
On the other hand, we do not want to check the lens laws for every
lens we define and every time again when we make changes to existing
definition. %

\subsection{Implementation}
In order to provide a more user-friendly library with less maintenance
effort, the library needs a rich set of combinators, which are already
well-behaved. %
Then, the user builds his own lenses with the help of these
combinators without considering any laws, but only with her
implementation in mind. %

In the following, we present a reimplementation of the Haskell library
\texttt{putlenses} in Curry. %
The original implementation provides a monadic interface to
instantiate different update strategies. %
Due to the lack of type classes in Curry, we can not use this
approach, and instead use the build-in nondeterminism. %
However, we can approximate the desired behaviour by instantiating the
monad with a list when using the Haskell library. %

\begin{spec}
data Lens s v = Lens (Maybe s -> v -> s) (s -> Maybe v)

put :: Lens s v -> Maybe s -> v -> s
put (Lens f _) = f

getM :: Lens s v -> s -> Maybe v
getM (Lens _ f) = f

get :: Lens s v -> s -> v
get (Lens _ f) s = case f s of
                        Just v   -> v
                        Nothing  -> error "get': value is `Nothing`"
\end{spec}

In order to handle the problem of partial lens definitions, which we
discussed in Section~\ref{subsec:partialLenses}, the given representation
of lenses wraps a |Maybe| data type around the view for the |get|
function. %
That is, we can actually observe if the expression |get s| succeeds or
fails. %
We could use \emph{SetFunctions}, introduced by~\cite{setFunctions}, to
identify defined and undefined values, but we adhere to the original
implementation for simplicity reasons.\footnote{SetFunctions might
  behave unexpectedly for partially applied functions.} %

\subsubsection*{Composition}
Composition is the most valuable combinator, because it serves as a
link between other primitive combinators to define more complex
lenses. %
The composition functions takes two lens functions that are
well-suited and yields a specialised combination of these lenses. %
Two lenses are well-suited for composition if the view of the first
lens and the source of the second lens have matching types. %

\begin{spec}
(<.>) :: Lens s v -> Lens v w -> Lens s w
l1 <.> l2 =  Lens putNew getNew
   where
    putNew ms@(Just s)  w  = put l1 ms (put l2 (getM l1 s) w)
    putNew Nothing      w  = put l1 Nothing (put l2 Nothing w)
    getNew s               = getM l2 (get l1 s)

\end{spec}

That is, in the get direction we can make two consecutive
applications, i.e., the composition of two get functions is just
function composition. %
Given two get functions |sub get l1 :: s -> v| and |sub get l2 :: v ->
w|, and a source of type |s|, we apply |sub get l1| to yield a view of
type |v|, and then apply |sub get l2| to the result, which yields a
value of type |w|. %
This application yields a new get function of type |sub get (l1+l2) ::
s -> w|. %
For the put direction we have to play a bit more with the available
functions and take a closer look at their type signatures. %
In addition to the get functions we discussed before, we have two put
functions, |(sub put l1) :: s -> v -> s| and |(sub put l2) :: v -> w
-> v|, a source of type |s|, an updated view of type |w|, and the
resulting put function is supposed to be of type |sub put (l1+l2) :: s
-> w -> s|. %
If there is no source available, i.e., the value of the source is
|Nothing|, we can apply the two put functions consecutively, in which
|sub put l2| is applied to the source and the given view, and |sub put
l1| is applied to the resulting value as second argument. %
In case of an available source, we have to set the inner structure
first with the given updated view, that is, we apply |sub put l2| to
the view of the given source and the updated view. %
Here, the usage of |Maybe| to wrap the result of a put function comes
in handy. %
We can easily make the distinction if a put function failed or not. %
As a last step, the source is updated with the resulting inner
structure from the previous step using |sub put l1|. %

The composition of two lenses is a powerful instrument, that is
assuming there exist primitive combinators to compose. %
We present some primitive combinators more briefly than the previous
description and emphasise examples of these combinators in action. %

\subsubsection*{Basics: Identity and Filter}
The identity combinator yields its source in the get direction and
replaces its source with the given view for the put function. %
This lens is restricted to sources and views of the same type. %

\begin{spec}
id :: Lens v v
id = Lens (\_ v' -> v') Just
\end{spec}

A similar, but maybe more feasible, combinator filters its source and
view, respectively, with regard to a specified predicate. %

\begin{spec}
phi :: (v -> Bool) -> Lens v v
phi p  = Lens get' put'
  where
   get' s    | p s        = Just s
             | otherwise  = Nothing
   put' _ v  | p v        = v
             | otherwise  = error "phi: predicate not fulfilled"
\end{spec}

In particular, the put direction declines any updated view that does
not fulfil the given predicate, that is, we demand the update on the
view to be valid. %
The get function checks if the given source fulfils the predicate and
yields that source for a positive outcome; otherwise a valid view for
the given source does not exist and the function yields |Nothing|. %

\subsubsection*{Products: Pairing and Unpairing}
The second category of combinators covers products to build pairs and
projects components of pairs. %
The first lens builds a pair in the put direction by injecting a value to the left of the
view, and projects the second component of the source in the get direction. %

\begin{spec}
addFst :: (Maybe (s1,v) -> v -> s1) -> Lens (s1,v) v
addFst f = Lens put' (Just . snd)
 where
  put' s v' = (f s v',v')
\end{spec}

The user constructs the injected value with a specified function,
which takes a possible source and the updated view to yield a new
first component. %
Recall the~\hyperref[ex:fstInc]{example of the previous section}: the
lens |fstInc| resets the first component of the source pair with an
updated view and, simultaneous, increments the second component. %
We can define this lens by means of |addSnd|, the dual lens of
|addFst| that behaves the same but injects a second component and
projects the first component, respectively. %

\begin{spec}
fstInc :: Lens (Int,Int) Int
fstInc = addSnd inc
  where
   inc Nothing _    = error "fstInc: undefined source"
   inc Just (_,sub s 2)  = (sub s 2) + 1
\end{spec}

Wait a minute! %
We criticised the use of |fstInc| as a lens, because it does not obey
the lens laws, in particular, the GetPut law. %
This observation implies that the given implementation of |addFst|
does not take any validation checks into account either. %
In the original implementation, Fisher \etal{} ensure well-behavedness
by using an auxiliary function |enforceGetPut| to resolve the
irregularity. %
As a second option, they suggest to adjust the implementation of the
get function to yield undefined for every source that does not fulfil
the GetPut law. %
For our implementation, we chose the latter solution, because
the manual correction increases the range of valid lenses, whereas the
elimination decreases the range and, thus, makes the lens less
applicable. %

The helper function |enforceGetPut| dynamically checks the behaviour
of the given lens; the get function stays untouched, but the function
applies the get function to the given source to check if the
resulting value equals the current value. %

\begin{spec}
enforceGetPut :: Lens a b -> Lens a b
enforceGetPut (Lens putL getL) = Lens put' getL
 where
  put' ms v = case ms of
    Just v' | getL v' == Just v  -> v'
    _                            -> putNew
\end{spec}

If the updated view is equal to the current view, we do not make any
further changes and yield the source; otherwise we apply the put
function as usual. %
That is, |enforeGetPut| yields the given source for an unchanged view according to the
GetPut law, hence, forces the lens to be well-behaved. %

As counterpart to |addFst| and |addSnd|, we define |remFst| and
|remSnd| to destruct the view pair by discarding the first and second
component, respectively. %

\begin{spec}
remFst :: (v -> v1) -> Lens v (v1,v)
remFst f = Lens put' (\ s -> Just (f s,s))
 where
  put' _ (v1,v)
    | f v == v1 = v
    | otherwise = error "remFst: first and second value do not match"
\end{spec}

For the definition of |remFst|, the given function creates the new
first component in the get direction, which is discarded in the
definition of the put function. %
Additionally, we have to assure that the user-specified function
applied to the second component of the source yields the same value as
the first value of the source. %
Without this correction, the given lens definition would not fulfil
the PutGet law. %

\subsubsection*{Sums: Either Left or Right}
In order to handle sum types like |Either|, we provide a lens that
distinguishes between a |Left| and a |Right| value. %
The lens |injL| injects the given update view as a left value and
ignores the source; its counterpart |injR| injects a right value. %
In the get direction, the function ignores a given left and right
value, respectively. %

\begin{spec}
injL :: Lens (Either v1 v2) v1
injL = Lens (const . Left) get'
  where
   get' (Left  v) = Just v
   get' (Right _) = Nothing

injR :: Lens (Either v1 v2) v2
injR = Lens (\_ v -> Right v) get'
 where
  get' (Left  _) = Nothing
  get' (Right v) = Just v
\end{spec}

Unlike |addFst| and |remFst|, the given lens definition and its dual
do not need any dynamic checks to ensure well-behavedness. %
These kind of lenses do not seem very feasible at first sight,
however, we will see some practical lens definitions in the next
section. %

\subsection{Usage and Examples}\label{subsec:implCombEx}
Although we gave the fundamental combinators of the library, we did not
dive into programming our own lenses so far. %
When defining lenses, the user has to build his lens by composing the
combinators of the library. %
As a first simple example, we define our running example, |fst|, by
the means of |addFst|. %

\begin{spec}
(sub fst comb) :: Lens (a,b) a
(sub fst comb) = addSnd (\ s _ -> maybe failed snd s)
\end{spec}

If there is no source available, we cannot do anything meaningful
without losing generality~--~we could yield the view instead. %
Consequently, both components of the given source pair must be of the
same type; thus, the lens just fails. %
Otherwise, we simply select the second component of the given pair and
add it to the updated view to form a pair again. %
The usage of |addSnd| indicates that we inject the value as second
component, whereas the second component is reserved for the updated
view. %
Naturally, it follows that we can define |snd| as a lens as well:
instead of |addSnd| and |snd|, we use their duals |addFst| and
|fst|. %

What about the get direction? %
We have only discussed the update strategy of the lens, i.e., the put
direction. %
First of all, let us test the behaviour of |sub fst comb|. %

\begin{spec}
> put (sub fst comb) (Just (1,"test")) 13
(13,"test")

> get (sub fst comb) (13,"test")
13
\end{spec}

The get as well as the put direction behaves as intended. %
We can observe that we do not need to take the get direction into
account when we define a new lens. %
The library encourages the user to define his lenses by means of the
put direction only. %
As we discussed in Section~\ref{bi:fisher}, it may be more
conventional and intuitive, but the put functions that we defined for
the library have a unique corresponding get function, because all put
functions comply with the requirements stated by Fisher \etal{}. \\

The library consists of several combinators that work on sums and
products; but what about built-in data types or user-defined
structures? %
We use the idea that every algebraic data type can be expressed by
sums and products. %
For example, we can take a look at the |Maybe| data type in Curry,
which is a classical representative of a sum type. %

\begin{spec}
data Maybe a = Nothing | Just a
\end{spec}

The |Maybe| data type has one constructor |Nothing| that represents a
failure value, and the |Just| constructor for valid values. %
We can easily rewrite this data type and use sum types, i.e.,
|Either|, instead. %

\begin{spec}
type Maybe a = Either () a

nothing :: Maybe a
nothing = Left ()

just :: a -> Maybe a
just = Right
\end{spec}

A failure value like |Nothing| can be represented with |Left ()|,
because |()| is the only value of the Unit type; and any other value
can be represented with |Right| instead of |Just|. %

As a second example, we discuss how to use the available combinators
to build lenses for lists. %
First of all, we need to think about a representation for lists by
means of sum or product types, for this, we recall the definition of
lists in Curry. %

\begin{spec}
data [a] = [] | a : [a]
\end{spec}

Similar to the |Maybe| data type, we have one value that stands for
itself and does not hold any value: the empty list. %
In addition, the second constructor adds a new element to the head
of a list, that is, the binary constructor can be represented as a
product, i.e., with |(,)|. %
With this general structure in mind, we can represent lists as
combination of |Either| and |(,)| as follows. %

\begin{spec}
type List a = Either () (a,[a])

empty :: List a
empty = Left ()

cons :: a -> [a] -> List a
cons x xs = Right (x,xs) 
\end{spec}

In this representation, the list |[1,2,3,4]| is rewritten as |Right
(1,[2,3,4])|, and the empty list, |[]|, corresponds to |Left ()|. %

Every algebraic data type has a set of selectors to work with. %
In the following, we define lenses equivalent to |head| and |tail| in
the get direction. %
Up to this point, we have withheld the information about another
special combinator that builds an isomorphism between two data
representations. %

\begin{spec}
isoLens :: (a -> b) -> (b -> a) -> Lens b a
isoLens f g = Lens (\_ v -> f v) (\s   -> Just (g s))
\end{spec}

The |isoLens| forms an isomorphism between two types |a| and |b|,
where |b| is the starting value, and |a| takes the role of the
internal structure. %
The functions takes two functions for transformations: from |a| to |b|
and vice versa; in the get direction, we transform the source into an
internal structure, and convert the updated structure back again in
the put direction. %
In order to provide selectors for lists, we have to define such an
isomorphism between the list data type and the rewritten structure
based on sums and products. %

\begin{spec}
inList :: Lens [a] (Either () (a,[a]))
inList = isoLens inn out
 where
  inn = either (\ () -> []) (\ (x,xs) -> x:xs)
  out xs   = case xs of
                  []   -> empty
                  y:ys -> cons y ys
\end{spec}

The transformation functions follow naturally from the definition of
|List a|, as above. %
We can eliminate the wrapping |Either| by using |injR| and |injL| that
unwrap the |Left| and |Right| constructor, respectively, and yielding
the containing value. %

\begin{spec}
cons :: Lens [a] (a, [a])
cons = inList <.> injR
\end{spec}

That is, for an exemplary list |[1,2,3,4]|, we can apply our lens
|cons| to transform the list into a pair of head element and remaining
list, and to replace the given list by a new one. %

\begin{spec}
> get' cons [1,2,3,4]
(1,[2,3,4])

> put' cons (Just [1,2,3,4]) (13,[])
[13]

> get' cons []
"get': value is `Nothing`"
\end{spec}

Unfortunately, we cannot transform the empty list into a
representation with product only for two reasons. %
Firstly, the used combinator |injR| only selects |Right| values and
the empty list is represented as |Left ()|. %
Secondly, product types are not suitable to model failure values like
the empty list. %
The usage of |injL| instead of |injR| is not feasible either: |injL|
can only select |Left| values and fails otherwise. %
However, this minor disadvantage does not affect the combinators that
we want to define. %
The functions |head| and |tail| are partial functions that only
operate on non-empty lists; we do not need to take the empty list into
consideration to define our lenses. %

The actual definition of |changeHead| and |changeTail|\footnote{The
  names conform to the functionality of their put function.} is rather
simple: the |cons| combinators split the list into head and tail,
thus, we only need to choose between the first and second component as
a last step. %

\begin{spec}
changeHead :: Lens [a] a
changeHead = cons <.> keepSnd

keepSnd :: Lens (v,s1) v
keepSnd = addSnd (\s v' -> maybe (const failed) snd s)

changeTail :: Lens [a] [a]
changeTail = cons <.> keepFst

keepFst :: Lens (v,s1) v
keepFst = addFst (\s v' -> maybe (const failed) fst s)
\end{spec}

Obviously, |changeHead| replaces the head of the list with a new
element and leaves the tail untouched with |keepSnd| and vice versa
for |changeTail|. %
In the corresponding get direction, we can access the head and tail of
the list, respectively. %
The definition of the auxiliary functions |keepSnd| and |keepFst| is
analogue to the definition of |sub fst comb| given above. %

\begin{spec}
> get' changeHead [1,2,3,4]
1

> put' changeHead [1,2,3,4] 13
[13,2,3,4]

> get' changeTail [1,2,3,4]
[2,3,4]

> put' changeTail [1,2,3,4] [13,14,15]
[1,13,14,15]
\end{spec}

Users can follow the same approach to define lenses for self-defined
data types. %
As an example, we transform a data type with one constructor and
several arguments into a sum. %
Consider the simple data type |Date| with one constructor and two
arguments corresponding to a month and a day, respectively. %

\begin{spec}
type Month = Int
type Day = Int

data Date = Date Month Day
\end{spec}

We can easily transform this data type into a pair |(Month,Day)| with the
lens

\begin{spec}
date :: Lens Date (Month,Day)
date = isoLens inn out
  where
   in (m,d) = Date m d
   out (Date m d) = (m,d)
\end{spec}

and provide selectors, |day| and |month|, to access the values in the
get direction and replace them with new values in the put direction. %

\begin{spec}
month :: Lens Date Month
month = dateLens <.> keepSnd

day :: Lens Date Day
day = dateLens <.> keepFst

> put' month (Date 12 10) 10
Date 10 10

> get' day (Date 11 18)
18
\end{spec}

We can observe from this simple example that, in addition to simple
accessors, we can add checks in order to restrict the range of valid
values. %
In the case of a data structure for dates, valid values range between
|Date 1 1| and |Date 12 31| with a lot of exceptions in between. %
We can modify the transformation functions of |date| easily to shrink
the range of valid dates. %

\begin{spec}
(sub date advance) :: Lens Date (Month,Day)
(sub date advance) = isoLens inn out
  where
   in (m,d) | check m d = Date m d
   out (Date m d) | check m d = (m,d)
   check m d  = m > 0 && m < 13 && d > 0 && d < 32
\end{spec}

Our modification still tolerates some invalide dates, e.g. |Date 2
31|, but for simplicity reasons we leave further adjustments to the
reader. %

This schema can be used for all kinds of algebraic data types; we
provide some more examples in Appendix~\ref{a:combExamples}. %
