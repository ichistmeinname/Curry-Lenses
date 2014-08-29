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
  seize the concept in Section \ref{sec:records}.}, or as type
synonym. %
For the purpose of simplicity, we define lenses with a simple type
synonym for a pair of get and put functions. %

\begin{spec}
type (sub Lens Pair) s v = (s -> v, s -> v -> s)

(sub put Pair) :: (sub Lens Pair) s v -> s -> v -> s
(sub put Pair) = snd

(sub get Pair) :: (sub Lens Pair) s v -> s -> v
(sub get Pair) = fst
\end{spec}

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
    put' (_,y) z = (x,z)
\end{spec}

The definition of this lens is straight-forward, but what we gain from
simplicity, we lose in accuracy and maintainability. %
First of all, the given definition does not automatically form a
well-behaved lens, because we did not check the lens laws
yet.\footnote{In fact, we know from Section \ref{sec:lenses} that this
  exemplary lens definition is well-behaved.} %
Secondly, if we modify one of the two functions, we have to make sure
that the other one still harmonises with our modifications. %
That is, we have to check the lens definition manually with regard to
consistency and validity. %
This circumstance is error-prone and requires a high maintenance
effort, because we do not only have to check the lens property once
after definition, but every time we modify one of the lens
components. %
As an example, let us make a slight modification to the put function
and define a lens |fstInc|, which, additionally, increments
its second component in the put direction. %

\phantomsection
\begin{spec}
fstInc :: (sub Lens Pair) (a,Int) a
fstInc = (get', put')
   where
    get' (x,_) = x
    put' (_,y) x = (x,y+1)
\end{spec}
\label{ex:fstInc}

Do the lens laws still hold? %
We have only made slight changes to the put function, which only
affect the second component. %

\begin{spec}
(sub get Pair) ((sub put Pair) fstInc (1,2) 13)
> 13

(sub put Pair) (1,2) ((sub get Pair) fstInc (1,2))
> (1,3)
\end{spec}

The first expression checks the behaviour of the PutGet law, which
seems fine, because the modification that we make with the put
function can still be retraced with the get function. %
However, the second expression behaves rather strange with respect to
the GetPut law. %
Technically, we do not change the given source at all, but the updated
source |(1,3)| differs from the original source |(1,2)|, because of
the integrated side-effect of the put function that updates the second
component as well. %

In the end, this simple approach is not satisfactory at all. %
On the one hand, it does not \emph{feel} bidirectional at all, because
we still maintain two unidirectional functions in disguise, which are combined
into one data structure. %

\subsection{Implementation}
In order to provide a more user-friendly library with less maintenance
efforts, the library needs a rich set of combinators, which are
already well-behaved. %
Then, the user builds its own lenses with the help of these
combinators without considering any laws, but only with her
implementation in mind. %

In the following, we present a reimplementation of the Haskell library
\texttt{putlenses} in Curry. %
The original implementation provides a monadic interface to instantiate
different update strategies. %
Due to the lack of type classes in Curry, we avoid this problem by
using one explicite monad instantiation -- namely, lists. %
Actually, we do not really use lists in our implementation, but the
built-in nondeterminism Curry provides. %
However, we can reproduce an equivalent behaviour of our implementation in the Haskell
library by instantiating the underlying monad to lists. %

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

In order to handle the problem of partial lens definition, which we
discussed in Section \ref{subsec:lensesLaws}, the given representation
of lenses wraps a |Maybe| data type around the view for the |get|
function. %
That is, we can actually observe if the expression |get s| succeeds or
fails. %
We could use \emph{SetFunctions} introduced by \cite{setFunctions} to
identify defined and undefined values, but we adhere to the original
implementation for simplicity reasons.\footnote{SetFunctions might
  behave unexpectedly for partially applied functions.} %

\subsubsection*{Composition}
Composition is the most valuable combinator, because it serves as a
link between other primitive combinators in order to define more
complex lenses. %
The composition functions takes two lens functions that are
well-suited and yields a specialised combination of these lenses. %

\begin{spec}
((sub (<.>) Pair)) :: (sub Lens Pair) s v -> (sub Lens Pair) v w -> (sub Lens Pair) s w
((sub (<.>) Pair)) l1 l2 =  LensPair putNew getNew
   where
    putNew ms@(Just s)  w  = put' l1 ms (put' l2 (getM l1 s) w)
    putNew Nothing      w  = put' l1 Nothing (put' l2 Nothing w)
    getNew s               = getM l2 (get' l1 s)

\end{spec}

What does well-suited mean in this context? %
It follows from the given type signature that two lenses are
well-suited for composition if the view of the first lens and the
source of the second lens have matching types. %
That is, in the get direction we can make two consecutive
applications, i.e., the composition of two get functions is just
function compostion. %
With two get functions |sub get l1 :: s -> v| and |sub get l2 :: v ->
w|, and a source of type |s|, we apply |sub get l1| to yield a view of
type |v|, and then, apply |sub get l2| to the result, which yields a
value of type |w|. %
In the end, we have a new get function of type |sub get (l1+l2) :: s
-> w|. %
For the put direction, we have to play a bit more with the available
functions and take a closer look at their type signatures. %
In addition to the get functions we discussed before, we have two put
functions, |(sub put l1) :: s -> v -> s| and |(sub put l2) :: v -> w ->
v|, a source of type |s|, an updated view of type |w|, and the
resulting put function is supposed to be of type |sub put (l1+l2) :: s
-> w -> s|. %
If there is no source available, i.e., the value of the source is
|Nothing|, we can apply the two put functions consecutively, in which
|sub put l2| is applied to the source an the given view and |sub put
l1| is applied to the resulting value as second argument. %
In the other case, we have to set the inner structure first with
the given updated view, that
is, we apply |sub put l2| to the view of the given source and the
updated view. %
As second step, the source is updated with resulting inner structure
from the previous step using |sub put l1|. %

The composition of two lenses is a powerful instrument, that is
assuming there exist primitive combinators to compose. %
Whereas we discussed the implementation of the composition in much
detail, in the following, we present some primitive combinators more
briefly and emphasise examples of these combinators in action. %

\subsubsection*{Basics: Identity and Filter}
The identity combinator yields its source in the get direction and
replaces its source with the given view for the put function. %
This lens is restricted to sources and views of the same type. %

\begin{spec}
(sub id Pair) :: Lens v v
(sub id Pair) = Lens (\_ v' -> v') (\s -> Just s)
                   -- Lens (flip const) Just
\end{spec}

A similar, but maybe more feasible, combinator filters its source and
view, respectively, with regard to a specified predicate. %

\begin{spec}
phi :: (v -> Bool) -> Lens v v
phi p  = Lens get_ put_
  where
   get_ s    | p s        = Just s
             | otherwise  = Nothing
   put_ _ v  | p v        = Just v
             | otheriwse  = error "phi: predicate not fulfilled"
\end{spec}

In particular, the put direction declines any updated view that does
not fulfil the given predicate, that is, we demand the update on the
view to be valid. %
The get function checks if the given source fulfils the predicate
and yields that source for a positive outcome; otherwise it does not
exist a valid view for the given source and the function yields
|Nothing|. %

\subsubsection*{Products: Pairing and Unpairing}
The second category of combinators covers products to build pairs and
projects components of pairs. %
The first lens builds a pair in the put direction by injecting a value to the left of the
view, and projects the second component of the source in the get direction. %

\begin{spec}
addFst :: (Maybe (s1,v) -> v -> s1) -> Lens (s1,v) v
addFst f = (Lens put_ (\ (_,v') -> Just v')
 where
  put_ s v' = (f s v',v')
\end{spec}

The user constructs the injected value with a specified function,
which takes a possible source and the updated view to yield a new
first component. %
Let us recall the~\hyperref[ex:fstInc]{example of the previous section}:
the lens |fstInc| resets the first component of the source pair
with given updated view and, simultaneous, increments the second
component. %
We can define this lens by the means of |addSnd|, the dual lens to
|addFst| that behaves the same but injects a second component and
projects the first component, respectively. %

\begin{spec}
fstInc :: Lens (Int,Int) Int
fstInc = addSnd inc
  where
   inc s _ = maybe  (error "fstInc: undefined source")
                    (\(_,s1) -> s1+1)
                    s
\end{spec}

Wait a minute! %
We critised the use of |fstInc| as a lens, because it does not obey
the lens laws, in particular, the GetPut law. %
This observation implies that the given implementation of |addFst|
does not take any validation checks into account either. %
In the original implementation, Fisher et. al ensure well-behavedness
by using an auxiliary function |enforceGetPut| to resolve the
irregularity. %
As a second option, they suggest to adjust the implementation of the
get function to yield undefined for every sources that does not fulfil
the GetPut law. %
For our implemention, we chose the latter solution as well, because
the manual correction increases the range of valid lenses, whereas the
elimination decreases the range and, thus, makes the lens less
applicable. %

The helper function |enforceGetPut| intervenes in the behaviour of a
lens; the get function stays untouched, but the function applies the
get function to the given source in order to check if the resulting
value already is the current value. %

\begin{spec}
enforceGetPut :: Lens a b -> Lens a b
enforceGetPut l = Lens put_ (getM l)
 where
  put_ ms v
   | isJust ms && getM l (fromJust ms) == Just v  = fromJust ms
   | otherwise                                    = put' l ms v
\end{spec}

If the updated view is equal to the current view, we do not make any
further changes and yield the source; otherwise we apply the put
function as usual. %
That is, |enforeGetPut| yields the give source for an unchanged view according to the
GetPut law, and, hence, forces the lens to be well-behaved. %

As counterpart to |addFst| and |addSnd|, we define |remFst| and
|remSnd| to destruct the view pair by discarding the first or second
component, respectively. %

\begin{spec}
remFst :: (v -> v1) -> Lens v (v1,v)
remFst f = Lens put_ (\ s -> Just (f s,s))
 where
  put_ _ (v1,v)
    | f v == v1 = v
    | otherwise = error "remFst: first and second value do not match"
\end{spec}

For the definition of |remFst|, the given function creates the new
first component in the get direction, which is discarded in the
definition of the put function. %
Additionally, we have to make sure that the user-specified function
applied to the second component of the source yields the same value as
the first value of the source. %
If not for this correction, the given lens definition would not fufill
the PutGet law. %

\subsubsection*{Sums: Either Left or Right}
In order to handle sum types like |Either|, we provide a lenses that
distinguish between a |Left| and |Right| value. %
The lens |injL| injects the given update view as a left value and
ignores the source; its counterpart |injR| injects a right value. %
In the get direction, the function ignores a given left and right
value, respectively. %

\begin{spec}
injL :: Lens (Either v1 v2) v1
injL = Lens (const (Left v)) get_
  where
   get_ (Left  v) = Just v
   get_ (Right _) = Nothing

injR :: Lens (Either v1 v2) v2
injR = { put := \_ v -> Right v
       , get := get_
       }
 where
  get_ (Left  _) = Nothing
  get_ (Right v) = Just v
\end{spec}

Unlike |addFst| and |remFst|, the given lens definition and its dual
do not need any dynamic checks to insure well-behavedness. %
These kind of lenses do not seem very feasible at first sight, but we
will see some practical lens definitions in the next section. %

\subsection{Usage and Examples}\label{subsec:implCombEx}
When composing lenses, the user has to think about its update strategy, i.e., the get direction of his lens. %

Running example: |fst|. %

\begin{spec}
keepFstOr :: (v -> s1) -> Lens (s1,v) v
keepFstOr f = addFst (\ s v' -> maybe (f v') fst s)

keepFst :: Lens (s1,v) v
keepFst = keepFstOr (const failed)
\end{spec}

Lenses on lists. %

\begin{spec}
isoLens :: (a -> b) -> (b -> a) -> Lens b a
isoLens f g = Lens (\_ v -> f v) (\s   -> Just (g s))

inList :: Lens [a] (Either () (a,[a]))
inList = isoLens inn out
 where
  inn eVal = either (\ () -> []) (\ (x,xs) -> x:xs) eVal
  out xs   = case xs of
                  []   -> Left ()
                  y:ys -> Right (y,ys)

cons :: Lens [a] (a, [a])
cons = inList <.> injR

unhead :: Lens [a] a
unhead = cons <.> keepSnd

untail :: Lens [a] [a]
untail = cons <.> keepFst
\end{spec}
