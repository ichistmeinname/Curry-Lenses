\section{Bidirectionalisation}\label{sec:bi}
% \begin{itemize}
% \item syntactic restraints (like variable use and nested function
%   calls) vs polymorphic restraints and no updates to shape

% \item cost / difference function, minimal cost / difference is
%   chosen
% \item syntactic derivation via view complement
%   \cite{viewComp}$\checkmark$

% \item semantic derivation using relational parametricity
%   \cite{biForFree} $\checkmark$
% \item and additional enhancements: \cite{biForFreeImprove}
%   $\checkmark$ and \cite{semRevisited} $\checkmark$
% \item and semantic approach with agda, i.e., dependent types
%   \cite{semDependentTypes}

% \item combination of both \cite{synSemComb} $\checkmark$
% \end{itemize}

Bidirectionalisation is the process of automatically transforming
uni-directional into bidirectional functions. %
In the following, we present two techniques to bidirectionalise a
uni-directional get function: the first technique by \cite{viewComp}
syntactically derives a put function for a given get function, whereas
the second approach takes a more semantic approach to generate an
appropriate put function at runtime. %
Both techniques have their advantages and disadvantages, so the
authors also worked out a combined approach, which yields results at
least as good as either one of the two techniques. %

\subsection{Syntactic Bidirectionalisation}\label{subsec:biSyn}
Matsuda \etal{} introduce a general first-order functional language
called \emph{VDL}. %
Their language has two syntactical restrictions, which we have to keep
in mind when talking about derivable functions: defined functions have
to be \emph{affine} and \emph{treeless}. %
A function definition is affine if every variable on the left hand
side is used at most once on the right hand side of the definition;
treeless characterises function definitions without intermediate data
structures. %
Nevertheless, VDL allows function definitions using arbitrary
algebraic data structures, e.g., lists and trees. %
The user defines uni-directional get functions, and VDL automatically
derives appropriate put functions. %
The presented derivation algorithm is based on a similar approach in
the field of databases, but follows a syntactical approach. %
As a first step, VDL derives a \emph{view complement function} $f^c: S
\rightarrow (S \setminus V)$ for a get\footnote{Matsuda \etal{} call
  |get| functions view functions instead.} function $f: S \rightarrow
V$. %
The main idea of the view complement function $f^c$ is to preserve any
information disregarded by the original function $f$, such that $f x$
and $f^c x$ are sufficient to reconstruct the argument $x$. %
For instance, if we consider $f : (A,B) -> A, f = fst$, $f^c : (A,B)
-> B, f^c = snd$ serves as a valid view complement function. %

Matsuda \etal{} require the view complement function $f^c$ to be
injective when paired with the view function $f$, where a pair $X$ and
$Y$is denoted with $X \triangle Y$. %
That is, $(f \triangle f^c)$ is injective for a get function $f$ and
its complement $f^c$. %
As the last step, an inverse transformation is performed on the pair
to obtain the put function. %
All in all, the put function can be derived if the paired function and
its inverse can be derived effectively. %

The following equations illustrate the definitions given above. %
\begin{align*}
  f:&~ S \rightarrow V \tag{get function}\\
  f^c:&~ S \rightarrow V' \tag{view complement function}\\
  f ~\triangle~ f^c:&~ S \rightarrow (V \times V'), \tag{tupled function}\\
  (f ~\triangle~ f^c)&~ |valS| = |(f valS,f^c valS)|
\end{align*}

\[\tag{derived put function}
  put_{<f,f^c>} (s,v) =  (f ~\triangle~ f^c)^{-1} (v,f^c s)
\]

In their paper, the authors give an algorithm to automatically derive a view
complement function for a given get function. %
% The algorithm uses so-called \emph{context notation}. %
% A context is a tree of the form $K \square_1 \cdots \square_n$, and
% such a context can be filled with expressions $e_1,\dots,e_n$, which
% is denoted as $K ~[e_1,\cdots,e_n]$ where every $\square_i$ is
% replaced by the corresponding $e_i$ for every $i \in
% \{1,\cdots,n\}$. %
% We can write any expression in treeless form as a context with
% function calls and variables. %
% In order to derive a view complement function, we analyse the rule of
% a given get function, which right-hand side is denoted as a context,
% and construct a corresponding rule for the complement function. %
% That is, for each rule $r_i$ of a get function, where $r_i$ is the
% $i$-th rule of the function definition, with the form
% \[
% f(p_1,\dots,p_n) \equiv
% K[f_1(x_{1_1},\dots,x_{1_m}),\dots,f_n(x_{n_1},\dots,x_{n_k})]
% \] we construct a rule $r_i^c$ for the complement, such that every
% context $K$ is mapped to a new constructor $B_r$, every function call
% $f_i(x_{i_1},\dots,x_{i_j})$ has a corresponding complement function
% $f^c_i(x_{i_1},\dots,x_{i_j})$ and every variable used as argument on
% left-hand side that is not used on the right-hand side is part of the
% context for the complement rule. %
% In the end, we have
% \[ f^c(p_1,\dots,p_n) \equiv
% B_r(f^c_1(x_{1_1},\dots,x_{1_m}),\dots,f^c_n(x_{n_1},\dots,x_{n_k}),y_1,\dots,y_l)
% \]
% as the derived rule for the complement, where $y_1,\dots,y_l$ are the
% missing variables on the left-hand side of $r_i$. %

% Let us take a look at our running example again; for a get function
% \[fst_{get} (x,y) = x \] we can derive a corresponding definition with
% one rule, where $B_1$ is a newly defined constructor with one argument
% $y$. %
% \[
% fst^c_{get} (x,y) = B_1(y)
% \]

% The variable $y$ is introduced on the left-hand side of $fst_{get}$,
% but missing on the right-hand side, thus, the complement function
% "remembers" the missing piece, the second component $y$, in order to
% reproduce a valid pair in the put direction. %
% As a bonus, the authors also present an improvement in order to derive
% a minimal complement function, which we do not discuss here. %

% The next step is to form a pair of the get function and its
% counterpart. %
% We can easily define this pair in an extension of VDL that allows
% tuples and local function definitions, i.e., where-clauses, but we
% have to keep the restrictions concerning treeless and affine functions
% definitions in mind. %
% Let $r_i$ be a rule of a get function $f$ and its complement $r^c_i$
% of the following form. %
% \[
% f(p_1,\dots,p_n) \equiv
% K[f_1(y_{1_1},\dots,y_{1_l}),\dots,f_n(y_{n_1},\dots,y_{n_k}),
% x_1,\dots,x_m]
% \]
% \[
% f^c(p_1,\dots,p_n) \equiv
% K'[f^c_1(y_{1_1},\dots,y_{1_l}),\dots,f^c_n(y_{n_1},\dots,y_{n_k}),
% x_1,\dots,x_m]
% \]

% The algorithm constructs a rule $r^{\triangle}_i$ with fresh
% variables $t_i,t'_i$ for $i \in \{1,\dots,n\}$ used in a where-clause
% to obey the requirement concerning treelessness.
% \todo{fix latex for alignment}
% \begin{align*}
%   f^{\triangle}(p_1,\dots,p_n&) \equiv
%   (K[t_1,\dots,t_n,x_1,\dots,x_m],K'[t'_1,\dots,t'_n,x_1,\dots, x_m])\\
%   \text{\textbf{where }}\quad\quad&\\
%   (t_1,t'_1) \equiv&~
%   f^{\triangle}_1(y_{1_1},\dots,y_{1_j})\\
%   \vdots\quad\quad&\quad\quad\quad\quad\vdots\\
%   (t_n,t'_n) \equiv&~
%   f^{\triangle}_n(y_{n_1},\dots,y_{n_j'})
% \end{align*}

% In order to gain the complete definition for $f^{\triangle} = f
% \triangle f^c$, all rule pairs for the get function and its
% complement, respectively, have to be considered. %
% For our example get function |sub fst get|, we do not need to
% introduce local function definitions, because the rules are clear of
% function calls, and just use variables. %
% \[
% fst_{get}^{\triangle}(x,y) = fst_{get} ~\triangle~ fst_{get}^c(x,y) = (x,B_1(y))
% \]

% As the last step of the complement derivation, we calculate an inverse
% for the resulting pair of the previous step, i.e.,
% $(f^{\triangle})^{-1}$. %
% Fortunately, we exactly know how a rule of $(f^{\triangle})^{-1}$
% looks like, thus, we can assume that the given rule $r^{\triangle}_i$
% has the following form. %
% \begin{align*}
%   f^{\triangle}(p_1,\dots,p_n&) \equiv (e_1,e_2)\\
%   \text{\textbf{where }}\quad\quad&\\
%   (t_1,t'_1) \equiv&~
%   f^{\triangle}_1(y_{1_1},\dots,y_{1_j})\\
%   \vdots\quad\quad&\quad\quad\quad\quad\vdots\\
%   (t_n,t'_n) \equiv&~
%   f^{\triangle}_n(y_{n_1},\dots,y_{n_j'})
% \end{align*}

% In order to inverse the given rule, we swap the left-hand and the
% right-hand side of that rule, and every function call needs to be
% inversed as well. %
% The inversion leads to the rule $(r^{\triangle}_i)^{-1}$, where we
% apply the inversion for every function $(f_i)^{-1}$
% recursively. %
% \begin{align*}
%   (f^{\triangle})^{-1}(e1,e2) \equiv (p_1,&\dots,p_n)\\
%   \text{\textbf{where }}\quad\quad \quad\quad \quad&\\
%   (y_{1_1},\dots,y_{1_j}) \equiv&~
%   (f^{\triangle}_1)^{-1} (t_1,t'_1)\\
%   \vdots\quad\quad&\quad\quad\quad\quad\vdots\\
%   (y_{n_1},\dots,y_{n_j}) \equiv&~
%   (f^{\triangle}_n)^{-1} (t_n,t'_n)\\
% \end{align*}

% When we apply their transformations to our example function
% $fst_{get}^{\triangle}$, we can finally derive the appropriate put
% function with the definition given above. %
% \begin{align*}
% (fst_{get}^{\triangle})^{-1}(x,B_1(y)) =&~ (x,y)\\
% put_{<fst_{get},fst^c_{get}>} ((x,y),v)
% =&~ (fst_{get} ~\triangle~ fst_{get}^c)^{-1} (v,fst_{get}^c (x,y))\\
% =&~ (fst_{get} ~\triangle~ fst_{get}^c)^{-1} (v,B_1(y))\\
% =&~ (v,y)
% \end{align*}

There are two details that we did not examine so far: a determinism
property for the inverse transformation and further requirements for
the complement function. %
The inverse transformation is not guaranteed to be deterministic: it
is possible to generate equations with overlapping left-hand sides. %
In the case of nondeterministic programs, a backtracking search
becomes necessary. %
However, \cite{synSemComb} state that it would be preferable to only
obtain deterministic programs. %
Furthermore, the complement, which we derive in the first step, must
be injective and minimal with respect to a collapsing order, which
needs to be defined. %
Fortunately, injectivity is decidable in VDL and the proposed
algorithm is sound and complete. %

\subsection{Semantic Bidirectionalisation}\label{subsec:biSem}
On the other hand, \cite{biForFree} introduces an approach for
semantic bidirectionalisation using free theorems to prove consistency
conditions. %
Voigtl\"ander defines a function |bff :: (forall a. [a] -> [a]) ->
(forall a. [a] -> [a] -> [a])| in Haskell, whose first argument is a
polymorphic get function and which yields the appropriate put
function. %
The acronym |bff| stands for |bidirectionalisation for free|, which is
the title of the underlying paper. %

In contrast to the syntactic approach we studied before, the resulting
put is a functional value which is semantically equivalent to a
syntactically derived put function. %
The advantage is that we have fewer language restrictions, because we
can use Haskell as our language of choice instead of a sublanguage. %
The |bff| function takes any Haskell function of appropriate type as
its argument. %
The |bff| function is defined on lists, but the approach is
also applicable for all data structures, which have shape and content,
i.e. which apply to the category of containers as defined by
\cite{containers}. %

The approach exploits the fact that the get function is polymorphic
over its first arguments, i.e. the container's element. %
We can assume that it does not depend on any concrete
element of its container, but only on positional information, which
are independent of the element's values. %
The use of free theorems allows us to inspect the effect of the |get|
transformation without knowing about the explicit implementation. %
That is, we can apply the get function to a specific list, e.g.,
integer values in ascending order, and observe the positional
information. %
In the following, we call such a specific list \emph{template}. %

We show the internal behaviour of |bff| with an example. %
As underlying get function, we use our typical example |fst| again. %
In order to fit the restrictions of |bff|, we adjust |fst| to yield
the head element as a singleton list. %
%
\begin{spec}
(sub get fst) :: [a] -> [a]
(sub get fst) (x:_) = [x]
\end{spec}%
%

In the following, we want to update the source
> eitherValues = [Left 10, Left 12, Right True, Left 13]
with the view |upd = [Right False]|. %

% \begin{spec}
% bff (sub get fst) [Left 10, Left 12, Right True, Left 13] [Right False]
% \end{spec}%
% %

% The definition of |bff| simulates its first argument, i.e. the get
% function, on an arbitrary container, like for example a list of
% |Integer| if we use a polymorphic list, |[a]|, as the container. %
% The container to simulate shares its shape property with the given
% container, which is the second argument of |bff|; in the example of
% lists, the simulation list and the given list need to be of the same
% length. %
% Every value in the simulated container has a corresponding value in
% the given container. %

The idea is to first construct a mapping for every element of the
given list: each element of |eitherValues| is mapped with the
corresponding element of the template. %

\begin{spec}
mapping :: [a] -> [(Int,a)]
mapping = zip [0..]

> mapping eitherValues
[(0,Left 10),(1,Left 12),(2,Right True), (3,Left 13)]
\end{spec}%

%
As a second step, we simulate the behaviour of the get function on the
template list: we apply get to template. %
Then, each element of the resulting list is mapped with the
corresponding elements of the updated view |upd|. %

\begin{spec}
mapping2 :: ([a] -> [a]) -> [Int] -> [a] -> [(Int,a)]
mapping2 getF is = zip (getF is) 

> mapping2 ((sub get fst) [0,1,2,3]) [Right False]
-- zip' [0] [Right False]
[(0,Right False)]

\end{spec}

We combine both mappings with precedences to the second: if we find a
value in both mappings, we choose the one from the view. %
That is, the auxiliary function |union'| combines the two lists
accordingly. % 

\begin{spec}
mapping3 :: [(Int,a)] -> [(Int,a)] -> [(Int,a)] 
mapping3 (sub m 1) (sub m 2) = union' (sub m 1) (sub m 2)

> mapping3  [(0,Left 10),(1,Left 12),(2,Right True), (3,Left 13)]
            [(0,Right False)]
[(0,Right False),(1, Left 12), (2, Right True), (3, Left 13)]
\end{spec}

As last step, every element in the template is
replaced by its associated value according to the combined mapping. %
With all the ground prepared, we can define the |bff| function given
in the paper. %

\begin{spec}
bff :: (forall a. [a] -> [a]) -> (forall a. [a] -> [a] -> [a])
bff (sub get f) s v = map (fromJust . flip lookup (mapping3 (sub m 1) (sub m 2))) (sub m 1)
  where
   (sub m 1) = mapping s
   (sub m 2) = (sub mapping 2) (sub get f) (map snd (sub m 1)) v

> bff (sub get fst) eitherValues upd
[Right False, Left 12, Right True, Left 13]
\end{spec}

Voigtl\"ander also defines two additional functions, |sub bff EQ| and
|sub bff ORD|, which use the functions of the type classes |Eq| and
|Ord|, respectively. %
In order to apply his approach for a |get| function that duplicates
elements, the defined mapping fails because of its simple
definition. %
In a more practical mapping, equivalent elements in the original list
need to map to the same element in the template. %
Thus, we need to compare the elements within the list, this is where
the |Eq| type class comes into play. %
For the function |sub bff Ord|, the mapping needs a similar, but
rather complicated and more technical, adjustment in order to allow
the use of free theorems again. %

As the major disadvantage, any get function that changes the shape of
its elements fails due to non-trackable updates. %
That is, the semantic approach is limited to get functions that
preserve the shape of the given list. %

% \subsubsection[Generalisation to Higher-Order Functions]{Generalisation for the Three Functions |bff|, |sub bff EQ| and |sub bff ORD|}

As an enhancement for semantic bidirectionalisation,
\citet{semRevisited} presented a generalisation that extends the range
of |get| function to higher-order functions that are not expressed by
type classes, or depend on different type classes than |Eq| and
|Ord|. %
Instead of three single functions, like in Voigtl\"ander's work, Wang
and Najd define a function
that takes an observer function that gives rise to
equivalence properties of the elements, and an
observer function for the template. %
The approach uses these observer functions to
build the mappings as in the original approach. %
These mappings are called observation tables here, and generalise the
explicit usage of different functions for different type class
dependencies. %

% As a first modification to the original approach, instead of mapping
% between indices and elements of a list, the map just holds two copies
% of each element. %
% That is, the original and its copy form a pair, where the first
% component stays constant and the second one is used for updates. %

% \begin{spec}
% (sub mapping by) :: [a] -> ([a],[a])
% (sub mapping by) m = zip m m
% \end{spec}

% This approach has the advantage, that we can use the equivalence
% function for both components of the pair and do not need to adjust the
% function to work on |Integer|, which are introduced for the mapping in
% the original approach.\footnote{This modification is used for
%   functions with an |Eq| or |Ord| type class context, i.e., for cases
%   where we would use |sub bff EQ| and |sub bff ORD|, respectively, in
%   the original approach. For all other cases, i.e., when we use |bff|,
%   the mapping is build as in the orignal approach.} %

% Similar to before, the next step simulates the get function on the
% source list, and creates a new mapping with the resulting view and the updated view. %

% %format eqA = "eq_{a}"
% %format eqI = "eq_{i}"
% %format unionBy' = "union_{by}"
% \begin{spec}
% sub mapping2 by :: (a -> a -> Bool) -> (i -> i -> Bool) -> [i] -> [a] -> [(i,a)]
% (sub mapping2 by) eqA eqI is as
%    |  length is == length as =
%        if and [not (i `eqI` j) or (x `eqA'` y) | (i,x) <- zs, (j,y) <- zs]
%          then zs
%          else error "Inconsistent update!"
%   where
%    zs = zip is as
% \end{spec}

% The first and second argument are equivalence functions; the first
% function compares the elements of the actual source list and the other
% function is used for comparisons on the keys of the given mapping. %
% In the case of the used mapping function |sub mapping by|, we do not
% need to distinguish between these functions. %
% The important step for the new approach is to check the resulting map
% in regard to the so-called \emph{Map Invariant}. %
% This invariant says that a valid map `zs :: [(i,x)]` must satisfy the
% following property for two observer functions |f :: sup x n -> Bool|
% and |g :: sup i n -> Bool|. %

% \begin{align*}
% \forall (i_1, x_1) \dots (i_n,x_n) \in zs. f x_1 \dots x_n \equiv g i_1 \dots i_n \tag{Map Invariant}
% \end{align*}

% The invariant requires all pairs of the given map to be equal in
% regard to the two observer function. %
% In particular, depending on the arity of the observer functions, all
% combinations of pairs in the map have to fulfil this property. %
% In order to check the invariant property for our map, we define a
% function to apply two given observer functions according to the
% definition above. %

% \begin{spec}
% checkInv :: (sup a n -> Bool) -> (sup i n -> Bool) -> [(i,a)] -> [(i,a)]
% checkInv f g zs  | all check (sub comb n) = zs
%                  | otherwise              = error "Invariant broken!"
%  where
%   (sub comb n) = nub (map (take n) (permutations zs))
%   check ys =  let (is,as) = unzip ys
%               in (sub uncurry n) f ((sub tuple n) as) == (sub uncurry n) g ((sub tuple n) is)
% \end{spec}

% Here, |sub comb n| is a list of all combinations of pairs in the given
% map, |sub uncurry n| is a function to uncurry a |n|-th tuple, and |sub
% tuple n| is the corresponding function to convert a list into an
% |n|-th tuple; we omit the definitions for brevity. %
% Furthermore, we combine both mappings with precedences to the second
% one, like in the original approach. %
% We combine two pairs of the given maps if the used keys, i.e., the
% first components, are equal with regard to the given equivalence
% function. %
% For the overall definition, we have to keep in mind to check the map
% invariant for the result as well.

% \begin{spec}
% sub mapping3 by :: (i -> i -> Bool) -> [(i,a)] -> [(i,a)] -> [(i,a)]
% (sub mapping3 by) eqI (sub m 1) (sub m 2) = unionBy' eqI (sub m 1) (sub m 2)

% unionBy' :: (i -> i -> Bool) -> [(i,a)] -> [(i,a)] -> [(i,a)]
% unionBy' _    xs            []  = xs
% unionBy' _    []            ys  = ys
% unionBy' eqI  (x@(i,_):xs)  ys
%     | not (all ((i `eqI`) . fst) ys)  = x : unionBy' eqI xs ys
%     | otherwise               = unionBy' eqI xs ys
% \end{spec}

% There are still some missing parts that we need to discuss, like the
% locally defined equality functions |eqA|, |eqI|. %
% Najd and Weng present three different equality functions, where each
% function has its own pros and cons. %
% They distinguish between observable, structural, and physical
% equivalence and we refer to their paper for further study. %
% As mentioned before, in our example we do not distinguish between keys
% and values of the given map, that is, we only need to define the
% equality function for the values of the source and use it for the keys
% as well. %

% Additionally, we need to adapt the |lookup| function that was used in
% the original approach to select the final values for the updated
% source. %
% We lookup the values with regard to the given equality function for
% the keys of the map. %

% \begin{spec}
% lookupBy :: (i -> i -> Bool) -> [(i,a)] -> i -> Maybe a
% lookupBy _ []         _  = Nothing
% lookupBy p ((j,x):xs) i
%   | p i j      = Just x
%   | otherwise  = lookupBy p xs i
% \end{spec}

% In the end, we can derive a corresponding put function with the
% following definition. %

% \begin{spec}
% (sub bff by) getBy f g s v = map (lookupBy eqI (sub m 3)) is
%    where
%     is         = map fst (sub m 1)
%     (sub m 1)  = mapping1 s
%     (sub m 2)  = checkInv f g (mapping2 eqA eqI (getBy f is) v)
%     (sub m 3)  = checkInv f g (mapping3 eqI (sub m 1) (sub m 2))
%     eqA        = undefined
%     eqI        = eqA
% \end{spec}

% In order to round up this approach, we examine the usage of the
% |bffBy| function for physical equivalence, i.e., |eqA (i,x) (j,y) = i == x && (j,y)|. %

% %format lookupBy = "lookup_{by}"
% \begin{spec}
% bffBy  filter
%        ((== "fst") . fst)
%        ((== "fst") . fst)
%        [("snd",17),("fst",13)]
%        [("fst",42)]
%   = map (lookupBy ((== "fst") . fst) m3 [("snd",17),("fst",13)]
%      where m3 =  [  (("snd",17),("snd",17))
%                  ,  (("fst",13),("fst",42)) ]
%   = [("snd",17),("fst",42)]
% \end{spec}

% \subsubsection{Adaption for Monomorphic Functions}

As a second enhancement, \cite{biForFreeImprove} introduce a type
class to extend the range of |get| functions to monomorphic
transformations. %
The main idea is to provide a type class |PackM delta alpha mu| to
convert monomorphic functions into polymorphic ones.% \footnote{The
  % following code needs several langue extension to run accordingly:
  % Rank2Types, MultiParamTypeClasses, FunctionalDependencies,
  % FlexibleInstances, ExistentialQuantification, FlexibleContexts.} %

\begin{spec}
class (Pack delta alpha, Monad mu) => PackM delta alpha mu where
  liftO :: Eq beta => ([delta] -> beta) -> [alpha] -> mu beta
class Pack delta alpha | alpha -> delta where
  new :: delta -> alpha
\end{spec}

The type variable |delta| represents the type of the concrete data
structure, whereas |alpha| is the type of the abstracted value. %
The last type variable |mu| is the used monad, which tracks the
transformation on values of the concrete structure called; these
tracking data are called observation histories. %
The additional type class |Pack delta alpha| constructs labels to
track information regarding the location of values within the concrete
structure. %
In short, the approach replaces monomorphic values in the definition
of the |get| function, which are, for example, used for comparisons,
with polymorphic values. %
% The following function is quite similar to Example
% \ref{filter:fstGet}, but it is defined on lists instead of trees; it
% selects the first element with a label named |"fst"| from the given
% list. %

% \begin{spec}
% (sub get fst) :: [String] -> [String]
% (sub get fst) []     = []
% (sub get fst) (v:vs)
%   | v == "fst"  = [v]
%   | otherwise   = (sub get fst) vs
% \end{spec}

% In order to execute such a function in its get direction, the authors
% define a special function with |PackM| context. %
% The following definition works on lists but any other polymorphic data
% type is possible as well. %

% \begin{spec}
% instance Pack delta (Identity delta) where
%   new = Identity

% instance PackM delta (Identity delta) Identity where
%   liftO p x     = Identity (p (map runIdentity x))

% get  :: (forall alpha. forall mu . PackM delta alpha mu
%      => [alpha] -> mu [alpha])
%      -> [delta]
%      -> [delta]
% get h s =  let Identity v = h (fmap Identity s)
%            in fmap runIdentity v
% \end{spec}

% In the get direction, we do not want to track any information about
% the mapping of abstract and concrete values, thus, the underlying
% monad is instantiated to the Identity monad\footnote{See Section
%   \ref{code:IdentityMonad} for the definition of the Identity monad.}. %
% Our example get function needs to be rewritten in order to obey the
% |PackM| context. %
% That is, the check needs to be lifted into the |PackM| type class and
% because of the underlying monad, we need to wrap our result into a
% monadic value as well. %

% \begin{spec}
% (sub get fst) :: forall alpha mu beta . PackM (String,beta) alpha mu => [alpha] -> mu [alpha]
% (sub get fst) [] = return []
% (sub get fst) (val:vs) = do   
%    b <- liftO2 (\x y -> fst x == fst y) val (new ("fst",undefined))
%    if b  then return [val]
%          else getFst vs

% liftO2 p x y  = liftIO (\ [x,y] -> p x y) [x,y]
% \end{spec}

% For the rewritten version of the get function, we can apply it to
% an example list |[("fst",13),("snd",21)]|. %
% In order to get a better insight of the ongoing operation, we evaluate
% the following expression with more intermediate steps. %

% \begin{spec}
% sub get example  = get (sub get fst) [("fst",13),("snd",21)]
%                  = fmap   runIdentity 
%                           (runIdentity ((sub get fst) (fmap  Identity
%                                                              [("fst",13), ("snd",21)])))
%                  = fmap runIdentity (runIdentity (Identity [Identity ("fst",13)]))
%                  = [("fst",13)]
% \end{spec}

% For the put direction, the approach constructs polymorphic values from
% the original monomorphic values, and does not instantiate type
% variables when used in comparisons in order to fulfil the
% requirements to use free theorems. %
% Like in the original approach, we first construct a mapping to track
% information about the location of the source values when applying the
% get function. %

% \begin{spec}
% data Loc alpha = Loc { body :: alpha, location :: Maybe Int }

% assignLocs :: [delta] -> [Loc delta]
% assignLocs xs = zipWith (\ x i -> Loc x (Just i)) xs [0..]

% instance Pack delta (Loc delta) where
%   new x = Loc x Nothing
% \end{spec}

% Here, the location is stored in a self-defined data structure, where
% every element of the list is mapped to its index. %
% Additionally, if a put function inserts a new value during the update,
% there is no location information for this value in the source;
% therefore, the author modelled the assigned location to be optional
% within the view structure, i.e. |Maybe Int|. %

% \begin{spec}
% sub assign example  = assignLocs [("fst",17),("snd",21)]
%                     = [Loc ("fst",17) (Just 0), Loc ("snd",21) (Loc 1)]
% \end{spec}

% Additionally, the authors use a writer monad to actually track the
% observation history. %
% In order to lift a function to be an observer function, the writer
% monad keeps track of the function and its arguments, which are
% unwrapped from the |Loc| data type first. %
% The relevant observation history is also modelled as a data structure;
% the structure depends on an observation function, a list of arguments
% and and the resulting value for the application of the function to its
% arguments. %
% That way, each result can be checked, when the update is executed in a
% later step, if an updated value does not confirm to the observation
% history, the update fails. %


% \begin{spec}
% data Result alpha = forall beta . Eq beta => Result ([alpha] -> beta) [alpha] beta

% data Writer eta beta = Writer { runWriter :: (beta, [Result eta]) }

% instance Monad (Writer alpha) where
%   return x = Writer (x, [])
%   Writer (x,sub res 1) >>= f =
%     let Writer (y,sub res 2) = f x
%     in Writer (y,sub res 1 ++ sub res 2)

% instance PackM delta (Loc delta) (Writer (Loc delta)) where
%   liftO p x = Writer (p' x, [Result p' x (p' x)])
%     where p' = p . map body
% \end{spec}

% As a next step, we apply the get function to our dummy list with the
% locations associated to the original source elements. %
% This time, the underlying monad is not the Identity, but the Writer
% monad we just defined, thus, we can track all relevant information
% regarding the observation function. %
% In our example, we only track the the first element because the
% remaining elements are not even touched, hence, we can be sure that
% they do not change during the update. %

% \begin{spec}
% Writer (sub upd example,sub history example)  =
%    (sub get fst) (sub assign example)
% =  (sub get fst)  [ Loc ("fst",17) (Just 0)
%                   , Loc ("snd",21) (Loc 1)]
% = Writer   ([Loc ("fst",17) (Just 0)]
%            ,[Result  (\ [x,y] -> x == y)
%                      [Loc "fst" Nothing]
%                      True])
% \end{spec}%

% We use the generated list of location information for the source and
% the updated view, i.e. |sub upd example|, as a new mapping. %
% The implementation requires both lists to be of the same size,
% otherwise the function fails because of a shape mismatch. %
% If the two lists have the same size, we create a mapping between the
% elements of the source list and its index in corresponding location
% information. %
% The mapping needs to be consistent, if the same element occurs
% repeatedly, each occurrence needs to map to the same location as
% before; otherwise the construction fails because of inconsistency. %

% \begin{spec}
% matchViews :: Eq delta => [Loc delta] -> [delta] -> [(Int,delta)]
% matchViews locVs vs
%   | length locVs vs  = makeUpd (zip locVs vs)
%   | otherwise        = error "Shape mismatch"

% makeUpd :: Eq delta => [(Loc delta, delta)] -> [(Int,delta)]
% makeUpd = foldr f []
%  where
%   f (Loc x (Just i), y) u =
%      maybe  ((i,y) : u)
%             (\ y' -> if y == y' then u else error "Inconsistent Update")
%             (lookup i u)
%   f (Loc x Nothing, y) u  | x == y     = u
%                           | otherwise  = error "Update of Constant"
% \end{spec}

% In our example, the element of the updated view is matched with its
% previous occurrences in the source list, yielding the following new
% mapping. %

% \begin{spec}
% sub matchViews example  =  let Writer (vs,res) = (sub writer example)
%                            in matchViews vs [("fst",42)]
%                         = makeUpd (zip [Loc "fst" (Just 0)] [("fst",42)])
%                         = [(0,("fst",42))]
% \end{spec}

% With the updated mapping and the observation history in mind, we
% actually want to execute the update. %
% Similar to the original approach, we have a mapping between the
% original list and their index position in that list, |sub assign
% example|. %
% In addition, we have the history with the location information for
% some elements. %
% Thus, we lookup the given position in the mapping, and change the
% element for the corresponding location information if we do find a
% match; otherwise the information remains unchanged. %

% \begin{spec}
% update :: [(Int,delta)] -> Loc delta -> Loc delta
% update upd (Loc x Nothing)   = Loc x Nothing
% update upd (Loc x (Just i))  = maybe  (Loc x (Just i))
%                                       (flip Loc (Just i))
%                                       (lookup i upd)
% \end{spec}

% This update function is important to check the observation history for
% consistency, and to run the actual modification on the source. %
% In the final version of the appropriate put function, we use the
% previous defined functions, and finally, update the given source list,
% if the history check succeeds. %

% \begin{spec}
% put :: Eq delta => (forall alpha. forall mu. PackM delta alpha mu => [alpha] -> mu [alpha]) -> [delta] -> [delta] -> [delta]
% put h s v
%    | checkHist (update upd) hist  = fmap (body . update upd) locs
%    | otherwise                    = error "Inconsistent History"
%   where
%    locs                = assignLocs s
%    Writer (res, hist)  = h locs
%    upd                 = matchViews res v
% \end{spec}

% The history check first applies the update to the underlying value
% and then runs the observation function for the possibly changed value
% and its arguments. %
% The resulting value must be the same
% as the one stored in the history. %

% \begin{spec}
% checkHist :: (Loc delta -> Loc delta) -> [Loc delta] -> Bool
% checkHist updF = all (\ (Result p xs r) -> p (map updF xs) == r)
% \end{spec}

% At the end, we can apply this polymorphic put function to our
% monomorphic get function to update a source for a given view. %
% In order to round up the ongoing example, we update the source list
% |[("fst",17),("snd",21)]| with the modified view |[("fst", 42)]|. %

% \begin{spec}
% (sub put fst)  = put (sub get fst) [("fst",17),("snd",21)] [("fst",42)]
%                = if   checkHist  (update (sub upd example))
%                                  (sub history example)
%                       then fmap (body . update (sub upd example)) (sub assign example)
%                       else error "Inconsistent History"
%                = fmap  (body . update [(1,("fst",42))])
%                        [ Loc ("fst",17) (Just 0)
%                        , Loc ("snd",21) (Just 1) ]
%                = [("fst",42),("snd",21)]
% \end{spec}%

\subsection[Combined Approach]{Combining Semantic and Syntactic Bidirectionalisation}

It becomes apparent that both approaches have their pros and cons. %
Naturally, \cite{synSemComb} proposed a combination of the semantic as
well as the syntactic bidirectionalisation. %
The combined approach uses each technique for their area of expertise:
the semantic derivation for content updates and the syntactic
derivation for shape-changing transformations. %
The authors categorise the two techniques as follows: syntactic
bidirectionalisation is used as black box whereas semantic
bidirectionalisation is similar to a glass box. %
That is, the semantic bidirectionalisation approach can be more
powerful if we refactor the transformation in order to plug-in a
syntactic technique; then, shape-changing transformations can be
handled. %
The presented combination is general enough to allow any syntactic
approach to be plugged-in. %
This generality is also the motivation to call it black-box: we do
not know anything about the actual derivation, but that it yields the
wanted results. %
The overall idea and implementation to plug-in a syntactical
bidirectionalisation is discussed by \cite{enhanceSem} in more
detail. %

% One additional improvement implemented in the combined approach is a
% |Nat| data structure as alternative for built-in integers. %

% \begin{spec}
% data Nat = Z | S Nat

% toNat :: Int -> Nat
% toNat 0          = Z
% toNat n | n > 0  = S (toNat (n-1))

% fromNat :: Nat -> Int
% fromNat Z      = 0
% fromNat (S n)  = 1 + fromNat n
% \end{spec}

% The attentive reader might notice, that the given definition coincides
% with the representation of Peano numbers. %
% In the following, we use the |Nat| data type and its conversion
% functions to redefine our |get| and |put| functions accordingly. %
% For instance, we can convert every get function |get :: [alpha] ->
% [alpha]| to a function |(sub get syn)|\footnote{In the paper, the
%   authors also give a simplified syntactic definition, which can be
%   derived directly for a given |get| function.}. %

% \begin{spec}
% (sub get syn) :: Nat -> Nat
% (sub get syn) n = toNat (length (get [0..fromNat n-1]))
% \end{spec}

% Next, we derive this new gained get function with the appropriate
% black-box to get a corresponding put function. %
% We could use, for example, the syntactic bidirectionalisation by
% \cite{viewComp} that we introduced in Section \ref{subsec:biSyn}. %
% Let us assume, that the resulting put function has the form |put ::
% Nat -> Nat - Maybe Nat|\footnote{The authors wrap their value in a
%   |Maybe| constructor in order to handle failures as well, i.e., if
%   the application of the put function does not yield a valid result. A
%   simplified version |put :: Nat -> Nat -> Maybe Nat| is possible as
%   well.}. %

% In the combined approach, we integrate the syntactically derived put function to
% explicitly handle shape-changing updates, and use the result to
% produce an additional mapping. %
% The final definition of |(sub put comb)|, the put function for the
% combined approach, adopts its additional mappings from the original
% semantic approach. %
% That is, we first map each element of the given list with its
% index, and run the derived put function |sub put syn| on the
% given shapes of the source and the updated view. %
% In the case of list, we use the
% length of the given lists as shapes. %
% We use the resulting list of integer values after the shape update
% as template. %
% Then, we construct an additional mapping between the template and the
% actual updated view. %
% As usual, we combine the mapping of the original source to its indices
% and the map with the simulated values with precedence to the latter
% one. %
% In the end, we lookup each index in the template and select the
% associated value, which finally leads to the updated source. %

% \begin{spec}
% (sub put comb) :: [alpha] -> [alpha] -> [Maybe alpha]
% (sub put comb) s v = map (flip lookup (sub m 5)) (sub m 3)
%    where
%     (sub m 1)  = zip [0..] s
%     (sub m 2)  = maybe  (error "Could not handle shape change.")
%                         id
%                         ((sub put syn) (toNat (length s)) (toNat (length v)))
%     (sub m 3)  = [0..fromNat m2-1]
%     (sub m 4)  = zip (get m3) v
%     (sub m 5)  = union' (sub m 4) (sub m 1)
% \end{spec}

As a minor drawback, the range of |get| definitions covered by the
combined approach is limited by two factors: only affine and treeless
functions are allowed because of the usage of the syntactic
bidirectionalisation, and we can only use polymorphic functions in
order to use the semantic bidirectionalisation technique. %
Fortunately, the presented enhancements and extensions to semantic
bidirectionalisation does consort well with the combined approach. %
That is, we can use the more general function |bffBy| in combination
with specified observer functions for semantic bidirectionalisation,
or turn monomorphic functions into polymorphic ones with the monadic
extension to gain a wider range of possible |get| functions. %
In the end, the combined approach performs never worse than one of the
two approaches by themselves. %
\todo{rephrase - covered}
The semantic bidirectionalisation on its own has difficulties in
shape-changing update, but are covered with the combined approach,
whereas the syntactic approach operates on specialised programs now,
which can lead to better results. %

In addition, the semantic bidirectionalisation uses free theorems also
to prove consistency conditions. %
We discussed the syntactical bidirectionalisation, which formulates
its derivation on the ground of the \emph{GetPut} and \emph{PutGet}
law, in contrast, Voigtl\"ander proves, with the help of free
theorems, for each of his function definitions, |bff|, |sub bff EQ|
and |sub bff ORD|, that they obey the lens laws. %
That is, instead of a correctness-by-construction approach, the laws
are verified by hand.
