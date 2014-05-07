# Papers

## Monadic Combinators for _Putback_ Style Bidirectional Programming

* get functions in general not injective: many possible corresponding
      put functions exist to form a well-behaved lens
* _PUTINJ_: `put s` is injective for any source `s`, i.e., $s' \in put
  s v \wedge s' \in put s v' \Rightarrow v = v'$
* _PUTTWICE_: $s' \in put s v \Rightarrow s' = put s' v$


## Validity Checking of _Putback_ Transformation in Bidirectional Programming

* requires putback functions to be affine and in treeless form, that
  is, each view variable is used at most once and no
  intermediate data structures are used in definitions
* this class of functions has similarities to tree transducers
* assumes only total functions
* hybrid compositional approach, but focus on designing language to
  specify various primitive putback functions over algebraic data structures
* validity of putback transformations - A put is valid, if there
  exists a get such that GETPUT and PUTGET are satisfied
* Uniqueness of get - Given a put function, there exists at most one
  get function that forms a well-behaved BX
* _PUTDETERMINATION_: $\forall s,s',v,v' . put s v = put s' v'
  \rightarrow v = v'$
* _PUTSTABILITY_: $\forall s . \exists v . put s v = s$
* validity - A put function is valid if and only if it satisfies the
  _PUTDETERMINATION_ and _PUTSTABILITY_ properties

# Implementations

## "Combinator-Lenses"

## "Put-Lenses"

* generating corresponding `get` for a defined `put`-function
* examples of "Bidirectionalization for free" do not terminate (see
  [Problems with internal structures](#structures)

# Practical Examples

## Spicey

* labeling bug: if a corresponding label is missing, a
  non-determinism error occurs
* high-potential library to include lenses for getter, setter and projection
* reads entities from database on page load - why?

## WUI

* `readQTerm (showQTerm ())` fails with parse error
* difference between `wJoinTuple` and `wPair`?

## Formlenses

* `lmap :: Lens a b -> f b -> f a`, where `f` is a `FormLens`
* use `WUILenses` instead of `WUI` in blog example of Spicey

# Problems with internal structures {#structures}

We have the following interface for put-based lenses that generate a
corresponding `get`-function for a defined lens. 

~~~~{.haskell .numberLines}
data Lens a b = a -> b -> a
put :: Lens a b ->  a -> b -> a
put lens s v = lens s v

get :: Lens a b -> a -> b
get lens s | put s v == s = v
 where v free
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Our aim is to generate a `get`-Function for `putHalve`:

~~~~ {.haskell .numberLines}
putHalve :: [a] -> [a] -> [a]
putHalve xs xs' | length xs' == n = xs' ++ drop n xs
 where
  n  = length xs `div` 2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`putHalve` takes to lists, a source list and a view list, and
concatenates the second list with the second half of the first
list. Valid view lists have half the length of the source list, if
otherwise, the function yields `failed`, i.e., no result is produced.
(BeginHint: the wished `get` function is equivalent to

```{.haskell}
halve :: [a] -> [a]
halve xs = take (length xs `div` s) xs
```
EndHint)

With help of our interface, we can derive a corresponding`get` function by simply
calling `get` with `putHalve` and our source value.

``` {.haskell}
getHalve = get putHalve
```

Unfortunately, a function call like `getHalve [(),()]` does not terminate. 
The problem is the combination of the internal list and
and numbers data structures, they do not harmonize well. This effect
is triggered by the usage of `length`. The free variable `v`
corresponds to `xs'` in the definition of `putHalve`,i.e., the system
guesses values for `xs'`. To be more precise, the system needs to
guess lists of type `()` for `xs'` and check, if their length is the
same as `div length [(),()]  2`. For the specific example above, we
can simplify our definition of `putHalve` in order to focus on the
problematic part.

```{.haskell}
putHalveSimple :: [a] -> [a]
putHalveSimple xs' | length xs' == 1 = xs' ++ drop 1 [(),()]
```

If we now consider the internal structure for `Int` values, we end up
with the following function

```{.haskell}
putHalveSimple :: [a] -> [a]
putHalveSimple xs' | length xs' == Pos IHi = xs' ++ drop 1 [(),()]
```
with respect to the internal data structure `BinInt`.

```{.haskell}
data BinInt = Neg Nat | Zero | Pos Nat
data Nat = IHi | O Nat | I Nat
```

## Guessing lists with a specific length

So, how does the evaluation steps look like, when we want to compute
the length of a list?

```{.haskell .numberLines}
length :: [a] -> Bin Int
length []     = Zero
length x:xs = inc (length xs)

inc :: BinInt -> BinInt
inc Zero        = Pos IHi
inc (Pos n)     = Pos (succ n)
inc (Neg IHi)   = Zero
inc (Neg (O n)) = Neg (pred (O n))
inc (Neg (I n)) = Neg (O n)
```

That is, for an empty list, we can directly compute the result, but
for a non-empty list we build a sequence of `inc`-operations,
e.g. `inc (inc Zero)` for a two-valued list, `inc (inc (inc Zero))`
for a three-valued list etc. In order to take this investigation
one step ahead, we need to look at the definition of `inc`, where only
lines 6 and 7 are of further interest.

(1) `Zero == Pos IHi`  
    $\rightarrow$ `False`

(2) `inc (Zero) == Pos IHi`  
    $\rightarrow$ `Pos IHi == Pos IHi`  
    $\rightarrow$ `True`

(3) `inc (inc Zero)) == Pos IHi`  
    $\rightarrow$ `inc (Pos IHi) == Pos IHi`  
    $\rightarrow$ `Pos (succ IHi) == Pos IHi`  
    $\rightarrow$ `Pos (O IHi) == Pos IHi` $\rightarrow$ `False`

(4) `inc (inc (inc Zero))) == Pos IHi`  
    $\rightarrow$ `inc (inc (Pos IHi)) == Pos IHi`  
    $\rightarrow$ `inc (Pos (succ IHi)) == Pos IHi`  
    $\rightarrow$ `Pos (succ (succ IHi)) == Pos IHi`  
    $\rightarrow$ `Pos (succ (O IHi)) == Pos IHi`  
    $\rightarrow$ `Pos (I IHi) == Pos IHi`  
    $\rightarrow$ `False`

The attentive reader may have already noticed that the definition of
`inc` is strict in its first argument and does not propagate its constructor, the successor
function on binary numbers is also strict. Unfortunately, the `length`
function cannot be implemented in a way that is sufficient to
propagate a constructor, because, as we will see later, it is
problematic to map the empty list to a value of type `Nat`.
Therefore, the whole list has to be evaluated in order to determine its length, which leads to
non-evaluation when guessing a list with a specific length. The
built-in search for free variables in KICS2 can be translated in
nested injections of `?`-operations, where every constructor of the
given type is a possible guess and arguments of constructors are also
free variables. For our example, we can illustrate the
built-in search as follows.

`length v == Pos IHi where v free`
$\rightarrow$ `length ([] ? _x2:xs) == Pos IHi where _x2,xs free`
$\rightarrow$ `(length [] ? length _x2:xs) == Pos IHi where _x2, xs free`  
$\rightarrow$ `length [] == Pos IHi ? length _x2:xs == Pos IHi where
_x2,xs free`  
$\rightarrow$ `Zero == Pos IHi ? length _x2:xs == Pos IHi where _x2,xs
free`  
$\rightarrow$ `False ? length _x2:xs == Pos IHi where _x2,xs free`  
$\rightarrow$ `False ? inc (length xs) == PosIHi where xs free`  
$\rightarrow$ `False ? inc (length [] ? length _x4:ys) == PosIHi where
_x4,ys free`  
$\rightarrow$ `False ? inc length [] == Pos IHi  ? inc (length _x4:ys) == PosIHi where _x4,ys free`  
`...`

```
{v = []} False
{v = [_x2]} True
{v = [_x2,_x4]} False
...
```

The given type is `[a]` with two constructors `[]` for an empty list
and `(:) x xs` for non-empty lists with an element `x` and the
remaining list `xs`. In our example, for every free variable of type
`[a]` both constructors are possible values, therefore, both
expressions are introduced with the `?`-operator. The `?`-operator
explicitly inserts non-determinism and is defined as follows.

```{.haskell}
(?) :: a -> a -> a
x ? y = x
x ? y = y
```

In the end, the built-in search collects all possible values and
works henceforth with a set of values, i.e., every list of the resulting
set is used for further function calls.

As we said in the beginning, the internal structure for lists and
numbers do not harmonise well - how can we solve the problem that
`putHalve` does not terminate with the current implementation? We will
present two different approaches.

### Peano numbers
At first, we use another data structure for numbers that has an unary representation, i.e. we will
use peano numbers.

```{.haskell}
data Peano = Zero | S Peano

lengthPeano :: [a] -> Peano
-- lengthPeano = foldr (const S) Z
lengthPeano []     = Z
lengthPeano (x:xs) = S (lengthPeano xs)
```
<!-- dropPeano :: Peano -> [a] -> [a] -->
<!-- dropPeano Z      xs      = xs -->
<!-- dropPeano (S n) (x:xs) = drop n xs -->
<!-- dropPeano (S n) []        = [] -->

Peano numbers are represented with a constructor for `Zero` and a
successor constructor `S Peano`. The corresponding `length` function
introduces an `S`-constructor for every element of the list and yields
`Zero` for an empty list. <!-- The implementation for `drop` is straightforward, for `Z` we -->
<!-- yield the given list, and for every `S` constructor of the given -->
<!-- number we drop an element from the list, and the empty list always -->
<!-- yields the empty list. -->
Let us now take a look at the simplified implementation
of `putHalvePeano` that uses peano numbers instead of `Int`
values and works on a list with two elements.

```{.haskell}
putHalvePeano :: [a] -> [a]
putHalvePeano xs' | lengthPeano xs' == S Z = xs' ++ [()]
```

`lengthPeano v == S Z where v free`

$\rightarrow$

`lengthPeano ([] ? _x3:xs) == S Z where _x3,xs free`

$\rightarrow$

`(lengthPeano [] ? lengthPeano (_x3:xs)) == S Z where _x3,xs free`

$\rightarrow$

`lengthPeano [] == S Z ? lengthPeano (_x3:xs) == S Z where _x3,xs free`

$\rightarrow$

`Z == S Z ? S (lengthPeano xs) == S Z where xs free`

$\rightarrow$

`Z == S Z ? lengthPeano xs == Z where xs free`

$\rightarrow$

`False ? lengthPeano [] ? lengthPeano (_x4:_x5) == Z where _x4,_x5
free`

$\rightarrow$

`False ? lengthPeano [] == Z ? lengthPeano (_x4:_x5) == Z
where _x4,_x5 free`

$\rightarrow$

`False ? Z == Z ? S (lengthPeano _x5) == S where _x5 free`

$\rightarrow$

`False ? True ? False`

In the end, the expression yields the following result in KICS2:

```
{v = []} False
{v = [_x3]} True
{v = (_x3:_x4:_x5)} False
```

That is, the expression `putHalve [(),()] v == [(),()] where v free`
evaluates to `{v = [()]} True`.

The main difference to the first implementation is that length can
propagate the constructor at the front of the remaining
evaluation. That is, the nested `?`-operators only occur as the argument of  a sequence of
`S`-constructors, which leads to a terminating search. The last line
of the example shows that no further guesses for free variables are
necessary, because the partial evaluation of `S n` can never be
evaluated to  `Z`, hence, the expression fails and the evaluation terminates.

### Binary List Representation

The second approach is to choose another list representation, more
precisely, a representation that behaves well with the internal
`BinInt` data structure. We define binary lists as follows.

```{.haskell}
data L a = LIHi a | LO (L (a,a)) | LI (L (a,a)) a
data BinaryList a = Empty | NonEmpty (L a)
```

At first, we define the data structure for non-empty lists that
corresponds to binary numbers, where `LIHi a` is a list with one
element, `LO (L (a,a))` represents a list with at least two elements,
and `LI (L (a,a)) a` is the constructor for an at least three-valued
list. Since this data structure has no representation for an empty
list, we introduce an additional data type `BinaryList` that wraps a
constructor `NonEmpty` around the list representation `L a` and
consists of a constructor `Empty` for an empty list, respectively.

```{.haskell}
lengthBList :: BinaryList a -> BinInt
lengthBList Empty           = Zero
lengthBList (NonEmpty list) = Pos (lengthL list)
 where
  lengthL :: L a -> Nat
  lengthL (LIHi _) = IHi
  lengthL (LO l)   = O (lengthL l)
  lengthL (LI l _) = I (lengthL l)
```

With the given type `BinaryList` we can utilize that we have a special
constructor for non-empty lists with an inner
representation. Therefore, we can propagate `Pos` for an non-empty
list without evaluating the actual inner list that `NonEmpty` is
holding. Furthermore, the list structure reflects which
`Nat`-constructor to use, so that, the constructor is again propagated
to the front of the expression. Again, let us see the evaluation in
action for the `putHalve` example.

```{.haskell}
putHalveBinaryList :: BinaryList a -> BinaryList a -> BinaryList a
putHalveBinaryList xs' | lengthBList xs' == Pos IHi = xs' ++ [()]
```

The following evaluation steps through the expression `lengthBList v
== Pos IHi where v free` that is essential for the evaluation of `get
putHalveBinaryList` applied to an arbitrary list.

`lengthBList v == Pos IHi where v free`

$\rightarrow$

`lengthBList (Empty ? NonEmpty xs) == Pos IHi where xs free`

$\rightarrow$

`(lengthBList Empty ? lengthBList (NonEmpty xs)) == Pos IHi where xs free`

$\rightarrow$

`lengthBList Empty == Pos IHi ? lengthBList (NonEmpty xs) == Pos IHi where xs
free`

$\rightarrow$

`Zero == Pos IHi ? Pos (lengthL xs) == Pos IHi where xs free`

$\rightarrow$

`False ? Pos (lengthL (LIHI _x2 ? LO _x2 ? LI _x2 _x3) ) == Pos IHi
where _x2,_x3 free`

$\rightarrow$

`False ? (lengthL (LIHI _x2 ? LO _x2 ? LI _x2 _x3) == IHi
where _x2,_x3 free`

$\rightarrow$

`False ?  (lengthL (LIHi _x2) ? lengthL (LO _x2) ? lengthL (LI
_x2 y)) == IHi where _x2,_x3 free`

$\rightarrow$

`False ? lengthL (LIHi _x2) == IHi ? (lengthL (LO _x2) ?
lengthL (LI _x2 _x3)) == IHi where _x2,_x3 free`

$\rightarrow$

`False ? IHi == IHi ? (O (lengthL _x2) ? I (lengthL _x2 _x3)) == IHi where _x2,_x3 free`

$\rightarrow$

`False ? True ? O (lengthL _x2) == IHi ? I (lengthL _x2 _x3) == Pos IHi where _x2,_x3 free`

$\rightarrow$

`False ? True ? False ? False`

In the end, this expression yields the following result in KICS2:

```
{v = Empty} False
{v = (NonEmpty (LIHi _x2))} True
{v = (NonEmpty (LO _x2))} False
{v = (NonEmpty (LI _x2 _x3))} False
```

That is, the expression `putHalveBinaryList v == [(),()] where v free` yields `{v = NonEmpty
(LIHi ())} True` and `get putHalveBinaryList [(),()] ` yields
`NonEmpty (LIHi ())`, respectively. 
