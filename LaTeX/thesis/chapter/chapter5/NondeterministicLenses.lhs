\section{Nondeterministic Lenses}

Due to our choice to use Curry as programming language, we want to
investigate the applicability of lenses in a nondeterministic
context. %
Let us use the setting of contacts from the previous section as
example. %
Instead of changing the underlying data structure to a list of
addresses, we model the possibility of several addresses
nondeterministically. %

\begin{spec}
contactSample :: Contact
contactSample = Contact ("John","Sample") address1

address1 :: Address
address1 = "Any Street 213"
address1 = "Working Avenue 17"
\end{spec}

That is, we have two rules for the definition of John's address: one
rule for his home address and one to contact him at work. %
If we execute |contactSample| in the interactive environment of KiCS2,
we get the following two results. %

\begin{spec}
> contactSample
Contact ("John","Sample") "Any Street 213"
Contact ("John","Sample") "Working Avenue 17"
\end{spec}

As a consequence, we also get two results if we use the lens |address|
to project the address of |contactSample|. %
However, the put direction does not behave nondeterministically,
because we ignore the current address. %
Note, we use |get| and |put| as selector and update function of
lenses, respectively, instead of the subscripted variant of the
previous section. %

\begin{spec}
> get address person1
"Any Street 213"
"Working Avenue 17"

> put address person1 "Sesame Street 123"
Contact ("John","Sample") "Sesame Street 123"
\end{spec}

The order of the nondeterministic results depends on the order of the
definition's rules. %
The the first rule of |address1| defines the private and the second
rule the working address, thus, leading to the order when evaluating
|contactSample|. %

Note, |address| is not a lens definition with a nondeterministic
get function, but the nondeterministic behaviour is introduced by the
definition of |address1|. %
We use |address1| in our exemplary expressions to gain
nondeterministic results in the get direction, but deterministic
values for the put function.\footnote{However, we give a lens with a
  nondeterministic get function in the next subsection.} %

% Let us use the setting of contacts of the previous section as
% example, but with a slight change of the |Contact| daya type. %

% \begin{spec}
% data Contact = Person (Address,Address)
% type Person = (String,String)
% type Address = String
% \end{spec}

% We change the |Contact| structure in order to associate a pair of
% addresses to a person. %
% With this modification, we can have the following contact information
% for John Sample. %

% \begin{spec}
% person1 :: Contact
% person1 = Contact ("John","Sample") (address1,address2)

% address1 :: (Address,Address)
% address1 = "Any Street 213"

% address2 :: (Address,Address)
% address2 = "Working Avenue 17"
% \end{spec}

% The contact information consists of a private address and an address
% for John's work place. %
% Next, we define lenses to change or project the address
% of a given contact that distinguish between private and work addresses. %

% \begin{spec}
% addressWork :: Lens Contact Address
% addressWork (Contact name (homeAddr,_)) newAddr =
%   Contact name (homeAddr,newAddr)

% addressHome :: Lens Contact Address
% addressHome (Contact name (_,workAddr)) newAddr =
%   Contact name (newAddr,workAddr)
% \end{spec}

% In the end, we use both of these implementation to reimplement a lens
% that yields a lens on the private and working address nondeterministically. %

% \begin{spec}
% address :: Lens Contact Address
% address = addressHome ? addressWork
% \end{spec}

% For our example value |person1|, we get two results when using
% |address| to project the address field. %
% Consequently, setting the address field changes the private and
% working address nondeterministically. %
% Thus, the update yields two results as well. %

% \begin{spec}
% > get address person1
% "Any Street 213"
% "Working Avenue 17"

% > put address person1 "Sesame Street 123"
% Contact ("John","Sample") ("Sesame Street 123","Working Avenue 17")
% Contact ("John","Sample") ("Any Street 213","Sesame Street 123")
% \end{spec}

% The order of the nondeterministic results depends on the definition. %
% In the definition of |address| we use the lens for the private address
% as first argument to the choice operators and the working address as
% second arguments. %
% This order leads to an output with |Any Street 213| as first and
% |Working Avenue 17| as second result. %

As an example for a nondeterministic put function, we
modify our definition of |sub fstAndInc put| given in
Section~\ref{ex:fstAndInc}. %
The modified version increments the second component or yields the old
value; the first component is updated by the given new value as
before. %

\begin{spec}
(sub fstAndInc put) :: Lens (a,Int) a
(sub fstAndInc put) (_,i) y = (y,i+1 ? i)
\end{spec}

The following exemplary expressions show the behaviour of |sub
fstAndInc put|. %
Whereas the get direction of our lens definition is deterministic, the
put function behaves nondeterministically. %

\begin{spec}
> get (sub fstAndInc put) ("Super Mario",2)
"Super Mario"

> put (sub fstAndInc put) ("Super Mario",3) "Luigi"
("Luigi",4)
("Luigi",3)
\end{spec}

\subsection{Extended Laws}

The attentive reader might wonder why we use a lens definition that we
explicitly exposed as incorrect before. %
Consequently, the question arises of how this
nondeterministic behaviour interacts with well-behavedness and the lens
laws, respectively. %
Therefore, we discuss the definition of laws for nondeterministic
lenses. %

% In the following equations, we state the adapted laws for
% well-behaved and very well-behaved nondeterministic lenses. %
%  %
% \begin{align*}
% |get (put s v)| =~& |v| \tag{PutGet}\\
% |put s (get s)| =~& |s| \tag{GetPut}\\
% \end{align*}
% %
% Again, the attentive reader might get suspicious. %
% The given laws consist of exactly the same equations that we have
% introduced in Section~\ref{sec:lenses}. %

As a reminder, we recapitulate the lens laws. %

\begin{align*}
|get (put s v)| =~& |v| \tag{PutGet}\\
|put s (get s)| =~& |s| \tag{GetPut}\\
|put (put s v) v'| =~& |put s v'| \tag{PutPut}\\
\end{align*}

The main idea of the adapted laws is to change the semantics of the
equational operator instead of modifying the equation itself. %
In a nondeterministic setting, expressions evaluate to a set of
results rather than one result as in a functional or imperative
context. %
This notion of expressions gives rise to a modified notion of the
equivalence of two expressions. %
In the context of nondeterministic lenses, we interpret the equational
operator in terms of sets. %
In particular, we want the smaller set to be a subset of the greater
one. %
We can specify the lens laws in a nondeterministic context with the
following equations, which are also applicable for singleton sets,
i.e., for deterministic values. %
%
\begin{align*}
|v| \subseteq~& |get (put s v)| \tag{PutGet-Nondet}\\
|s| \subseteq~& |put s (get s)| \tag{GetPut-Nondet}
\end{align*}
%
For the PutGet law, we demand the value that we put into the source to
be one of the elements that we can get out of the modified source. %
We can state a similar requirement for GetPut: if we get a value out
of a source and put it back again, the resulting set of sources should
at least contain the initial source. %

We can define a similar equation for the PutPut law, but do not check
the law for our examples above. %
Instead, we postpone a detailed example for the PutPut law to the next
subsection. %
%
\begin{align*}
|put s v'| \subseteq~& |put (put s v) v'| \tag{PutPut-Nondet}
\end{align*}
%

With the first two equations in mind, we can check the lens
definitions given above. %
We start with |address|, the lens definition with a nondeterministic
get direction. %
In case of the GetPut law, the modified view of the put action is a
nondeterministic value. %
Thus, the put action yields a nondeterministic result as well. %
Fortunately, the resulting set is a superset of the modified view. %

%format _supseteq_ = "\supseteq"
\begin{spec}
put address person1 (get address person1)
==
{ Contact ("John.","Sample") "Working Avenue 17"
, Contact ("John.","Sample") "Any Street 213" }
_supseteq_
{ Contact ("John.","Sample") "Working Avenue 17"
, Contact ("John.","Sample") "Any Street 213"}
==
person1
\end{spec}

For a nondeterministic address entry, the PutGet law holds even with
the old semantics, because the put direction yields a deterministic
result regardless of the address's value. %
The new semantics holds trivially, because we work only on
singletons. %

\begin{spec}
get address (put address "Sesame Street 123")
==
{"Sesame Street 123"}
_supseteq_
{"Sesame Street 123"}
\end{spec}

Next, we take a look at our example with a nondeterministic put
function. %
In case of the GetPut law, the inner function call of get is
deterministic and yields the first component of the given pair. %
Then, the put function yields two results nondeterministically: one
pair with the original second component and one pair with an
incremented second component. %
Remember,we increment the second component on every update in the
deterministic version of |fstInc|; thus, leading to a violation of te
GetPut law. %
However, the nondeterministic version is well-behaved with respect to
the modified lens laws. %

\begin{spec}
put fstInc ("Super Mario",2) (get fstInc ("Super Mario",2))
==
{("Super Mario",3),("Super Mario",2)}
_supseteq_
("Super Mario",2)
\end{spec}

At last, we need to check the PutGet law. %
The get function is applied to the resulting set of the put action
leading to a nondeterministic result. %
The put action yields two results with the same first
component, thus, leading to the same two results in the get
direction.\footnote{In the notion of sets, we can eliminate
  duplicates, but the elimination is not required to fulfil the
  equations of the current example.} %
Obviously, the modified view is a subset of the resulting set, which
contains the modified view twice. %

\begin{spec}
get fstInc (put fstInc ("Super Mario",2) "Luigi")
==
{"Luigi","Luigi"}
_supseteq_
{"Luigi"}
\end{spec}

\subsection{Productive Nondeterministic Lenses}

We motivate the usage of nondeterministic lenses with three additional
examples. %
Furthermore, we want test the PutPut law for one of these examples. %

In the previous subsection, we defined lenses with a nondeterministic
put and get function, respectively. %
Our next example behaves nondeterministically in both directions. %

\begin{spec}
replace :: Lens [a] a
replace (x:xs) y = y:xs ? x : replace xs y
\end{spec}

We define |replace| to nondeterministically replace an element of a
given list with a new value. %
This example is based on the implementation of |insert|, which is most
commonly used to define a nondeterministic function to produce all
permutations of a list. %
However, |insert| is not an applicable example, because the get
function fails for every input value. %
We take a quick look at the implementation to figure out why |insert|
is not suitable as a lens. %

\begin{spec}
insert :: Lens [a] a
insert []     y  = [y]
insert (x:xs) y  = y:x:xs ? x : putInsert x ys
\end{spec}

We nondeterministically insert a value at every possible position of
the list. %
However, the insertion of an element becomes a problem when we search
for a corresponding get function. %
For the get direction, we have to evaluate the following expression to
find a corresponding result. %

\begin{spec}
insert s v == s where v free
\end{spec}

That is, we search for an element to insert to the list, such that we get
the original list. %
In order to get the original list, we cannot change the given list at
all. %
Thus, there is no element to insert and no suitable result for this
expression to be true. %

For our example, we use |replace| instead of |insert| to avoid this
problem. %
As mentioned in the beginning, |replace| is nondeterministic in the
get as well as in the put direction. %
We nondeterministically get an element of the list if we use |replace|
in the get direction; the put direction replaces a given element
nondeterministically. %
The nondeterministic behaviour leads to the following results for
exemplary expressions. %

\begin{spec}
> get replace [1..3]
1
2
3

> put replace [1..3] 5
[5,2,3]
[1,5,3]
[1,2,5]
\end{spec}

If we check the PutPut law for |replace|, we get highly nondeterministic
results. %
The first application of put yields nondeterministic values and a
consecutive call to put is applied to all these results. %
That is, for a list with $n$ elements, two consecutive put actions
yield $n^2$ results. %
For our examples, we use rather small lists to reduce the number of
results. %

\begin{spec}
> put replace (put replace [1,2] 4) 5
[5,2]
[4,5]
[5,4]
[1,5]
\end{spec}

For the PutPut law, we stated that the set of running two consecutive
put actions should at least contain the set of the second put
action. %
In our example, the PutPut law is fulfilled, because |[5,2]| and
|[1,5]| are part of the results. %
In particular, the PutPut law does not require any result of the first
put action to be in the set as well. %
If we take a look at our example again, we can see that the
intermediate results of the first put action -- |[4,2]| and |[1,4]| --
are not part of the result. %

Moreover, the usage of nondeterministic lenses gives rise to
functions that cannot be defined in a deterministic setting. %
For instance, we would have to change the definition of |replace| as
follows. %

\begin{spec}
replace' :: [a] -> a -> [[a]]
replace' []      _ = []
replace' (x:xs)  y = y:xs : map (x:) (replace' xs y)
\end{spec}

As a consequence, the type of the modified implementation is not
applicable for lenses -- the first argument and the result have to be
of the same type. %

Our second example of nondeterministic lenses is the definition of a
pretty-printer with a corresponding parser in the get direction. %
We introduce this concept of so-called \emph{printer-parsers} in
detail in Section~\ref{sec:printerParser}. %
A lens for pretty-printers consists of a get function for parsing, and
put function for pretty-printing. %
The get function can be nondeterministic to model traditional parser
structures with a list of results. %
That is, when we parse a string, we yield all corresponding results
nondeterministically. %

Later, we introduce printer-parsers with a handful of examples on
arithmetic expressions with prefix notation. %
We decided to use prefix notation, because the corresponding
definitions are simple and easy to follow. %
The string representation of these arithmetic expression are, however,
not ambiguous and yield a deterministic result for the parsing
direction. %
Therefore, we want to use an example for arithmetic expressions with
infix notation in advance to provide another nondeterministic
lens definition. %

The following example uses arithmetic expressions that are defined as
follows. %

\begin{spec}
data Expr     = BinOp Op Expr Expr
              | Num Int
data Op       = Plus | Mult | Div | Minus
\end{spec}

Let us assume that we have a lens |ppExpr'| to parse and pretty-print
arithmetic expressions in infix notation. %
Then, we can use the nondeterministic behaviour of the parsing
direction to yield all possible results. %
For example, a simple expression with a binary operator can be parsed
in two ways: (1) we only parse the first argument as an identifier;
(2) we parse the string as an expression with binary operator, thus,
parsing both arguments as identifiers with the operator in the
middle. %

\begin{spec}
> get ppExpr' "1 + 2"
((BinOp Plus (Num 1) (Num 2)),"")
((Num 1)," + 2")
\end{spec}

Obviously, the number of possible parsing results increases with the
size of the arithmetic expression. %
For an expression with two binary operators, we have three possible
parsing results. %

\begin{spec}
> get ppExpr' "1 - 2 * 3"
((BinOp Minus (Num 1) (BinOp Mult (Num 2) (Num 3))),"")
((BinOp Minus (Num 1) (Num 2))," * 3")
((Num 1)," - 2 * 3")
\end{spec}

However, we can disambiguate the expression by adding parentheses
resulting in a deterministic parsing result. %

\begin{spec}
> get ppExpr' "(1 - 2) * 3"
((BinOp Mult (BinOp Minus (Num 1) (Num 2)) (Num 3)),"")
\end{spec}

In Appendix~\ref{a:ppInfix}, we present the implementation of
|ppExpr'| and make additional comments about the problems we ran
into. %
All in all, our definition of a printer-parser and similar ideas
given in Section~\ref{sec:printerParser} are prime examples for the
usage of nondeterministic lenses. %