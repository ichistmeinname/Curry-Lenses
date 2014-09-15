\section{Nondeterministic Lenses}

Due to the usage of Curry as programming language of our choice, we
want to investigate the applicablity of lenses in a nondeterministic
context. %
Let us use the setting of contacts of the previous section as
example. %
We can have the following contact information of John Q. Public. %

\begin{spec}
person1 :: Contact
person1 = Contact ("John","Sample") address1

address1 :: Address
address1 = "Any Street 213"
address1 = "Working Avenue 17"
\end{spec}

The contact information consists of a private address and an address
for John's work place. %
Instead of changing the underlying data structure to a list of
addresses, we model the possibility of several addresses
nondeterministically. %
That is, we have two rules for the definition of John's address: one
rule for his home adress and one to contact him at work. %
If we execute |person1| in the interactive environment of KiCS2, we
get the following two results. %

\begin{spec}
> person1
Contact ("John Q.","Public") "Any Street 213"
Contact ("John Q.","Public") "Working Avenue 17"
\end{spec}

As a consequence, we also get two results if we use the lens |address|
to project the address of |person1|. %
However, the put direction does not behave nondeterministically,
because we ignore the current address. %

\begin{spec}
> get address person1
"Any Street 213"
"Working Avenue 17"

> put address person1 "Sesame Street 123"
Contact ("John","Sample") "Sesame Street 123"
\end{spec}

The order of the nondeterministic results depends on the rules of the
definition. %
The the first rule of |address1| defines the private and the second
rule the working addresss, thus, leading to the order wehen evaluating
|person1|. %

In order to give an example with a nondeterministic put function, we
modify our definition of |sub fstInc put| given in
Section~\ref{ex:fstInc}. %
The modified version increments the second component or yields the old
value; the first component is update by the given new value as
before. %

\begin{spec}
(sub fstInc put) :: Lens (a,Int) a
(sub fstInc put) (_,i) y = (y,i+1 ? i)
\end{spec}

The following exemplary expressions show the behaviour of the |sub
fstInc put|. %
In this case, the get direction of our lens definition is
deterministic, whereas the put function behaves
nondeterministically. %

\begin{spec}
> get (sub fstInc put) ("Super Mario",2)
"Super Mario"

> put (sub fstInc put) ("Super Mario",3) "Luigi"
("Luigi",4)
("Luigi",3)
\end{spec}

\subsection{Extended Laws}

The attentive reader might wonder why we use a lens definition that we
explicitely expose as incorrect. %
Consequenlty, there arises the question of how does this
nondeterministic behaviour interact with well-behavedness and the lens
laws, respectively. %
Therefore, we discuss the definition of laws for nondeterministc
lenses. %

In the following equations, we state the adopted laws for
well-behaved and very well-behaved nondeterministic lenses. %
  
\begin{align*}
|get (put s v)| =~& |v| \tag{PutGet}\\
|put s (get s)| =~& |s| \tag{GetPut}\\
|put (put s v) v'| =~& |put s v'| \tag{PutPut}
\end{align*}

Again, the attentive reader might get suspicious. %
The given laws consist of exactly the same equations that we have
introduced in Section~\ref{sec:lenses}. %
The main idea of the adopted laws is to change the semantics of the
equational operator instead of modifying the equation itself. %
In a nondeterministic setting, expressions evaluate to a set of
results rather than one result as in a functional or imperative
context. %
This view of expressions results in a modified view on the equivalence
of two expressions. %
In the context of nondeterministic lenses, we interpret the equational
operator in terms of a subset operator. %
That is, an equation |a = b| is holds if the result of evaluating |a|
is a subset of the resulting list for the evaluation of |b| or vice
versa. %

This interpretation leads to the following view of the lens laws. %

\begin{align*}
|get (put s v)| \subseteq~& |v| \tag{PutGet}\\
|put s (get s)| \supseteq~& |s| \tag{GetPut}\\
|put (put s v) v'| \supseteq~& |put s v'| \tag{PutPut}
\end{align*}

For the PutGet law, we want the value that we put into the source to
be in one of the elements that we get out of the source. %
We can state a symmetric setting for GetPut: if we get a value out of
a source and put it back again, the resulting set of sources should at
least contain the initial source. %
In case of a nondeterministic put function, the set of running two
consecutive put actions should at least contain the set of the second
put action. %
The equation does not require any result of the first put action to be
in the set as well. %

With this equations in mind, we can check the lens definitions given
above. %
We start with |address|, the lens definition with a nondeterministic
get direction. %
For a nondeterminstic address entry, the PutGet law holds even with
the old semantic, because the put direction yields a deterministic
result regardless of the address's value. %

%format _supseteq_ = "\supseteq"
%format _subseteq_ = "\subseteq"
\begin{spec}
get address (put address "Sesame Street 123")
==
{"Sesame Street 123"}
_subseteq_
{"Sesame Street 123"}
\end{spec}

\begin{spec}
put address person1 (get address person1)
==
{ Contact ("John.","Doe") "Working Avenue 17"
, Contact ("John.","Doe") "Any Street 213" }
_supseteq_
person1
==
{ Contact ("John.","Doe") "Working Avenue 17"
, Contact ("John.","Doe") "Any Street 213"}
\end{spec}

\begin{spec}
get fstInc (put fstInc ("Super Mario",2) "Luigi")
==
{"Luigi","Luigi"}
==
{"Luigi"}
_subseteq_
{"Luigi"}
\end{spec}

\begin{spec}
put fstInc ("Super Mario",2) (get fstInc ("Super Mario",2))
==
{("Super Mario",3),("Super Mario",2)}
_supseteq_
("Super Mario",2)
\end{spec}

\subsection{Productive Nondeterministic Lenses}
