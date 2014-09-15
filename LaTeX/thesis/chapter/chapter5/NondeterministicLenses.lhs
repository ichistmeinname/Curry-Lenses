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

> put (sub fstInc put) ("Super Mario",3) 2
("Super Mario",4)
("Super Mario",3)
\end{spec}

\subsection{Extended Laws}

The attentive reader might wonder why we use a lens definition that we
explicitely expose as incorrect. %
Consequenlty, there arises the question of how does this
nondeterministic behaviour interact with well-behavedness and the lens
laws, respectively. %
Therefore, we discuss the definition of laws for nondeterministc
lenses. %

In Figure~\ref{fig:lawsNonDet}  
\begin{figure}[h]
\begin{align*}
|get (put s v)| =& |v| \tag{PutGet}\\
|put s (get s)| =& |s| \tag{GetPut}\\
|put (put s v) v'| =& |put s v'| \tag{PutPut}
\end{align*}
\caption{Adopted lens laws for nondeterministic lenses}
\label{fig:lawsNonDet}
\end{figure}

\subsection{Productive Nondeterministic Lenses}
