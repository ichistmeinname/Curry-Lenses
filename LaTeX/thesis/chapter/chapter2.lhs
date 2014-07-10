\chapter[About Bidirectional Transformations]{About Lenses and Other Bidirectional Shenanigans}

\section{Bidirectional Programming}
Typical problems that are based on bidirectional transformations are
handled with two separate functions. %
That is, one function maps the concrete value to the abstract
representation and one function serves as backward direction, from the
abstract value to the concrete representation. %
This is rather error-prone and tedious to maintain, because, firstly,
we have to keep the two functions in sync by hand, in order to
guarantee correctness; secondly, changes in one of the representations
affects both functions in order to maintain round-tripping rules. %
This unidirectional programming mechanism is well-studied and many
programmers are familiar with this paradigm. %
Whereas bidirectional programming is a new approach on a specific
domain of problems, which becomes more and more popular in different
areas of computer science. %
Software engineering, programming languages, databases and graph
transformations are some of the current fields of computer science
that participate in research activities concerning bidirectional
transformations. %

So, what is the new, challenging feature of bidirectional programming,
that keeps researchers busy? %
A bidirectional transformations does not consist of two functions like
in the unidirectional way, but of one function, that can be read
forward and backward. %
In literature, we distinguish between a forward function |get :: A ->
B|, and a backward function |put :: B -> A|; |A| is most
commonly called the source and |B| is the view, this naming convention
originates from applications in databases. %
This two functions form, in the easiest approach, an bijection from
|A| to |B| and back. %
In the next section, we discuss lenses, a more general approach of
bidirectional transformation, which, additionally, are one of the most
popular forms in bidirectional programming. %

\begin{itemize}
\item the view update problem
\item status quo \cite{biPApproaches}
\end{itemize}

\section{Lenses}
Lenses describe bidirectional transformations that originate in
databases as introduced by \cite{viewUpdate}.
In the setting of lenses, the |get| function describes a transformation from |A| to |B|, in
most applications |B| is a subset of |A|, and information are
discarded from |A| to |B| respectively. %
On the other hand, the |put| function synchronises a given, potentially
updated, view with respect to the original source. %
A popular example from databases shows the correspondences quite well:
we have database with a set of data |S| and a query that yields a
table |B| that matches the given criteria. %
The query is the forward transformation |get|. %
In a second step, we modify the resulting table, because we recognise
a misspelled name field or suchlike, which yields to a updated table
|B'|. %
We definitely want to propagate the update back to our database; this
is where the |put| function comes into play. %
The |put| function synchronises our changes of the view, |B'|, with
the original database set |A|. %
Figure \ref{fig:bit} illustrates the discussed situation of an updated
view, which is synchronised with its original source. %

\begin{figure}[h]
\begin{center}
  \includegraphics[width=0.75\textwidth]{../images/bx8iVC.pdf}
\end{center}
\caption{Bidirectional transformations consist of a pair of
    functions: |get| and |put|}
\label{fig:bit}
\end{figure}

Furthermore, the community distinguishes two characteristics of
lenses: \emph{symmetric} and \emph{asymmetric} lenses. %
The typical case is an asymmetric setting. %
As we stated at the beginning of the section, in most applications the
view is a subset of the source. %
That is, the |get :: A -> B| function discards some information, when it
transforms a source of type |A| to a view of type |B|. %
This setting is called asymmetric, because we only regard changes of
the view that will be propagated back to the source. %
The definition of the |put| function we
introduced above, needs to be adapted for the asymmetric setting. %
We only need to synchronise updated view with a source, so that we
need |put| to take the initial source as argument. %
In a symmetric setting, both sides can be updated, so that the |get|
functions takes an additional argument in comparison with our
definition of |put| above. %
In the following, we will only consider asymmetric lenses in a
detailed manner. %
For an detailed introduction to symmetric lenses, you should take a
look at the work of \cite{symmLenses}. %

\begin{itemize}
\item state vs operation-based
\item symmetric \cite{symmLenses} vs asymmetric
\end{itemize}

\subsection{Laws}

So far, we characterised lenses as a bidirectional transformation with
an adapted |put| function, which allows round-tripping behaviour. %
It is important to state that lenses fulfil certain laws. %
The first law states that, if we update a given source with a specific
view and transform the result to a view afterwards, we get the view
that we just put in. %

\begin{equation}\tag{GetPut}
|get (put s v) = v|
\end{equation}

This law is called \emph{GetPut}, which is read from right to left,
since we first |put| a new value in our source and then try to |get|
it out again. %

As an example, let us take a look at bidirectional transformation with
a pair of |String| and |Integer| as the domain of the source, and
|String| as the view's domain. %
In order to define an appropriate |get| function, we need a function
|get :: (String,Integer) -> String|. %
In Curry, it already exist a function with such an type, namely
|fst|. %

\begin{code}
  get :: (String,Integer) -> String
  get (str,int) = str
\end{code}

Our function |get| yields the first component of a pair with no
further changes or adjustments to the value; this definition is equivalent to
|fst| %
The |put| function has the form |put :: (String,Integer) -> String ->
(String,Integer)|; we define a function that sets the first component
of a pair with a given string without further ado. %

\begin{code}
  put :: (String,Integer) -> String -> (String,Integer)
  put (str,int) newStr = (newStr,int)
\end{code}

In order to test the \emph{GetPut} law, we first run the functions on
example values. %

\begin{spec}
  > put ("foo",42)
  "bar" ("bar",42)
  > get ("bar",42)
  "bar"
\end{spec}

We can even show that the defined pair of get and put behaves
according to the law for every initial value and additional string. %

\begin{proof}
For all $w$, $v$ and $v'$, where $(v,w)$ is of type |(String,Integer)|
and $v'$ is of type |String|, it holds |get (put (v,w) v') = v'|. %
\def\commentbegin{\quad\{\ } \def\commentend{\}}
\begin{spec}
  get (put (v,w) v')
== {- definition of |put| -}
   get (v',w)
== {- definition of |get| -}
  v'
\end{spec}
\end{proof}

In addition to the \emph{GetPut} law, lenses are also supposed to
fulfil a second round-tripping criteria. %
The \emph{PutGet} law states that if we |get| a view out of a source
and |put| it back again, the source does not change at all, as if
nothing happend. %

\begin{equation}\tag{PutGet}
|put s (get s) = s|
\end{equation}

\begin{proof}
With our example above, we obtain the following equation, where for all $w$
and $v$ where $(v,w)$ is of type |(String,Integer)|, it holds
|put (v,w) (get (v,w)) = (v,w)|. %
\def\commentbegin{\quad\{\ }
\def\commentend{\}}
\begin{spec}
  put (v,w) (get (v,w))
== {- definition of |get| -}
  put (v,w) v
== {- definition of |put| -}
  v
\end{spec}
\end{proof}

\begin{figure}[h]
\begin{minipage}{0.47\textwidth}
  \begin{center}
    \includegraphics[width=\textwidth]{../images/ex1-fst-getput}
  \end{center}
\end{minipage}
	\hfill
\begin{minipage}{0.47\textwidth}
  \begin{center}
    \includegraphics[width=\textwidth]{../images/ex1-fst-putget}
  \end{center}
\end{minipage}
\caption{GetPut Law (left) and PutGet Law (right)}
\label{fig:laws}
\end{figure}

In Figure \ref{fig:laws}, we illustrate both lens laws; the
different colouring of the view distinguishes the original value of the
view and the new updated view. %
In the pioneering work of \cite{biTCombinators} in the topic of
bidirectional programming and lenses, a lens is called \emph{well-behaved} if both laws, the \emph{GetPut} and
the \emph{PutGet} law, hold. %

There is also a third lens law, which is called \emph{PutPut}. %
The lens satisfies the \emph{PutPut} law if we run two consecutively |put|
operations on a source with two different views, but only the second
|put| matters. %
That is, we can formulate this law with the following equation.  %

\begin{equation}\tag{PutPut}
|put (put s v) v' = put s v'|
\end{equation}

In most applications, the \emph{PutPut} does not play an important
role, because the preconditions are too strong. %
That is, plenty of constructive well-behaved lens are not very
well-behaved. %

\todo{Examples}

Furthermore, more and more frameworks for bidirectional
transformations and bidirectional programming languages respectively,
endorse a weaker notion of the presented \emph{PutGet} and
\emph{GetPut} law. %
In our current notion of the laws, we only consider total |get| and
total |put| functions. %
In practice, most of the time we do not want to work with total
functions only. %

\todo{Examples}

In order to use partial lenses, we need to adjust the lens
laws by means of partiality. %
In the following, the expression $|(f x)|\downarrow$ is satisfied, if
the function $f$ yields a result for the argument $x$. %
We define the partial version of \emph{PutGet} and \emph{GetPut} in
terms of inference rules. %
That is, if the above condition is not satisfied, the equation below
does not need to be checked and the rule trivially holds. %

\begin{equation}\tag{Partial-PutGet}
\dfrac{|(put s v)|\downarrow}{|get (put s v) = v|}
\end{equation}

\begin{equation}\tag{Partial-GetPut}
\dfrac{|(get s)|\downarrow}{|put s (get s) = s|}
\end{equation}

\begin{itemize}
\item weaker laws \cite{biTProperties}
\end{itemize}

\section{Applications}
\begin{itemize}
\item Boomerang \cite{boomerang}
\item HaXML \cite{haxml}
\item Model transformation \cite{parsing1}
\end{itemize}

\chapter{? Introduction to Curry ?}
