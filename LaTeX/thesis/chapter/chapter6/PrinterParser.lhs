\section{Case Study I - Bidirectional Printer-Parser}\label{sec:printerParser}

Printer and parser are well-studied fields in Computer Science and
take an important role in the design of programming languages. %
In order to prove a programming language's expressiveness, the
implementation of a printer or parser library is a very popular
example. %
Printer and parser are also represented in processes that include
reading and updating data from files. %
For example, measurements of experiments in the field of biology are
often tracked in CSV files. %
In order to analyse these results, we need to read the CSV data from
the files, manipulate them and write the results back to another
file. %
Parsers specify the format of the CSV data in hand and yield an
editable structure in the source language, whereas printers write the
results back in a valid format. %
This process leads to the observation that printer and parsers fulfil
certain round-tripping rules, which need to be considered. %
We want to print the resulting data in a valid format, that is, the
printed results can be reread again by the parser. %
This observation leads to the following equation for a data structure
|val :: a| and associated functions |parse_a| for parsing and
|print_a| for printing, which are parametrised over the specific data
structure. %

\begin{equation}\tag{Print-Parse}
  |parse_a (print_a val) = val|
\end{equation}

We might also consider the inverse round-tripping rule and demand that
if we parse a string and print it again, we get the original string as
result. %
The following equation postulates this round-tripping rule. %

\begin{equation}\tag{Parse-Print}
  |print_a (parse_a str) = str|
\end{equation}

In certain scenarios, the \emph{Parse-Print} rule is a desirable
requirement. %
For example, when we choose a format for the data that is unambiguous,
both round-tripping rules together form the requirements for a correct
implementation. %
|Show| and |Read| instances for data structures in Haskell also form
the identity this round-tripping rules. %
On the other hand, in many scenarios regarding data acquisition, users
handle data manipulation, update, and sometimes even input manually. %
Especially in the field of programming languages, users write the code
that the parser consumes, whereas generated code may be part of many
programs. %
Therefore, most programming languages allow redundancies like spaces
and braces, so that a parsed data structure corresponds to more than
one string representation. %

Like for parser and printers, we need a handfull of combinators to
simplify the definition of new printer-parsers for our own
structure. %
At first, we declare our data structure as |type PP a = String ->
(a,String) -> String|, which is sometimes more, sometimes less used in
the implementations. %
The first argument is the input string, the second argument is pair
consisting of a representation and a remaining string. %
In the initial \emph{put}-direction, we concatenate the remaining
string and the end of the pretty-printed value of the given
representation. %
This representation\footnote{a slightly different one, but still
  equivalent} is most commonly used for pretty-printers to achieve a
linear run-time subject to the resulting pretty-printed string. %
Additionally, we have two functions |pretty :: PP a -> String ->
(a,String) -> String| and |parse :: PP a -> String -> (a,String)|. %
All following implementations define the same basic combinators, that
we will present next. %

As primitive printer-parsers, we define |digit :: PP Int| that
pretty-prints a digit in the \emph{put}- and parses a digit in the
|get|-direction.

\subsection{Similar Approaches}

\begin{itemize}
\item FlipPr \cite{flippr}
\item Invertible Syntax Description \cite{invertibleSyntax}
\item Arrows \cite{invertibleArrows}
\item unparsing/parsing \cite{parsing2}
\end{itemize}

\subsection{Printer-Parser}
In a first step, we implemented an abstraction for printing and
parsing, that only needs one definition. %
The data structure has the form |type PPrinter a = Lens String
(a,String)|. %
We need to define the `put`-direction for our lens, that is, our
printer-parser definitions have the following form: |ppExpr :: String
-> (a,String) -> String|. %

As a main disadvantage of this approach, we cannot ignore redundant
parts in the given string, when we want to parse, if this redundancies
do not appear in the pretty-printer definition. %
Furthermore, the definitions for the printer have to be more
sophisticated than usual. %
The printer and parser definitions are connected, that is, typical
restrictions known from parser constructions need to be considered. %
In our example, we only defined a prefix based arithmetic expression
data structure, but with infix operators, we have to avoid
left-recursion in the definitions. %

\subsection{Replace-Parser}

\subsection{Parser-Printer}
