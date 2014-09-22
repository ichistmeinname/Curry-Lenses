\chapter{Different Implementation Approaches}\label{ch:impl}

Bidirectional programming is a rising topic in the field of computer
science, and many different approaches exist to tackle the problem. %
These approaches come from different disciplines of computer science
like databases, graph transformation, programming languages and
interface design. %
This section summarises the main two approaches and highlights
differences as well as some details. %

The main two techniques to work with bidirectional transformations are
combinatorial languages and bidirectionalisation. %
Most commonly, a combinatorial language is defined as a DSL in a
general purpose programming language or as a new programming language,
and provides a set of primitives, which can be combined to define
complex structures. %
The definitions of these primitives mostly consist of both functions,
the get and the appropriate put function. %
In contrast to this approach, the bidirectionalisation technique takes
an uni-directional function and synthesises this function to a
bidirectional one. %

The remainder of the chapter introduces combinatorial and
bidirectionalisation approaches for lenses. %
The former approach has two subcategories, because the implementation
can either focus on defining a |get| function or a |put| function;
such a subdivision was not investigated for the bidirectionalisation of
lenses yet.  In this context, we will discuss advantages and
disadvantages of defining a get function and present a first proposal
by \cite{putCombinators} to set the focus on the |put| function. %

%include chapter4/Combinators.lhs

%include chapter4/Bidirectionalisation.lhs
