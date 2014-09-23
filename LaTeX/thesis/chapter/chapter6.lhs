\chapter{Case Studies}\label{ch:studies}

In this chapter, we discuss two case studies for the application of
lenses: (1) an implementation of a library for pretty-printing and
parsing; (2) a theoretical concept to generate lenses from record data
types in order to manipulate a particular record field. %

The first case study uses our put-based lens library to unify the
definition of a pretty-printer and parser: a printer-parser. %
That is, we discuss three different implementations of data structures
and lenses for a pretty-printer with a corresponding parser in the get
direction. %
We start off with a simple lens definition without further data
structures to model the printer-parser. %
% The put direction of our lens pretty-prints a given data structure and
% ignore the given input. %
% Additionally, we can parse a pretty-printed string with the get
% direction of the lens. %
Then, we give two additional approaches to solve emerging
problems and disadvantages of the naive implementation. %
% These approaches follow a slightly different semantic that allows to
% replace a given string with respect to its layout instead of simply
% ignoring it.  %
Moreover, we discuss related work and conclude the section with an
evaluation of these approaches and the applicability of lenses for
these approaches. %

Secondly, we propose to generate lenses for record data types. %
A similar idea is already integrated in Haskell with the extension
|OverloadedRecordFields|\footnote{\url{https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields}}. %
First, we take a look at the current usage of record syntax in Curry
in order to motivate our proposal. %
Record syntax in Curry is similar to the concept in Haskell, but uses
a special record selector in combination with labels instead of a
function to project a particular field. %
We define an exemplary module and give its generated
counterpart. %
Finally, we conclude with the definition of the underlying transformations to
generate the appropriate lenses for a record data type. %


%include chapter6/PrinterParser.lhs

%% %include chapter6/WUILenses.lhs

%include chapter6/Records.lhs
