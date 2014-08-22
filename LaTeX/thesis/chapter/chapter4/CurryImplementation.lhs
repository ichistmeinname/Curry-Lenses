\section{Implementation in Curry}\label{sec:Impl}


\subsection{Combinatorial Lens Library}\label{sec:ImplComb}


\subsubsection{Examples}\label{sec:ImplCombEx}


\subsection{Put-Lenses Library}\label{sec:ImplPut}


\subsubsection{Examples}\label{sec:ImplPutEx}


% \section{Get-Lenses vs Put-Lenses}\label{sec:GetVsPut}
% Get
% \begin{itemize}
% \item get is intuitive
% \item underspecified put, when only get is defined
% \item non-injective get functions
% \end{itemize}
% Put
% \begin{itemize}
% \item generated get is unique \cite{putback}
%   \begin{itemize}
%   \item requires putback functions to be affine and in treeless
%     form, that is, each view variable is used at most once and no
%     intermediate data structures are used in definitions
%   \item this class of functions has similarities to tree transducers
%   \item assumes only total functions
%   \item hybrid compositional approach, but focus on designing
%     language to specify various primitive putback functions over
%     algebraic data structures
%   \item validity of putback transformations - A put is valid, if
%     there exists a get such that \emph{GetPut} and \emph{PutGet} are
%     satisfied
%   \item Uniqueness of get - Given a put function, there exists at
%     most one get function that forms a well-behaved BX
%   \end{itemize}
% \item better suited for implementation with Curry
% \end{itemize}
