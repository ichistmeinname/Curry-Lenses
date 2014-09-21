% Header mit Deklarationen
%include extras/header.lhs
%include polycode.fmt
%include greek.fmt
%include forall.fmt
%include colorcode.fmt
\definecolor{codecolor}{rgb}{1,1,1}
\colorhs
%include lineno.fmt
\arrayhs
\numbersoff

%format parse_a = "parse_a"
%format print_a = "print_a"
%format (tau i)  = "\tau_" i
%format ...     = "~\dots~"
%format Rec = "Rec"
%format (label1 i) = "f_{" i "}"
%format (get__ a) = "get_{" a "}"
%format (set__ a) = "set_{" a "}"
%format (sub (t) (i)) = t "_{" i "}"
%format (val (i)) =  "val_{"i "}"
%format -.- = "~\cdots~"
%format (label f i) = f "_{" i"}"
%format <$$> = "<\$>"
%format <*> = "<*>"
%format <||> = "<|>"
%format *> = ">>>"
%format <* = "<<<"

% neue Umgebungen
% \newtheoremstyle{break}
%   {7pt}{10pt}%
%   {\itshape}{}%
%   {\bfseries}{}%
%   {0pt}{}%
% \theoremstyle{break}
\newtheorem{tf}{Transformation}
\newtheorem{ex}{Example}


\begin{document}

% Römische Nummerierung für Sonderseiten, wie Verzeichnisse und Anhang
\pagenumbering{Roman}

% Titelblatt
%include extras/frontpage.lhs

% Eidesstattliche Erklärung
%include extras/eidesstattliche.lhs

% Abstract
%include extras/abstract.lhs

% Table of Contents, Figures, etc
% Kopfzeile links Kapitel, rechts leer
\renewcommand{\chaptermark}[1]{\markboth{\thechapter\ #1}{}}
\ihead{\leftmark}
\ohead{}
%include extras/tables.lhs

% Merke mir die römische Seitenzahl in 'roemisch' und setzte Nummeriernung 
% auf arabisch für die eigentlichen Kapitel
\newpage
\newcounter{roemisch}
\setcounter{roemisch}{\value{page}}
\pagenumbering{arabic}

% Die einzelnen Kapitel
% Kopfzeile: links Kapitel, rechts Sektion
\ihead{\leftmark}
\ohead{\rightmark}
%include chapter/chapter1.lhs
%include chapter/chapter2.lhs
%include chapter/chapter3.lhs
%include chapter/chapter4.lhs
%include chapter/chapter5.lhs
%include chapter/chapter6.lhs
%include chapter/chapter7.lhs

% Appendix, falls vorhanden
\appendix

% Literaturverzeichnis
\bibliography{lenses}

%include extras/appendix.lhs

\end{document}
