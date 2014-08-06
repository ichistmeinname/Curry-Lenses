% Header mit Deklarationen
%include extras/header.lhs
%include polycode.fmt
%include greek.fmt
\arrayhs

%format parse_a = "parse_a"
%format print_a = "print_a"
%format tau_1   = "~\tau_1"
%format tau_k   = "~\tau_k"
%format ...     = "\dots"

\begin{document}

% Römische Nummerierung für Sonderseiten, wie Verzeichnisse und Anhang
\pagenumbering{Roman}

% Titelblatt
%include extras/frontpage.lhs
% Table of Contents, Figures, etc
% Kopfzeile links Kapitel, rechts leer
\renewcommand{\chaptermark}[1]{\markboth{\thechapter\ #1}{}}
\ihead{\leftmark}
\ohead{}
%include extras/tables.lhs

% neue Umgebungen
\newtheoremstyle{break}
  {\topsep}{\topsep}%
  {\itshape}{}%
  {\bfseries}{}%
  {\newline}{}%
\theoremstyle{break}
\newtheorem*{tf}{Transformation}
\newtheorem*{ex}{Example}

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

% Setze Nummerierung wieder auf römisch zurück und setzte von oben fort
% Wert ist demnach der von 'roemisch'

\newpage
\pagenumbering{Roman}
\setcounter{page}{\value{roemisch}}

% Appendix, falls vorhanden
\appendix
%include extras/appendix.lhs

% Literaturverzeichnis
\bibliography{lenses}

% Eidesstattliche Erklärung
% \input{extras/eidesstattliche}

\end{document}
