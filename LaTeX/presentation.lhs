\documentclass{beamer}
\usenavigationsymbolstemplate{}
% \usepackage{beamerthemesplit} // Activate for custom appearance
\usepackage[latin1]{inputenc}
\usepackage{amsmath,amsfonts,amssymb}
\usepackage{float}
\usepackage{ulem}
\usepackage{booktabs}
\usepackage{graphicx}
\setbeamerfont{note page}{size=\tiny}
\usetheme{Pittsburgh}
\usepackage{helvet}
\usepackage{hyperref}

\setlength\parindent{0pt}

% Header mit Deklarationen
%include polycode.fmt
%include greek.fmt
%include forall.fmt

%format alpha = "\alpha"
%format beta  = "\beta"
%format gamma = "\gamma"
%format sigma = "\sigma"

\arrayhs

\title{Bidirektionale Programmierung und Linsen -\\Einmal Rupert und zur\"uck}
\author{Sandra Dylus}
\institute[IfI -- CAU Kiel]{Institut f\"ur Informatik \\ Christian-Albrechts-Universit\"at zu Kiel}
\subtitle{Vortrag}
\date{11. Juni 2014}

\setbeamertemplate{footline}[frame number]

\definecolor{newRed}{rgb}{0.7,0,0}
\definecolor{newBlue}{rgb}{0,0,0.7}
\definecolor{newGreen}{rgb}{0,0.7,0}
\newtheorem{dn}{Definition}
\newcommand{\mtext}[1]{\text{\textit{#1}}}

\begin{document}

%
% Titelseite
%
\begin{frame}
\titlepage
\end{frame}

\note{
Test
}
\begin{frame}{Bidirektionale Programmierung}
\includegraphics<1>[width=\textwidth]{images/bx2iVC.pdf}
\includegraphics<2>[width=\textwidth]{images/bx3iVC.pdf}
\includegraphics<3>[width=\textwidth]{images/bx4iVC.pdf}
\includegraphics<4>[width=\textwidth]{images/bx5iVC.pdf}
\includegraphics<5>[width=\textwidth]{images/bx6iVC.pdf}
\includegraphics<6>[width=\textwidth]{images/bx7iVC.pdf}
\includegraphics<7>[width=\textwidth]{images/bx8iVC.pdf}
\includegraphics<8>[width=\textwidth]{images/bx9iVC.pdf}

\end{frame}

\begin{frame}{Beispiele f\"ur Linsen}
\vspace{0.58cm}
\begin{minipage}[c]{0.49\textwidth}
\begin{flushleft}
\includegraphics<1>[width=0.9\textwidth]{images/ex1.pdf}
\includegraphics<2->[width=0.9\textwidth]{images/ex1-fst.pdf}
\end{flushleft}
\end{minipage}
\begin{minipage}[c]{0.49\textwidth}
\begin{flushright}
\includegraphics<1,2>[width=0.9\textwidth]{images/ex2.pdf}
\includegraphics<3>[width=0.9\textwidth]{images/ex2-length.pdf}
\end{flushright}

\end{minipage}
\end{frame}

\begin{frame}{Gesetze f\"ur Linsen}
\begin{minipage}{0.49\textwidth}
\begin{flushleft}
\includegraphics<1-2>[width=0.9\textwidth]{images/ex1-fst.pdf}
\includegraphics<3-5>[width=0.9\textwidth]{images/ex2-length.pdf}
\only<6->{
\visible<6->{
GetPut
\begin{code}
put s (get s) == s
\end{code}

\vspace{1.5cm}
\visible<7>{
PutGet
\begin{code}
get (put s v) == v
\end{code}
}
}
}
\end{flushleft}
\end{minipage}
\begin{minipage}{0.49\textwidth}
\begin{flushright}
\visible<1-7>{
\alt<1-3>{
\includegraphics[width=0.9\textwidth]{images/ex1-fst-putget.pdf}
}{
\includegraphics[width=0.9\textwidth]{images/ex2-length-putget.pdf}
}
}
\visible<2-7>{
\alt<2-4>{
\includegraphics[width=0.9\textwidth]{images/ex1-fst-getput.pdf}
}{
\includegraphics[width=0.9\textwidth]{images/ex2-length-getput.pdf}

}
}
\end{flushright}
\end{minipage}
\end{frame}

\note{
\begin{itemize}
\item Was versteht man unter Bidirektionale Programmierung
  \begin{itemize}
  \item Code bzw. Funktionen, die sowohl \emph{vorw\"arts} als auch \emph{r\"uckw\"arts} gelesen werden k\"onnen
  \item unspektakulaer: bijektiv, zwei Repr\"asentationen der gleichen Daten
  \end{itemize}
\item Gebrauch
  \begin{itemize}
  \item Synchronisation mehrerer Quelldaten (z.B. Modelltransformationen)
  \item Kleine Einheiten, die leicht zu manipulieren sind und in Quelldaten zur\"uckgeschrieben werden
  \end{itemize}
\item Linse als Beispielinstanz
\end{itemize}
\end{frame}
}

\note{
\begin{itemize}
\item Was ist eine Linse?
  \begin{itemize}
  \item in der vorw\"arts-Richtung gehen Informationen verloren $\rightarrow$ view-update-Problem
  \item bidirektionales Programm, das bestimmte Gesetze erfuellt
  \item Formen: symmetrisch, asymmetrisch
  \end{itemize}
\item Gesetze
  \begin{itemize}
  \item Round-trip Rules
  \item GetPut: |put lens s (get lens s)    = s|
  \item PutGet: |get lens (put lens s v)    = v|
  \item PutPut: |put lens (put lens s v) v' = put lens s v'|
  \item Gesetze nur in Abh\"angigkeit von `put`
  \end{itemize}
\item Beispiele
\end{itemize}
}

\note{
put lens source view | get lens s' == view = s'
get lens s | put lens s v == s = v
}

\begin{frame}[t]{Implementierungen von Linsen}
\begin{itemize}
\item Kombinator-Sprachen

\visible<2->{
\begin{code}
type Lens sigma alpha =  { getLens :: sigma -> alpha
                         , putLens :: sigma -> alpha -> sigma }
\end{code}
}

\item get-basierte Sprachen
\visible<3->{
\begin{code}
type Lens sigma alpha = sigma -> alpha
\end{code}
}
\item put-basierte Sprachen
\visible<4>{
\begin{code}
type Lens sigma alpha = sigma -> alpha -> sigma
\end{code}
}
\end{itemize}
\end{frame}

\note{
\begin{code}
(<<<) :: PPrinter a -> PPrinter () -> PPrinter a
(pA <<< pB) str (expr,str') = (pA <> pB) str ((expr,()),str')
\end{code}
\begin{code}
(>>>) :: PPrinter () -> PPrinter b -> PPrinter b
(pA >>> pB) str (expr,str') = (pA <> pB) str (((),expr),str')
\end{code}
}

\note{
\begin{flushleft}
\begin{code}
type PPrinter a = Lens String (a,String)
\end{code}
\begin{code}
ppNum :: PPrinter Int
ppNum _ (d,str') | d <= 9 && d >= 0 = show d ++ str'
\end{code}
\uncover<2->{
\begin{code}
(<>) :: PPrinter a -> PPrinter b -> PPrinter (a,b)
(pA <> pB) str ((expr1,expr2),str') = pA str (expr1, newStr)
  where newStr = pB str (expr2,str')
\end{code}
}
\uncover<3->{
\begin{code}
(<<<) :: PPrinter a -> PPrinter () -> PPrinter a
(pA <<< pB)
\end{code}}
\uncover<4>{
\begin{code}
(>>>) :: PPrinter () -> PPrinter b -> PPrinter b
(pA >>> pB)
\end{code}}
\end{flushleft}
}


\begin{frame}{Nicht-deterministische Linsen-Gesetze}
\begin{minipage}{0.49\textwidth}
\visible<3->{
\begin{code}
all  (\v -> put s v == s)
     (get s)
\end{code}
}
\vspace{1.9cm}
\visible<6->{
\begin{code}
all  (\s -> get s == v)
     (put s v)
\end{code}
}
\end{minipage}
\begin{minipage}{0.49\textwidth}
\vspace{0.25cm}

\includegraphics<1>[width=\textwidth]{images/nondetPutGet1.pdf}
\includegraphics<2->[width=\textwidth]{images/nondetPutGet2.pdf}

\vspace{0.75cm}

\visible<4->{
\includegraphics<1-4>[width=\textwidth]{images/nondetGetPut1.pdf}
\includegraphics<5->[width=\textwidth]{images/nondetGetPut2.pdf}
}
\end{minipage}
\end{frame}

\note{
\begin{itemize}
\item eingebaute Suche ausnutzen
  \begin{itemize}
  \item Komplikation bei (zu) strikten Funktionsdefinitionen
  \end{itemize}
\item Verhalten bzgl. Nicht-determinismus untersuchen
  \begin{itemize}
  \item Ist es sinnvoll die Gesetze umzuformulieren?
  \item Kann man nichtdeterministische Linsen dennoch sinnvoll einsetzen?
  \end{itemize}
\end{itemize}
}

\note{
\begin{code}
checkPutGetND :: Lens a b -> a -> b -> [Test]
checkPutGetND lens x y = not (null (putND lens x y))
                           ==> all (y `elem`) getVs <~> True
 where
  putVs = putND lens x y
  getVs = map (getND lens) putVs

checkGetPutND :: Lens a b -> a -> [Test]
checkGetPutND lens x = not (null (getND lens x))
                         ==> all (x `elem`) putVs <~> True
 where
  putVs = map (putND lens x) getVs
  getVs = getND lens x 
\end{code}
}

%
% Zusammenfassung
%


\begin{frame}{Was bisher geschah}
\begin{itemize}
\item Implementierungen
\begin{itemize}
\item Kombinator-Bibliothek
\item put-basierte Bibliothek
\end{itemize}
\item Vielzahl von Linsen-Primitiven und -Definitionen
\item Untersuchung von (einfachen) nicht-deterministischen Linsen
\item Anwendungen
\begin{itemize}
\item Datenstrukturprojektionen im Web-Framework Spicey
\item \alt<2>{Show/Read}{Printer/Parser}
\end{itemize}
\end{itemize}
\end{frame}

\begin{frame}{Im n\"achsten Vortrag}
\begin{itemize}
\item verbesserter Printer/Parser
\item sinnvolle nicht-deterministische Linsen
\item Erweiterung der Linsen-Gesetze bzgl. Nicht-determinismus
\end{itemize}

\begin{itemize}
\item Curry-spezifische Probleme bei der Suche nach `get`
\item statische Analysen bzgl. Wohldefiniertheit
\end{itemize}
\end{frame}

\end{document}
