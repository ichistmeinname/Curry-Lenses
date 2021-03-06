\documentclass[%
	latex,%              PDFTex verwenden
	a4paper,%             A4 Papier
	twoside,%             Einseitig
        openright,%           freilassen der Seite
	chapterprefix,%       Kapitel anschreiben als Kapitel
	headsepline,%         Linie nach Kopfzeile
	%footsepline,%         Linie vor Fusszeile
	%pointlessnumbers,%     Nummern ohne abschließenden Punkt
	10pt
]{scrbook}

\usepackage[nottoc]{tocbibind}

%
% Paket für Übersetzungen ins Deutsche
%
\usepackage[english]{babel}

%
% for citations to start a chapter
%
\usepackage{epigraph}
\renewcommand{\epigraphsize}{\small}
\setlength\beforeepigraphskip{-0.5cm}
\setlength\epigraphwidth{8cm}

%
% figures over several pages
%
\usepackage{subcaption}

\usepackage{todonotes}

% Paket fuer Inferenzsysteme
\usepackage{bussproofs}
\EnableBpAbbreviations

\usepackage{cite}
\usepackage[round]{natbib}
\usepackage{url}
%
% Pakete um Latin1 Zeichnensätze verwenden zu können und die dazu
% passenden Schriften.
%
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}

%
% Paket zum Erweitern der Tabelleneigenschaften
%
\usepackage{array}
\usepackage{multirow}

%
% needed for colorcode.fmt
%
%% \usepackage{colortbl}
%% \usepackage{calc}

%
% Paket für schönere Tabellen
%
\usepackage{booktabs}

%
% Paket um Grafiken einbetten zu können
%
\usepackage{graphicx} 
%
% Spezielle Schrift im Koma-Script setzen.
%
\usepackage{titlesec}
\newcommand*{\justifyheading}{\raggedleft}
\titleformat{\chapter}[display]
  {\normalfont\Huge\bfseries\justifyheading}{\thechapter}
  {10pt}{\Huge}
\titleformat{\section}
  {\normalfont\Large\bfseries}{\thesection}{1em}{}
\titleformat{\subsection}
  {\normalfont\large\bfseries}{\thesubsection}{1em}{}
% \setkomafont{sectioning}{\normalfont\bfseries}
\setkomafont{captionlabel}{\normalfont\bfseries} 
\setkomafont{pagehead}{\normalfont\bfseries} % Kopfzeilenschrift
\setkomafont{descriptionlabel}{\normalfont\bfseries}

%
% Zeilenumbruch bei Bildbeschreibungen.
%
\setcapindent{1em}

%
% Kopf und Fußzeilen
%
\usepackage{scrpage2}
\pagestyle{scrheadings}
% Inhalt bis Section rechts und Chapter links
\automark[section]{chapter}
% Mitte: leer
\chead{}

%
% mathematische symbole aus dem AMS Paket.
%
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{amsthm}
\usepackage{latexsym}
% for prim in lhs-format
\usepackage{ textcomp }

%
% Paket für Farben im PDF
%
\usepackage{color}

%
% Paket für Links innerhalb des PDF Dokuments
%
\definecolor{LinkColor}{rgb}{0,0,0.5}
\usepackage[%
	pdftitle={Lenses and Bidirectional Programming in Curry},% Titel der Diplomarbeit
	pdfauthor={Sandra Dylus},% Autor(en)
	pdfcreator={LaTeX, LaTeX with hyperref and KOMA-Script},% Genutzte Programme
	pdfsubject={Master thesis}, % Betreff
	pdfkeywords={Curry, Lenses, Bidirectional Programming}]{hyperref} % Keywords halt :-)
\hypersetup{colorlinks=true,% Definition der Links im PDF File
	linkcolor=LinkColor,%
	citecolor=LinkColor,%
	filecolor=LinkColor,%
	menucolor=LinkColor,%
	pagecolor=LinkColor,%
	urlcolor=LinkColor}

%
% Paket um LIstings sauber zu formatieren.
%
\usepackage[savemem]{listings}
\lstloadlanguages{TeX}

%
% aller Bilder werden im Unterverzeichnis figures gesucht:
%
\graphicspath{{../images/}}

%
% Literaturverzeichnis-Stil
%

%
% Anführungsstriche mithilfe von \textss{-anzufuehrendes-}
%
\newcommand{\textss}[1]{"#1"}

%
% Strukturiertiefe bis subsubsection{} möglich
%
\setcounter{secnumdepth}{3}

%
% Dargestellte Strukturiertiefe im Inhaltsverzeichnis
%
\setcounter{tocdepth}{1}

%
% Zeilenabstand wird um den Faktor 1.25 verändert
%
\renewcommand{\baselinestretch}{1.25}

\bibliographystyle{plainnat}
