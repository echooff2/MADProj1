%%
%% ijSmartGrid.cls
%% LaTeX Class for International Journal on Smart Grid
%% Matches: ijSmartGridAcceptedPaperTemplate_modified_16-03-2026.docx
%%
%% Font:    Times New Roman (newtxtext + newtxmath)
%% Paper:   A4, margins: top/bottom 24mm, left/right 15mm
%% Layout:  Single column, 10pt body text
%%

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{ijSmartGrid}[2026/03/29 v1.0 International Journal on Smart Grid]

%% ============================================================
%%  BASE CLASS
%% ============================================================
\LoadClass[10pt,a4paper]{article}

%% ============================================================
%%  REQUIRED PACKAGES
%% ============================================================

%% Page geometry
%% top=24mm: headheight(40pt≈14mm) + headsep(4pt≈1.5mm) + üst boşluk(~8.5mm)
\RequirePackage[a4paper,
  top=24mm, bottom=24mm,
  left=15mm, right=15mm,
  includeheadfoot]{geometry}

%% Times New Roman for text and math
\RequirePackage[T1]{fontenc}
\RequirePackage[utf8]{inputenc}
\RequirePackage{newtxtext}      % Times New Roman text
\RequirePackage{newtxmath}      % Times New Roman math

%% Typography
\RequirePackage{microtype}      % Improved text rendering

%% Spacing
\RequirePackage{setspace}
\RequirePackage[polish]{babel}

%% Section heading formatting
\RequirePackage{titlesec}

%% Figure and table captions
\RequirePackage{caption}

%% Headers and footers
\RequirePackage{fancyhdr}

%% Math
%% NOT: amssymb buraya EKLENMEZ -- newtxmath zaten tüm AMS sembollerini
%% içerir. İkisi birlikte yüklenirse \Bbbk çakışması hatası oluşur.
\RequirePackage{amsmath}

%% Figures
\RequirePackage{graphicx}

%% Tables
\RequirePackage{booktabs}
\RequirePackage{array}

%% Numbered citations [1], [2,3], ...
\RequirePackage{cite}

%% URL support for references
\RequirePackage{url}

%% ORCID iD sembolü + tıklanabilir link
%% Kullanım: \orcidlink{0000-0002-9735-5697}
%% hyperref yüklüyse otomatik link oluşturur, yoksa sadece ikon gösterir.
\RequirePackage{orcidlink}

%% Arial font (sans-serif) -- header metinleri için
%% Overleaf/TeX Live'da Arial = Helvetica klonu (phv)
\RequirePackage[scaled]{helvet}   % \sffamily -> Helvetica/Arial

%% Logo şeffaflığı için tikz
\RequirePackage{tikz}

%% ============================================================
%%  PARAGRAPH FORMATTING
%% ============================================================
\setlength{\parindent}{0.63cm}   % First-line indent (180 twips)
\setlength{\parskip}{6pt}        % Space between paragraphs (120 twips)
\singlespacing                   % Single line spacing

%% ============================================================
%%  SECTION HEADINGS
%%  H1  -> 10pt, Bold,           numbered "1."
%%  H2  -> 10pt, Bold + Italic,  numbered "1.1."
%%  H3  -> 10pt, Italic,         numbered "1.1.1."
%% ============================================================
\titleformat{\section}
  {\normalfont\normalsize\bfseries}
  {\thesection.}{0.5em}{}
\titlespacing*{\section}{0pt}{6pt}{6pt}

\titleformat{\subsection}
  {\normalfont\normalsize\bfseries\itshape}
  {\thesubsection.}{0.5em}{}
\titlespacing*{\subsection}{0pt}{6pt}{6pt}

\titleformat{\subsubsection}
  {\normalfont\normalsize\itshape}
  {\thesubsubsection.}{0.5em}{}
\titlespacing*{\subsubsection}{0pt}{6pt}{6pt}

%% ============================================================
%%  CAPTION FORMATTING
%%  Figures : "Fig. 1. Caption text."   -- below figure, centered
%%  Tables  : "Table 1. Caption text."  -- above table, centered
%% ============================================================
\DeclareCaptionLabelFormat{figlabel}{Rys.~#2}
\captionsetup[figure]{
  labelformat  = figlabel,
  labelsep     = period,
  font         = normalsize,
  justification= centering,
  position     = below,
  skip         = 6pt
}
\captionsetup[table]{
  name         = Tabela,
  labelsep     = period,
  font         = normalsize,
  justification= centering,
  position     = above,
  skip         = 6pt
}

%% ============================================================
%%  TITLE BLOCK
%%  Usage in .tex file:
%%    \title{...}
%%    \author{Author One\textsuperscript{*}, Author Two\textsuperscript{**}}
%%    \ijaffiliation{
%%      \textsuperscript{*}Department, University, Country \\
%%      \textsuperscript{**}Department, University, Country
%%    }
%%    \ijemail{first@email.com, second@email.com}
%%    \corresponding{\textsuperscript{\dag} Corresponding Author: Name,
%%                   Affiliation, email@address.com}
%%    \receivedaccepted{dd.mm.yyyy}{dd.mm.yyyy}
%%    \maketitle
%% ============================================================

%% Internal storage for title block fields
\newcommand{\@affiliations}{}
\newcommand{\ijaffiliation}[1]{\renewcommand{\@affiliations}{#1}}

\newcommand{\@ijemail}{}
\newcommand{\ijemail}[1]{\renewcommand{\@ijemail}{#1}}

\newcommand{\@corresponding}{}
\newcommand{\corresponding}[1]{\renewcommand{\@corresponding}{#1}}

\newcommand{\@receiveddate}{}
\newcommand{\@accepteddate}{}
\newcommand{\receivedaccepted}[2]{%
  \renewcommand{\@receiveddate}{#1}%
  \renewcommand{\@accepteddate}{#2}%
}

\renewcommand{\maketitle}{%
  \begin{center}%
    %% Title: 24pt, Times New Roman, centered
    {\fontsize{24}{28}\selectfont\@title\par}%
    \vspace{6pt}%
    %% Author names: 12pt, centered (use \textsuperscript for markers)
    {\fontsize{12}{14}\selectfont\@author\par}%
    \vspace{6pt}%
    %% Affiliations: 10pt, centered
    \ifx\@affiliations\empty\else
      {\normalsize\@affiliations\par}%
      \vspace{6pt}%
    \fi
    %% E-mail addresses: 10pt, centered, wrapped in parentheses
    \ifx\@ijemail\empty\else
      {\normalsize(\@ijemail)\par}%
      \vspace{6pt}%
    \fi
    %% Corresponding author line: 10pt, centered
    \ifx\@corresponding\empty\else
      {\normalsize\@corresponding\par}%
      \vspace{6pt}%
    \fi
    %% Received / Accepted dates: 10pt, italic, centered
    \ifx\@receiveddate\empty\else
      {\normalsize\textit{Otrzymano: \@receiveddate\quad  \@accepteddate}\par}%
    \fi
  \end{center}%
  \vspace{12pt}%
  \thispagestyle{plain}%   % First page: header visible, footer (page no.) hidden
}

%% ============================================================
%%  ABSTRACT
%%  Word template format: "Abstract-" (bold) followed by text
%%  Usage: \begin{abstract} ... \end{abstract}
%% ============================================================
\renewenvironment{abstract}{%
  \setlength{\parindent}{0pt}%
  \noindent\textbf{Streszczenie-}\space\ignorespaces
}{%
  \par\vspace{6pt}%
}

%% ============================================================
%%  KEYWORDS
%%  Word template format: "Keywords:" (bold) followed by list
%%  Usage: \keywords{keyword1, keyword2, keyword3}
%% ============================================================
\newcommand{\keywords}[1]{%
  \noindent\textbf{Słowa kluczowe:} #1\par
  \vspace{12pt}%
}

%% ============================================================
%%  RUNNING HEADER METADATA
%%  Set these commands in the preamble of your .tex file.
%%
%%  \ijshortauthor{F.~Author et al.}  -- abbreviated author list
%%  \ijvolume{x}                       -- journal volume number
%%  \ijissue{x}                        -- journal issue number
%%  \ijpubmonth{Month}                 -- publication month (e.g. March)
%%  \ijpubyear{xxxx}                   -- publication year
%%  \ijarticletype{Research Article}   -- Research Article / Review Article
%% ============================================================

\newcommand{\@ijshortauthor}{F.~Author et al.}
\newcommand{\ijshortauthor}[1]{\renewcommand{\@ijshortauthor}{#1}}

\newcommand{\@ijvolume}{x}
\newcommand{\ijvolume}[1]{\renewcommand{\@ijvolume}{#1}}

\newcommand{\@ijissue}{x}
\newcommand{\ijissue}[1]{\renewcommand{\@ijissue}{#1}}

\newcommand{\@ijpubmonth}{Month}
\newcommand{\ijpubmonth}[1]{\renewcommand{\@ijpubmonth}{#1}}

\newcommand{\@ijpubyear}{xxxx}
\newcommand{\ijpubyear}[1]{\renewcommand{\@ijpubyear}{#1}}

\newcommand{\@ijarticletype}{Research Article}
\newcommand{\ijarticletype}[1]{\renewcommand{\@ijarticletype}{#1}}

%% ============================================================
%%  HEADERS AND FOOTERS
%%
%%  LAYOUT (Word şablonuyla birebir):
%%
%%  ┌────────────────────────────────────────────────────────┐
%%  │ [LOGO] │ INTERNATIONAL JOURNAL of SMART GRID (bold)   │
%%  │        │ F. Author et al., Vol. x, No. x, Month, xxxx │
%%  │        │ |Research or Review Article|                  │
%%  ├────────────────────────────────────────────────────────┤
%%  │ (metin alanı)                              [Sayfa No.] │
%%  └────────────────────────────────────────────────────────┘
%%
%%  Birinci sayfada: header görünür, sayfa numarası YOK
%%
%%  Overleaf: logoIJSG.jpg proje kök klasörüne yüklenmeli.
%% ============================================================

%% headheight = 40pt: 3 satır \small(9pt) + 2x baselineskip(12pt) = 33pt + tolerans
%% headsep   = 4pt : kural çizgisi ile metin arası minimum boşluk
\setlength{\headheight}{40pt}
\setlength{\headsep}{4pt}

%% Header içeriği: logo + dergi bilgisi yan yana, [c] ile dikey ortalı.
%% \raisebox KULLANILMAZ -- fancyhdr içeriği doğal yüksekliğiyle yerleştirir.
\newcommand{\@ijfullheader}{%
  %% Logo kutusu: dikey ortalı, büyütülmüş + %80 opaklık (hafif transparan)
  \begin{minipage}[c]{1.5cm}%
    \begin{tikzpicture}%
      \node[opacity=0.80,inner sep=0pt]%
        {\includegraphics[height=1.25cm]{sggw_logo}};%
    \end{tikzpicture}%
  \end{minipage}%
  \hspace{8pt}%
  %% Dergi bilgisi: dikey ortalı, satır aralığı sıkı, Arial font
  \begin{minipage}[c]{\dimexpr\textwidth-1.5cm-12pt\relax}%
    \setlength{\baselineskip}{12pt}%
    \setlength{\parskip}{0pt}%
    	%{\small\sffamily Szkoła Główna Gospodarstwa Wiejskiego w Warszawie}\\%
	%{\small\sffamily Wydział Zastosowań Informatyki i Matematyki}\\%
	{\small\sffamily Klasyfikacja meczów League of Legends przy ogra-}\\%
	{\small\sffamily niczonych danych z pierwszych 10 minut meczu}\\%
    \makebox[\linewidth][r]{\small\sffamily|\@ijarticletype|}%
  \end{minipage}%
}

%% Normal sayfa stili (sayfa 2'den itibaren)
\pagestyle{fancy}
\fancyhf{}
\renewcommand{\headrulewidth}{0.4pt}
\fancyhead[L]{\@ijfullheader}   %% Tam genişlik header sol'dan başlar
\fancyhead[C]{}
\fancyhead[R]{}
\fancyfoot[R]{\small\thepage}   %% Sayfa numarası: sağ alt

%% Birinci sayfa: header görünür, sayfa numarası YOK
\fancypagestyle{plain}{%
  \fancyhf{}%
  \renewcommand{\headrulewidth}{0.4pt}%
  \fancyhead[L]{\@ijfullheader}%
  \fancyhead[C]{}%
  \fancyhead[R]{}%
  %% fancyfoot boş -- birinci sayfada sayfa numarası görünmez
}

%% ============================================================
%%  EQUATION REFERENCE SHORTHAND
%%  In-text: \eqref{label} -> (1)  [standard amsmath]
%%  Sentence start: Eq.~(\ref{label})
%% ============================================================
%% Already provided by amsmath

%% ============================================================
%%  STARRED SECTIONS (no numbering)
%%  Acknowledgements, Author Contributions, Conflict of Interest
%%  -- these are formatted exactly like \section but without number
%% ============================================================
%% Just use \section*{Acknowledgements} etc. in the .tex file
%% titlesec applies the same font (bold, 10pt) automatically

%% ============================================================
%%  BIBLIOGRAPHY
%%  Style: [1] numbered, in order of citation (ieeetr or unsrt)
%%  Use \bibliographystyle{ieeetr} in the .tex file
%% ============================================================

%% End of class
\endinput