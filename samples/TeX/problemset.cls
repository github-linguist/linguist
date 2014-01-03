% =====================================
% problemset Document Style
%   For Problem Sets
%
% Options:
% final      hides to-dos
% worksheet  hides solutions and places each problem on separate page
% expand     places each problem on a separate page
% =====================================

\ProvidesClass{problemset}
\DeclareOption*{\PassOptionsToClass{final}{article}}
\DeclareOption{worksheet}{\providecommand{\@solutionvis}{0}}
\DeclareOption{expand}{\providecommand{\@expand}{1}}
\ProcessOptions\relax

% ================== Packages and Document Options ==================
\LoadClass[10pt,letterpaper]{article}
\RequirePackage[%
        top=0.85in,
        bottom=1in,
        left=1in,
        right=1in
        ]{geometry}
\RequirePackage{pgfkeys}    % For mathtable environment.
\RequirePackage{tabularx}   % For pset heading
\RequirePackage{float}      % Used for floats (tables, figures, etc.)
\RequirePackage{graphicx}   % Used for inserting images.
\RequirePackage{enumerate}  % Used for the enumerate environment.
\RequirePackage{mathtools}  % Required. Loads amsmath.
\RequirePackage{amsthm}     % Required. Used for theorem environments.
\RequirePackage{amssymb}    % Required.
\RequirePackage{booktabs}   % Required. Used for mathtable environment.
\RequirePackage{esdiff}     % For derivatives and partial derivatives
\RequirePackage{mathtools}  % Optional. Used for \shortintertext.
\RequirePackage{fancyhdr}   % Required. For customizing headers/footers.
\RequirePackage{lastpage}   % Required. For page count in header/footer.
\RequirePackage{xcolor}     % Required. For setting the color of hyperlinks
\RequirePackage[%
        obeyFinal,          % Disable todos by setting [final] option for class
        color=@todoclr,
        linecolor=red
        ]{todonotes}        % For keeping track of to-dos.
\RequirePackage[%
        colorlinks=true,
        linkcolor=navy,
        urlcolor=black
        ]{hyperref}         % For following urls and references in a document.
\RequirePackage{url}        % Enables urls with the \url tag
\RequirePackage[all]{hypcap}
% hypcap: Links go to object instead of caption. [Keep as last package]

% ==================   Appearance Settings   ==================
\definecolor{@todoclr}{gray}{0.80}   % For To-Dos. 50% brightness
\definecolor{navy}{RGB}{0,0,150}     % For coloring hyperlinks
\setlength{\parskip}{1.5ex}          % Sets space between paragraphs.
\setlength{\parindent}{0pt}          % Indent for first line of new paragraphs.

% Smaller verbatim type size
\let\VERBATIM\verbatim
\def\verbatim{%
  \def\verbatim@font{\small\ttfamily}%
\VERBATIM}

% =============   Caption Modifications   =============
\usepackage[small]{caption}
\usepackage[footnotesize]{subcaption}
    % For no visible number, use: \caption*{Unnumbered figure caption.}
\captionsetup[table]{labelformat=simple, labelsep=period, labelfont=bf}
\captionsetup[figure]{labelformat=simple, labelsep=period, labelfont=bf}
\captionsetup[subtable]{labelformat=parens, labelsep=space, labelfont=bf}
\captionsetup[subfigure]{labelformat=simple, labelsep=period, labelfont=bf}

% ==================     Booleans     ==================
\def\TRUE{1}
\def\FALSE{0}
\def\SHOW{1}
\def\HIDE{0}

% ============= Gets Document Info, Generates Heading =============
\providecommand{\heading}[5][]{
  \thispagestyle{empty}
  \listoftodos
  \clearpage
  \pagenumbering{arabic}
  %
  \providecommand{\shortname}{#1}
  \providecommand{\authorname}{#2}
  \providecommand{\coursename}{#3}
  \providecommand{\assignment}{#4}
  \providecommand{\duedate}{#5}
  \begin{minipage}{0.5\textwidth}
    \begin{flushleft}
      \hypertarget{@assignment}{
        \textbf{\assignment}
      }\\
      \authorname
    \end{flushleft}
  \end{minipage}
  \begin{minipage}{0.5\textwidth}
    \begin{flushright}
      \coursename\\
      \duedate\\
    \end{flushright}
  \end{minipage}
  \thispagestyle{empty}
}

% ============= Headers and Footers =============
\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{0.5pt}
\pagestyle{fancyplain}
\fancyhf{}
\lfoot{%
\fancyplain{}{%
  \hyperlink{@assignment}{%
    \small{%
      \color{black}{%
        \assignment
      }
    }
  }
}
}
\cfoot{%
\fancyplain{}{%
    \small{%
      \coursename
    }
}
}
\rfoot{%
\fancyplain{}{%
  \small{\shortname~\thepage~of~\pageref{LastPage}}
}
}

% ============= Problem Command =============
% INPUT: Points for question [#1] (Optional)
\newcounter{theproblem} % Problem counter for environment

\providecommand{\problem}[1][]{%
  \addtocounter{theproblem}{1}%
  \setcounter{table}{0}%
  \setcounter{figure}{0}%
  \setcounter{equation}{0}%
  \noindent%
  \textbf{%
      Problem~\arabic{theproblem}.}~\textit{\small{#1}}

}

% ============= QED, Page Breaks After QED? =============
\providecommand{\@expand}{\HIDE}  % Default is to omit pagebreaks after the solution
\providecommand{\qqed}{\hfill\rule{2mm}{2mm}\ifnum\@expand=\SHOW\\\pagebreak\fi}


% ============= Solution Command =============
\providecommand{\@solutionvis}{1}  % Default setting is to show solutions.
\providecommand{\solution}[2][\@solutionvis]{
\vspace{0.5em}\noindent\textbf{Solution.}
\ifnum#1=\SHOW%
#2

\hfill\qqed\vspace{0.1em}
\else%
\pagebreak%
\fi
}

% ============= Chapter, Section, Item Commands =============
\providecommand{\chap}[2][0]{
\ifnum#1=0%
\else%
\setcounter{section}{#1}%
\addtocounter{section}{-1}%
\fi%
\vspace{-1.75em}%
\section{#2}
}

\providecommand{\sect}[2][0]{
\ifnum#1=0%
\else%
\setcounter{subsection}{#1}%
\addtocounter{subsection}{-1}%
\fi%
\vspace{-0.5em}%
\subsection{#2}
}

\providecommand{\subsect}[1]{\noindent\textbf{#1.}}

% ============= Insert Non-Float Image =============
\providecommand{\insertgraphic}[2][0.5\textwidth]{
\vspace{-1em}
\begin{center}
  \includegraphics[width=#1]{#2}
\end{center}
\vspace{-1em}
}



% ============= Object Numbering by Problem =============
\renewcommand{\thetable}{\arabic{theproblem}.\arabic{table}}
\renewcommand{\thefigure}{\arabic{theproblem}.\arabic{figure}}
\renewcommand{\theequation}{\arabic{theproblem}.\arabic{equation}}


% ============= Formula Environment =============
\newcounter{formula}
\newenvironment{formula}[1][Formula \arabic{formula}]
{
  \addtocounter{formula}{1}
  \begin{displaymath}
  \tag*{\parbox{5em}{\textbf{\small{#1}}}}
}{
  \end{displaymath}\\
}

% ============= Math Table =============
\newif\ifcaption
\pgfkeys
{
  /mypkg/title/.store in=\Caption,% Any value assigned to title will be stored in \Caption
  /mypkg/title= ,              % Initialize so \Caption exists
   /mypkg/label/.store in=\Label,  % Any value assigned to label will be stored in \Label
   /mypkg/label= ,              % Initialize so \Label exists
   /mypkg/caption/.is if=caption,  % Declare a boolean, defaults to false
}
\newenvironment{mathtable}[2][]{
  \pgfkeys{/mypkg/.cd, #1}%
  \vspace{-1em}%
  \begin{table}[ht!]%
  \small \begin{center}%
  \begin{displaymath}%
  \begin{array}{#2}%
  \toprule
}{
  \bottomrule
  \end{array}%
  \end{displaymath}%
  \ifcaption%
  \vspace{-1em}\caption{\Caption}\label{\Label}%
  \fi%
  \end{center}%
  \end{table}%
  \vspace{-1em}%
}


% ============= Double-line Column-Heading for table =============
\providecommand{\double}[2]{%
  \multicolumn{1}{c}{%
    \genfrac{}{}{0pt}{}{\text{#1}}{\text{#2}}
  }
}


% ============= Theorem-Style Environments =============
\theoremstyle{plain}          % Bold label, italic letters
\newtheorem{thm}{Theorem}[section]    % Numbered by section
\newtheorem{lma}[thm]{Lemma}
\newtheorem{crl}[thm]{Corollary}
\newtheorem{prp}[thm]{Proposition}
\newtheorem{cnj}[thm]{Conjecture}
\newtheorem{alg}[thm]{Algorithm}

% Associated environments (for numbered theorem environments)
\newenvironment{theorem}[2][]{\begin{thm}[#1]\label{#2}}{\end{thm}}
\newenvironment{lemma}[2][]{\begin{lma}[#1]\label{#2}}{\end{lma}}
\newenvironment{corollary}[2][]{\begin{crl}[#1]\label{#2}}{\end{thm}}
\newenvironment{proposition}[2][]{\begin{prp}[#1]\label{#2}}{\end{crl}}
\newenvironment{conjecture}[2][]{\begin{cnj}[#1]\label{#2}}{\end{cnj}}
\newenvironment{algorithm}[2][]{\begin{alg}[#1]\label{#2}}{\end{alg}}

\theoremstyle{remark}          % Italic label, roman letters
\newtheorem{rmk}{Remark}[section]    % Numbered by section. Remarks are used to expand on and integrate material.
\newtheorem*{note}{Note}        % Un-numbered. Notes are used to comment on specific elements of the material.
\newtheorem*{caveat}{Caveat}      % Un-numbered. Caveats are used to guide the reader away from a common error.
\newtheorem*{warning}{Warning}      % Un-numbered. Warnings are used to guide away from especially egregious errors.

\theoremstyle{definition}        % Bold label, roman letters
\newtheorem{dfn}{Definition}[section]  % Numbered by section. Definitions of concepts and terms.
\newtheorem{exm}{Example}[section]    % Numbered by section. Illustrative examples.
\newtheorem{smm}{Summary}[subsection]  % Numbered by subsection. For section summaries.
\newtheorem*{question}{Question}    % Un-numbered. For questions to motivate further analysis.
\newtheorem*{speculation}{Speculation}  % Un-numbered. For questions that arise but will not be immediately answered.

% Associated environments (for numbered theorem environments)
\newenvironment{remark}[2][]{\begin{rmk}[#1]\label{#2}}{\end{rmk}}
\newenvironment{definition}[2][]{\begin{dfn}[#1]\label{#2}}{\end{dfn}}
\newenvironment{example}[2][]{\begin{exm}[#1]\label{#2}}{\end{exm}}
\newenvironment{summary}[2][]{\begin{smm}[#1]\label{#2}}{\end{smm}}


% ============= Greek Letters =============
\renewcommand{\a}{\ensuremath{\alpha}}
\renewcommand{\b}{\ensuremath{\beta}}
\renewcommand{\c}{\ensuremath{\gamma}}
\newcommand{\ch}{\ensuremath{\chi}}
\renewcommand{\d}{\ensuremath{\delta}}
\newcommand{\ep}{\ensuremath{\epsilon}}
\newcommand{\et}{\ensuremath{\eta}}
\newcommand{\ve}{\ensuremath{\varepsilon}}
\renewcommand{\r}{\ensuremath{\rho}}
\newcommand{\s}{\ensuremath{\sigma}}
\renewcommand{\t}{\ensuremath{\tau}}
\newcommand{\f}{\ensuremath{\psi}}
\newcommand{\w}{\ensuremath{\omega}}
\newcommand{\h}{\ensuremath{\phi}}
\newcommand{\m}{\ensuremath{\mu}}
\renewcommand{\l}{\ensuremath{\lambda}}
\renewcommand{\k}{\ensuremath{\kappa}}
\renewcommand{\v}{\ensuremath{\nu}}
\renewcommand{\i}{\ensuremath{\iota}}
\renewcommand{\o}{\ensuremath{\theta}}
\newcommand{\z}{\ensuremath{\zeta}}

% ============= Mathematical Symbols =============
\providecommand{\NN}{\ensuremath{\mathbb{N}}}
\providecommand{\ZZ}{\ensuremath{\mathbb{Z}}}
\providecommand{\QQ}{\ensuremath{\mathbb{Q}}}
\providecommand{\RR}{\ensuremath{\mathbb{R}}}
\providecommand{\CC}{\ensuremath{\mathbb{C}}}
\providecommand{\pd}{\partial}    % 'dee' symbol for partial derivatives
\providecommand{\dd}{\mathrm{d}} % 'dee' symbol for ordinary derivatives
\providecommand{\x}{\times}
\providecommand{\n}{\scalebox{0.6}[1.0]{\ensuremath{-}}}



% ============= Mathematical Macros =============
\providecommand{\Sum}[3][n]{\ensuremath{\sum_{{#1}={#2}}^{#3}}}     % Sum from [n]={1}to{2}.     \Sum{1}{10}
\providecommand{\infsum}[2][n]{\ensuremath{\sum_{{#1}={#2}}^\infty}}  % Infinite sum from [n]={1}    \infsum{1}
\providecommand{\Int}[4][x]{\ensuremath{\int_{#3}^{#4}\!{#2}\,\mathrm{d}{#1}}}   % Integrate {1} from {2} to {3} with respect to [x]
\providecommand{\Lim}[3][\infty]{\ensuremath{\displaystyle \lim_{{#2}\to{#1}}\!\!{#3}}} % Take the limit from {1} to [infinity] of {3} \Lim{x}{f(x)}
\providecommand{\Frac}[2]{\ensuremath{\,^{#1}\!/\!_{#2}}}            % Slanted fraction with proper spacing. Usefule for in-line display of fractions.
\providecommand{\eval}[3]{\ensuremath{\left[ #1 \right \vert_{#2}^{#3}}}
\renewcommand{\L}{\left}              % for left-hand right-sizing
\providecommand{\R}{\right}            % for right-hand right-sizing
\providecommand{\D}{\diff}            % for writing derivatives
\providecommand{\PD}{\diffp}          % for writing partial derivatives
\providecommand{\full}{\displaystyle}      % Forces display style in math mode
\providecommand{\Deg}{\ensuremath{^\circ}}    % for adding a degree symbol, even if not in math mode
\providecommand{\abs}[1]{\left\vert #1 \right\vert}   % Absolute Value
\providecommand{\norm}[1]{\left \Vert #1 \right \Vert}  % Norm (vector magnitude)
\providecommand{\e}[1]{\ensuremath{\times 10^{#1}}}    % Scientific Notation with times symbol
\providecommand{\E}[1]{\ensuremath{10^{#1}}}  % Scientific Notation
\renewcommand{\u}[1]{\text{ #1}}        % For inserting units in Roman text
\providecommand{\mc}{\text{,}\hspace{1em}}    % For inserting comma and space into math mode
\providecommand{\mtxt}[2][]{#1\hspace{0.5em}\text{#2}\hspace{0.5em}}  % For insterting text into math mode with space on either side. Option for preceding punctuation.

% ============= Probability and Statistics =============
\providecommand{\prob}[1]{\ensuremath{P\!\left(#1\right)} }
\providecommand{\cndprb}[2]{\ensuremath{P\!\left(#1 \left\vert  #2 \right.  \right)} }
\providecommand{\cov}[1]{\ensuremath{\text{Cov}\!\left(#1\right)} }
\providecommand{\ex}[1]{\ensuremath{E\!\left[#1\right]} }

% ============= Linear Algebra =============

% Column vectors
\providecommand{\twovector}[3][r]{\left(\begin{array}{#1} #2 \\ #3\end{array}\right)}
\providecommand{\threevector}[4][r]{\left(\begin{array}{#1}  #2 \\ #3 \\ #4\end{array}\right)}
\providecommand{\fourvector}[5][r]{\left(\begin{array}{#1}  #2 \\ #3 \\ #4 \\ #5 \end{array}\right)}

% ============= Vector Calculus =============
% ------------- Susan Lea's notation ---------------
\providecommand{\vecs}[1]{\ensuremath{\vec{\bm{#1}}} }  % bolded symbol, arrow
\providecommand{\vect}[1]{\ensuremath{\vec{\textbf{#1}}} }  % bolded text, arrow
\providecommand{\unitvecs}[1]{\bm{\hat{#1}}}
\providecommand{\unitvect}[1]{\hat{\textbf{#1}}}
\providecommand{\Div}[1]{\vecs{\del} \cdot \vect{#1}}
\providecommand{\Curl}[1]{\vecs{\del} \times \vect{#1}}
\providecommand{\Grad}{\vecs{\del}}




