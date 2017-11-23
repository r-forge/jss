<!--- Render this file by running 
pandoc final_editing.md -f markdown -t html -s -o final_editing.html 
-->

# JSS cleanup


## General
For the following you will need the R-package `jss` and some suggested packages.  
**Installation**:

- Install bibtool

~~~
sudo apt-get install bibtool
~~~

- Install R packages in R

~~~
install.packages(c("base64enc", "bibtex", "formatR", "rcrossref", "tm", "tth"))
install.packages("jss", repos = "http://R-Forge.R-project.org")
~~~

To make rendering the PDF smooth, it is useful to store jsslogo.jpg and jss.cls
in `/texmf/tex/latex/jss` and jss.bst in `/texmf/bibtex/bst/jss`.  You get
these files from [this
link](https://www.jstatsoft.org/public/journals/1/jss-style.zip).

-------------------------------------

## Folder and Names

- Base folder name is the submission number, e.g. 12345/

- Base folder contains folders Figures/ (if any) and Code/

- Base folder contains files with names paper.tex, ref.bib

- Code/ contains file code.R (or, if package on CRAN older version than
  .tar.gz, save .tar.gz)

- paper.tex and ref.bib should be ASCII, to check this, use

~~~
file ref.bib
file paper.tex
~~~

if it does not say that the file is ASCII text, you can find
problematic lines in R:

~~~
tools::showNonASCII(readLines("ref.bib")) 
~~~

- After steps below are finished, zip base folder



-------------------------------------

## Manuscript

- Below `\documentclass`, add

~~~
\usepackage{thumbpdf,lmodern} 
\graphicspath{{Figures/}}  % folder with figures
~~~

- Change bibliography name to ref.bib

- In math formulas, use `\top` for transposing, `\dots` or `\ldots` for ...,
  and instert spaces after commas (e.g. "0, 1" instead of "0,1").

- If possible, figures should be in the following format

~~~
\begin{figure}[t!]
    \centering
    \includegraphics[...]{...}
    \caption{...}
    \label{...}
\end{figure}
~~~

- Set figure options sensibly, e.g. `width=0.8\textwidth, trim=0 20 0 20,
  clip`.  Choose width in a way that text within figure is approximately same
size as caption. If there are white margins and figure needs enlarging, use
`trim` and `clip`. Do not set `height` to keep aspact ratio.

- Before and after code, formula (equation, align) or itemization (itemize,
  enumerate) environments should be no blank line in the tex-file. This would
lead to a new paragraph in the pdf, which is usually not wanted. Fill blank
lines with `%`. 

- `\proglang` (e.g. `\proglang{S}3`), `\pkg` and `\code` have to be used for
  code highlighting (including titles and references).

- Code that could potetially be executed by the reader should be in
  CodeInput/CodeOutput or equivalently int Sinput/Soutput environments.

- Code that should not be run (e.g. `plot(x, y, ...)`) should be in `\code{}`.

- Formatting should be:

~~~
R> foo(prompt = "R> ", attention = "include spaces in code",
+    continue = "with + and then an indentation with 4 spaces",
+    max_chars_per_line = 76)
~~~

- When writing code, fill lines and use " instead of '. Make sure that spaces
  are entered after commas, e.g. `plot(x, y)` instead of `plot(x,y)`.

- Proper names should start with capital letters in text (e.g. Poisson), but
words like "mixed logit" should be lower case.

- Abbreviations should usually have capital letters (e.g. "PDF").

- `\emph` instead of `\textbf` for emphasizing

- Further clean up according to the guide on the
  [website](https://www.jstatsoft.org/about/submissions).

- Run `aspell check --lang=en_US --mode=tex paper.tex`



-------------------------------------

## Bibliography
    
- Check if all Software used is cited. If not, add `\cite`/`\citep` in
  manuscript and add respective bib-item in bib-file. For R-packages
`citation("pkgname")`/`citation("pkgname", auto = TRUE)` helps.  Some common
software citations are in software.bib.  <!--- TODO: add link here --> For CRAN
Packages: use current version, use https://CRAN (not http://CRAN), do only cite
authors [aut], if possible us official `citation()`. For other software check
website and if needed set up citation as follows: 

~~~
@Manual{,
  title = {\pkg{pkgname}: Package Title or Description},
  author = {First Author and Second Author},
  year = {2000},
  note = {\proglang{R}~package version~1.2-3},
  url = {https://CRAN.R-project.org/package=pkgname},
} 
~~~

- To clean the bib-file make sure you have bibtool installed and are connected
  to the internet and then run in R

~~~
jss::fix_bib("ref.bib")
~~~

- Check if `fix_bib()` ran without warnings and check the bib-file if it looks
  ok.  The original bib-file is saved as \_orig.bib. If you get errors, try to
fix them. Note, that if you rerun `fix_bib()` \_orig.bib will not be the
original, but the last ref.bib created.



-------------------------------------

## Code

- Tor run R code run

~~~
Rscript -e "library('knitr'); stitch('code.R')"
~~~

- To clean R code run

~~~
jss::fix_rscript("code.R")
~~~

- This should run without warnings. A copy of the previous R code is saved in
  \_orig.bib. 

- Prettify the comments.

- If feasible: Reproduce all results in Paper and check if they are the same.

- Else (e.g. due to runtime or proprietary dependencies): check for common problems
	+ Data used is missing/can't be read in
	+ Parts of the code are missing (e.g. a simulation study)
	+ No seed is set (`set.seed()`) even though the results depend on
	  something with random numbers.
