\name{fix_bib}
\alias{fix_bib}
\alias{bibtool}
\alias{shortcites}
\alias{get_doi}
\alias{add_doi}

\title{Fix BibTeX Files According to JSS Style Guidelines}

\description{
Utilities for processing BibTeX files, adding DOIs etc.,
especially for publications in the Journal of Statistical Software.
}

\usage{
fix_bib(x, file = x, orig = "_orig.bib", bibtool = TRUE, doi = TRUE, shortcites = TRUE)

bibtool(tex = NULL, temp = "_foo.bib", orig = "_orig.bib")

shortcites(x, maxlength = 6)

get_doi(x, minscore = 1.5, type = NULL)
add_doi(x, file = "out.bib", minscore = 1.5)
}

\arguments{
  \item{x}{character or bibentry object. If a character is supplied
    it is read into a bibentry object using \code{\link[bibtex]{read.bib}}.}
  \item{file}{character for output BibTeX file name.}
  \item{orig}{character with file name for copy of the original
    BibTeX file (if any).}
  \item{bibtool}{logical. Should \code{bibtool} be run first (usually
    to remove bibentries that are not cited)?}
  \item{doi}{logical. Should DOIs be added using \code{get_doi} (based
    on \code{\link[rcrossref]{cr_works}})?}
  \item{shortcites}{logical. Should bibentries with a long author list
    be reported in a shortcites statement?}
  \item{tex}{character specifying the input LaTeX file.}
  \item{temp}{character with file name for the temporary copy
    of the BibTeX file (while running \code{bibtool}).}
  \item{maxlength}{numeric. Maximum number of authors for a
    normal cite (that does not have to be shortened).}
  \item{minscore}{numeric. Minimum score that needs to be achieved
    when querying a DOI.}
  \item{type}{character. The reference type (e.g., \code{"journal-article"}
    or \code{"book"}) that needs to be matched.}
}

\details{
The \code{fix_bib} function is a utility to automatically bring a .bib
file almost into JSS style. By default, it proceeds by reading a .bib file
using \code{\link[bibtex]{read.bib}}, omitting unused entries using
\code{bibtool}, modifying author names, titles, journals, etc. if necessary,
adding DOIs for articles and books using \code{\link[rcrossref]{cr_works}},
and writing the result again to the same .bib file.

Some steps can be switched off by setting the corresponding arguments.
Standalone functions for these steps are also provided.
}

\value{
\code{fix_bib} and \code{add_doi} return an object of class \code{"bibentry"}
(invisibly if an output file is specified).

\code{bibtool} returns a logical, only indicating whether the file renaming worked.

\code{shortcites} and \code{get_doi} return character vectors with BibTeX keys
and DOIs, respectively.
}

\seealso{
  \code{\link[jss]{jss}}
}

\examples{
bibfile <- system.file("bib", "example.bib", package = "jss")

if(require("bibtex")) {

## read BibTeX entry as currently stored
bde1 <- read.bib(bibfile)
print(bde1)
toBibtex(bde1)

## fix BibTeX entry
## - but do not overwrite file
## - running bibtool is not necessary (and also not possible as there is no .tex file)
## - querying the DOI is switched off (just to save processing time in this example)
bde2 <- fix_bib(bibfile, file = FALSE, bibtool = FALSE, doi = FALSE)
print(bde2)
toBibtex(bde2)
## This has
## - a capitalized title,
## - correctly spaces the middle initials,
## - unifies the TAS journal name,
## - and the DOI could be added by setting doi = TRUE or manually queried via
## get_doi(bde2)
}
}

\keyword{utilities}
