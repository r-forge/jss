\name{get_orcidlink}
\alias{get_orcidlink}

\title{Obtain ORCID Links for Authors}

\description{
Query \url{https://orcid.org} in order to obtain orcidlinks for
all authors in a JSS paper.
}

\usage{
get_orcidlink(person = NULL, rows = 1L, verbose = TRUE, ...)
}

\arguments{
  \item{person}{character, \code{\link[utils]{person}} object, or
    \code{\link[jss]{jss}} object. In case of a character it can either
    be the file name of a JSS-style LaTeX file (by default the first
    LaTeX file in the current working directory is used) or person names
    (optionally enhanced by ORCID search strings).}
  \item{rows}{integer. The number of search results for each author.}
  \item{verbose}{logical. Should results be printed to the screen
    (and then returned invisibly)?}
  \item{...}{further arguments passed to \code{\link[rorcid]{orcid}}.}
}

\details{
The \code{get_orcidlink} function is a utility to automatically search
possible ORCID links for each author in a JSS paper. By default it reads
the first LaTeX file in the current directory (assuming it is in JSS style),
extracts the author names, and then uses \code{\link[rorcid]{orcid}} to
query \url{https://orcid.org/} for their possible ORCIDs.

All search results should be checked carefully because the chance to
obtain an erroneous ORCID result is quite high if an author does not
have an ORCID.
}

\value{
\code{get_orcidlink} returns a character matrix with the author names,
ORCID links in LaTeX format, and ORCID URLs. The matrix is returned
invisible if \code{verbose = TRUE} (the default).
}

\seealso{
  \code{\link[jss]{jss}}, \code{\link[rorcid]{orcid}}
}

\examples{
\dontrun{
get_orcidlink("Achim Zeileis")
}
}

\keyword{utilities}
