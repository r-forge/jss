\name{make_metainfo}
\alias{make_metainfo}
\alias{make_citation}
\alias{make_readme}
\alias{make_crossref}
\alias{make_jss_bib}

\title{Generate Files with Metainformation for JSS Publications}

\description{
Write selected elements of the metainformation from \code{\link[jss]{jss}}
objects into text files.
}

\usage{
make_metainfo(x = ".", \dots)
make_citation(x = ".", \dots)
make_readme(x = ".", \dots)
make_crossref(x = ".", \dots)
}

\arguments{
  \item{x}{an object of class \code{\link[jss]{jss}} or a character
    specifying the \code{dir} to be passed to \code{jss}.}
  \item{\dots}{arguments passed to \code{\link[jss]{format.jss}}.}
}

\details{
\code{make_citation} generates a \code{CITATION} file for inclusion in R packages.
\code{make_readme} generates a \code{README.txt} file for internal use in JSS.
\code{make_metainfo} generates both files and also tries to clean up and compact
the manuscript LaTeX files.
\code{make_crossref} generates a \code{CROSSREF.xml} file for registering DOIs
with CrossRef.
}

\seealso{
  \code{\link[jss]{jss}}
}

\keyword{utilities}
