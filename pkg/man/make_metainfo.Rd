\name{make_metainfo}
\alias{make_metainfo}
\alias{make_citation}
\alias{make_readme}
\alias{make_crossref}
\alias{make_ojs}
\alias{make_ojs3}
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
make_crossref(x = ".", \dots, deposit = TRUE)
make_ojs(x = ".", \dots)
make_ojs3(x = ".", \dots)
}

\arguments{
  \item{x}{an object of class \code{\link[jss]{jss}} or a character
    specifying the \code{dir} to be passed to \code{jss}.}
  \item{\dots}{arguments passed to \code{\link[jss]{format.jss}}.}
  \item{deposit}{logical. Should the metainformation be deposited with Crossref?}
}

\details{
\code{make_citation} generates a \code{CITATION} file for inclusion in R packages.

\code{make_readme} generates a \code{README.txt} file for internal use in JSS.

\code{make_metainfo} generates both files and also tries to clean up and compact
the manuscript LaTeX files.

\code{make_crossref} generates a \code{CROSSREF.xml} file for registering DOIs
with CrossRef. If \code{$HOME/.crossref} exists and contains two lines (with login id
and password, respectively), the XML is deposited automatically via \code{curl}.

\code{make_ojs} and \code{make_ojs3} generate \code{OJS.xml} files for importing a new submission
into OJS (version2) and OJS3, respectively.
}

\seealso{
  \code{\link[jss]{jss}}
}

\keyword{utilities}
