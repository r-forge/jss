\name{make_jss_cls}
\alias{make_jss_bst}
\alias{make_jss_cls}
\alias{make_jss_style}

\title{Generate JSS LaTeX Style Files}

\description{
Utility functions for generating JSS's BibTeX style file (bst),
LaTeX class file (cls), or a zipped archive with both files,
manual, templates, logo, and further instructions.
}

\usage{
make_jss_bst(dir = ".")
make_jss_cls(dir = ".", pdf = FALSE)
make_jss_style(dir = ".")
}

\arguments{
  \item{dir}{character specifying the output directory.}
  \item{pdf}{logical. Should the PDF documentation for the
    .cls file be generated as well?}
}

\keyword{utilities}
