\name{as.bibentry}
\alias{as.bibentry}
\alias{as.bibentry.jss}

\title{Coercion to Bibentry Objects}

\description{
Generic function and method for coercion to objects
of class \code{"bibentry"}.
}

\usage{
as.bibentry(x, \dots)

\method{as.bibentry}{jss}(x, keytype = c("number", "author"), doi = TRUE, url = !doi, ...)
}

\arguments{
  \item{x}{an object (of class \code{"jss"}).}
  \item{keytype}{character specifying which type of bib key should be generated.}
  \item{doi}{logical. Should the DOI be included in the bibentry?}
  \item{url}{logical. Should the URL be included in the bibentry?}
  \item{\dots}{arguments passed to methods.}
}

\details{
Currently only a method for objects of class \code{"jss"} is provided.
}

\value{
An object of class \code{"bibentry"}.
}

\seealso{
  \code{\link[jss]{jss}},
  \code{\link[utils]{bibentry}}
}

\keyword{utilities}
