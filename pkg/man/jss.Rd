\name{jss}
\alias{jss}
\alias{as.character.jss}
\alias{format.jss}

\title{Metainformation Processing for JSS Publications}

\description{
Utilities for processing the metainformation of (forthcoming) publications
in the Journal of Statistical Software (JSS).
}

\usage{
jss(dir = ".")

\method{format}{jss}(x, style = "BibTeX", \dots)
\method{as.character}{jss}(x, \dots)
}

\arguments{
  \item{dir}{character specifying the directory to a JSS publication
    along with supplementary files (if any).}
  \item{x}{an object of class \code{\link[jss]{jss}}.}
  \item{style}{character specifying the style of string
    that should be generated (BibTeX, CITATION, or README).}
  \item{\dots}{arguments passed to other functions.}
}

\details{
The function \code{jss} sets up a list with information about the
publication and corresponding supplementary files. The resulting
object of class \code{jss} can then be formatted or coerced into
different formats.
}

\value{
\code{jss} returns an object of class \code{"jss"}, i.e., a list with components as follows.
  \item{key}{character vector with two possible styles of bib keys,}
  \item{author}{character vector with LaTeX-style formatted authors,}
  \item{person}{person vector of authors,}
  \item{title}{character with simple text title,}
  \item{textitle}{character with LaTeX-formatted title (including proglang, pkg, etc.),}
  \item{plaintitle}{character with plain LaTeX-formatted title (without proglang, pkg, etc.),}
  \item{journal}{character with journal/section,}
  \item{year}{year of publication,}
  \item{month}{month of publication,}
  \item{volume}{volume,}
  \item{number}{issue,}
  \item{pages}{number of pages,}
  \item{url}{JSS URL,}
  \item{doi}{DOI,}
  \item{type}{type of publication (article, code snippet, book review, or software review),}
  \item{directory}{character with path to all files,}
  \item{package}{name of package(s) associated with the publication,}
  \item{rpackage}{logical indicating whether the publication is associated with an R package,}
  \item{readme}{character with path to \code{readme.txt} file (if any),}
  \item{rscript}{character with path to \code{.R} script (if any).}
}

\seealso{
  \code{\link[utils]{bibentry}},
  \code{\link[utils]{person}}
}

\keyword{utilities}
