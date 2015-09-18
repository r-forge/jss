\name{fix_rscript}
\alias{fix_rscript}
\alias{code_lines}

\title{Fix R Scripts to use JSS's Preferred Formatting}

\description{
The function \code{fix_rscript} is just a simple convenience
interface to \code{\link[formatR]{tidy_source}}, using certain
argument settings. The \code{code_lines} function is a small
utility for counting lines after removing comments and empty lines.
}

\usage{
fix_rscript(file = NULL)

code_lines(path = ".")
}

\arguments{
  \item{file}{character. The .R script that should be fixed,
    by default \code{Sys.glob("*.R")} is used.}
  \item{path}{character specifying a directory with .R files.}
}

\seealso{
  \code{\link[formatR]{tidy_source}}
}

\keyword{utilities}
