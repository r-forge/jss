library("bibtex")

bibtool <- function(tex = NULL, temp = "_foo.bib", orig = "_orig.bib") {
  if(is.null(tex)) {
    x <- dir()
    tex <- which(substr(x, nchar(x) - 2L, nchar(x)) == "tex")
    tex <- x[tex]
  }
  if(length(tex) != 1L) stop("need exactly one .tex file")
  stopifnot(file.exists(tex))
  system(paste("pdflatex", tex))
  texbase <- gsub(".tex", "", tex, fixed = TRUE)
  
  aux <- paste(texbase, "aux", sep = ".")
  aux <- readLines(aux)
  aux <- aux[substr(aux, 2L, 8L) == "bibdata"]
  bibbase <- substr(aux, 10L, nchar(aux) - 1L)
  bib <- paste(bibbase, "bib", sep = ".")
  stopifnot(file.exists(bib))

  system(paste("bibtool -x", texbase, ">", temp))
  system(paste("texclean", tex))

  file.rename(bib, orig)
  file.rename(temp, bib)

  invisible()
}

write.bib <- function(x, file = "Rbib.bib") {
  stopifnot(inherits(x, "bibentry"))
  writeLines(toBibtex(x), file)
}

shortcites <- function(x, maxlength = 6) {
  rval <- x$key[sapply(x$author, length) > maxlength]
  writeLines(sprintf("\\shortcites{%s}",
    paste(rval, collapse = ",")))
  invisible(rval)
}

## FIXME: title style, software titles, with discussion, A.-B. Name, editor field?

bibfix <- function(x) {
  stopifnot(inherits(x, "bibentry"))
  stopifnot(length(x) >= 1L)
  for(i in 1:length(x)) {
    if(!is.null(x[i]$author))    x[i]$author    <- fix_person(x[i]$author)
    if(!is.null(x[i]$editor))    x[i]$editor    <- fix_person(x[i]$editor)
    if(!is.null(x[i]$title))     x[i]$title     <- fix_title(x[i]$title)
    if(!is.null(x[i]$booktitle)) x[i]$booktitle <- fix_title(x[i]$booktitle)
    if(!is.null(x[i]$journal))   x[i]$journal   <- fix_journal(x[i]$journal)
    if(!is.null(x[i]$publisher)) x[i]$publisher <- fix_publisher(x[i]$publisher)
    if(!is.null(x[i]$edition))   x[i]$edition   <- fix_edition(x[i]$edition)
    xi <- unclass(x[i])
    for(j in c("abstract", "date-added", "date-modified", "bdsk-url-1", "bdsk-url-2", "keywords", "annote", "month", "owner", "timestamp")) xi[[1]][[j]] <- NULL
    class(xi) <- "bibentry"
    x[i] <- xi
  }
  x
}

fix_title <- function(x) {
  if(is.null(x)) return(x)
  stopifnot(is.character(x))
  x <- fix_tabspace(x)
  return(x)
}

fix_edition <- function(x) {
  if(is.null(x)) return(x)
  stopifnot(is.character(x))
  x <- fix_tabspace(x)
  x1 <- tolower(x)
  if(x1 == "first") return("1st")
  if(x1 == "second") return("2nd")
  if(x1 == "third") return("3rd")
  if(x1 == "fourth") return("4th")
  if(x1 == "fifth") return("5th")
  if(x1 == "sixth") return("6th")
  if(x1 == "seventh") return("7th")
  return(x)
}

fix_person <- function(x) {
  stopifnot(inherits(x, "person"))
  if(length(x) < 1L) return(x)
  for(i in 1:length(x)) {
    if(!is.null(x$given) && length(grep(".", x$given, fixed = TRUE) > 0L)) {
      x[i] <- person(fix_tabspace(gsub(".", ". ", x[i]$given, fixed = TRUE)), fix_tabspace(x[i]$family))
    } else {
      x[i] <- person(given = fix_tabspace(x[i]$given), family = fix_tabspace(x[i]$family))
    }
  }
  return(x)
}

fix_tabspace <- function(x) {
  if(is.null(x)) return(x)
  stopifnot(is.character(x))
  x <- gsub("  ", " ", x, fixed = TRUE)
  x <- gsub("\n", " ", x, fixed = TRUE)
  x <- gsub("  ", " ", x, fixed = TRUE)
  x <- gsub("\t", " ", x, fixed = TRUE)
  x <- gsub("  ", " ", x, fixed = TRUE)
  x <- gsub("[[:space:]]+$", "", x)
  return(x)
}

fix_publisher <- function(x) {
  if(is.null(x)) return(x)
  stopifnot(is.character(x))
  x <- fix_tabspace(x)
  if(is.character(x)) {
    if(length(grep("Wiley", x))) return("John Wiley \\& Sons")
    if(length(grep("Springer", x))) return("Springer-Verlag")
    return(x)
  }
}

fix_journal <- function(x) {
  if(is.null(x)) return(x)
  stopifnot(is.character(x))
  x <- fix_tabspace(x)
  if(is.character(x)) {
    if(x == "American Statistician") return("The American Statistician")
    if(x == "Annals of Statistics") return("The Annals of Statistics")
    if(x == "Computational Statistics and Data Analysis") return("Computational Statistics \\& Data Analysis")
    if(length(grep("Journal of the Royal Statistical Society", x))) {
      y <- gsub("Journal of the Royal Statistical Society", "", x)
      if(gsub(" ", "", y) %in% c("A", "B", "C", "D")) return(x)
      y <- gsub("Series", "", y, fixed = TRUE)
      y <- gsub("The", "", y, fixed = TRUE)
      y <- gsub(" ", "", y, fixed = TRUE)
      y <- gsub(":", "", y, fixed = TRUE)
      y <- substr(y, 1L, 1L)
      if(y %in% c("A", "B", "C", "D")) return(paste("Journal of the Royal Statistical Society", y))
      warning("could not fix JRSS journal name")
    }    
    return(x)
  }
}

