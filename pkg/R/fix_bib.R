bibtool <- function(tex = NULL, temp = "_foo.bib", orig = "_orig.bib")
{
  ## use supplied/available .tex file and call pdflatex
  if(is.null(tex)) {
    x <- dir()
    tex <- which(substr(x, nchar(x) - 2L, nchar(x)) == "tex")
    tex <- x[tex]
  }
  if(length(tex) != 1L) stop("need exactly one .tex file")
  stopifnot(file.exists(tex))
  system(paste("pdflatex", tex))
  
  ## determine .bib file used and call bibtool
  texbase <- tools::file_path_sans_ext(tex)  
  aux <- paste0(texbase, ".aux")
  aux <- readLines(aux)
  aux <- aux[substr(aux, 2L, 8L) == "bibdata"]
  bibbase <- substr(aux, 10L, nchar(aux) - 1L)
  bib <- paste(bibbase, "bib", sep = ".")
  stopifnot(file.exists(bib))
  system(paste("bibtool -x", texbase, ">", temp))

  ## clean up temporary LaTeX files and rename .bib
  suppressWarnings(file.remove(paste0(texbase, ".", c(
    "aux", "bbl", "blg", "cp", "cps", "dvi", "fdx", "fn", "fns",
    "lof", "log", "ky", "kys", "nav", "out", "pg", "pgs", "snm",
    "toc", "tp", "vdx", "vr", "vrb", "vrs", "tpt"))))
  file.rename(bib, orig)
  file.rename(temp, bib)
}

shortcites <- function(x, maxlength = 6) {
  if(is.character(x)) x <- bibtex::read.bib(x)
  rval <- x$key[sapply(x$author, length) > maxlength]
  if(length(rval) < 1L) return(character(0))
  writeLines(sprintf("\\shortcites{%s}", paste(rval, collapse = ",")))
  invisible(rval)
}

fix_bib <- function(x, file = x, orig = "_orig.bib", bibtool = TRUE, doi = TRUE, shortcites = TRUE)
{
  file <- file
  if(is.character(x)) {
    if(bibtool) {
      tex <- gsub("\\.bib$", ".tex", x)
      if(!file.exists(tex)) tex <- Sys.glob("*.tex")[1L]
      bibtool(tex = tex, orig = orig)
    } else {
      file.copy(x, orig)
    }
    x <- bibtex::read.bib(x)
  }
  stopifnot(inherits(x, "bibentry"))
  stopifnot(length(x) >= 1L)

  for(i in 1:length(x)) {
    if(!is.null(x[i]$author))    x[i]$author    <- fix_bib_person(x[i]$author)
    if(!is.null(x[i]$editor))    x[i]$editor    <- fix_bib_person(x[i]$editor)
    if(!is.null(x[i]$title))     x[i]$title     <- fix_bib_title(x[i]$title)
    if(!is.null(x[i]$booktitle)) x[i]$booktitle <- fix_bib_title(x[i]$booktitle)
    if(!is.null(x[i]$journal))   x[i]$journal   <- fix_bib_journal(x[i]$journal)
    if(!is.null(x[i]$publisher)) x[i]$publisher <- fix_bib_publisher(x[i]$publisher)
    if(!is.null(x[i]$edition))   x[i]$edition   <- fix_bib_edition(x[i]$edition)
    if(!is.null(x[i]$doi))       x[i]$doi       <- fix_bib_doi(x[i]$doi) else {
      if(doi & tolower(x[i]$bibtype) == "article") x[i]$doi <- get_doi(x[i]) ## could add type = "journal-article"
      if(doi & tolower(x[i]$bibtype) == "book") x[i]$doi <- get_doi(x[i], type = "book")
    }
    if(!is.null(x[i]$doi)) {
      if(x[i]$doi == "") x[i]$doi <- NULL
      if(!is.null(x[i]$doi) && !is.null(x[i]$url)) x[i]$url <- NULL
    }
    xi <- unclass(x[i])
    for(j in c("abstract", "date-added", "date-modified", "bdsk-url-1", "bdsk-url-2", "keywords", "annote", "month", "owner", "timestamp")) xi[[1L]][[j]] <- NULL
    class(xi) <- "bibentry"
    x[i] <- xi
  }

  if(shortcites) shortcites(x)
  
  if(is.character(file)) {
    bibtex::write.bib(x, file = file)
    invisible(x)
  } else {
    return(x)
  }
}

## FIXME: with discussion
fix_bib_title <- function(x) {
  if(is.null(x)) return(x)
  stopifnot(is.character(x))
  x <- fix_bib_tabspace(x)
  ## if(substr(x, 1L, 1L) == "{" && substr(x, 1L, nchar(x)) == "}" && !grepl("{", substr(x, 2L, nchar(x) - 1L))) {
  ##   x <- substr(x, 2L, nchar(x) - 1L)
  ## }
  x <- toTitleCase(x, tolower = identity, either = "FlexMix",
    lower = readLines(system.file("case", "lower.txt", package = "jss")))
  return(x)
}

fix_bib_edition <- function(x) {
  if(is.null(x)) return(x)
  stopifnot(is.character(x))
  x <- fix_bib_tabspace(x)
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

fix_bib_person <- function(x) {
  stopifnot(inherits(x, "person"))
  if(length(x) < 1L) return(x)
  for(i in 1:length(x)) {
    if(!is.null(x$given) && length(grep(".", x$given, fixed = TRUE) > 0L)) {
      giv <- fix_bib_tabspace(gsub(".", ". ", x[i]$given, fixed = TRUE))
      giv <- gsub(". -", ".-", giv, fixed = TRUE)
    } else {
      giv <- fix_bib_tabspace(x[i]$given)
    }
    if(length(giv) > 0L) {
      giv <- gsub("\\\" ", "\\\"", giv, fixed = TRUE)
      giv <- gsub("\\' ", "\\'", giv, fixed = TRUE)
      giv <- gsub("\\` ", "\\`", giv, fixed = TRUE)
      giv <- gsub("\\~ ", "\\~", giv, fixed = TRUE)
      giv <- gsub("{\\c ", "\\c{", giv, fixed = TRUE)
      giv <- strsplit(giv, " ", fixed = TRUE)[[1L]]
    }
    x[i] <- person(given = giv, family = fix_bib_tabspace(x[i]$family))
  }
  return(x)
}

fix_bib_tabspace <- function(x) {
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

fix_bib_publisher <- function(x) {
  if(is.null(x)) return(x)
  stopifnot(is.character(x))
  x <- fix_bib_tabspace(x)
  if(length(grep("Wiley", x))) return("John Wiley \\& Sons")
  if(length(grep("Springer", x))) return("Springer-Verlag")
  return(x)
}

fix_bib_journal <- function(x) {
  if(is.null(x)) return(x)
  stopifnot(is.character(x))
  x <- fix_bib_tabspace(x)
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
      y <- gsub(",", "", y, fixed = TRUE)
      y <- substr(y, 1L, 1L)
      if(y %in% c("A", "B", "C", "D")) return(paste("Journal of the Royal Statistical Society", y))
      warning("could not fix JRSS journal name")
    }    
    return(x)
  }
}

fix_bib_doi <- function(x) {
  x <- strsplit(x, "/", fixed = TRUE)
  x <- t(sapply(x, function(y) if(length(y) >= 2L) tail(y, 2L) else c("", "")))
  ok <- substr(x[, 1L], 1L, 3L) == "10." & apply(nchar(x) >= 1L, 1L, all)
  x <- apply(x, 1L, paste, collapse = "/")
  x <- tolower(x)
  x[!ok] <- ""
  return(x)  
}

get_doi <- function(x, minscore = 1.5, type = NULL) {
  ## set up query string
  if(is.list(x)) {
    if(!is.null(x$doi)) return(fix_bib_doi(x$doi))
    if(!is.null(x$url) && fix_bib_doi(x$url) != "") return(fix_bib_doi(x$url))
    x$url <- NULL
    qry <- format(x) ## FIXME better exploit the available information in setting up query?
  } else {
    qry <- x
  }
  
  ## query CrossRef through rcrossref
  y <- as.data.frame(rcrossref::cr_works(query = qry, limit = 1L)$data)

  ## use the result only if score > minscore (1.5 is very ad hoc)
  ok <- y$score > minscore
  if(!is.null(type)) ok <- ok & (type == y$type)
  if(ok) return(y$DOI) else return("")
}

add_doi <- function(x, file = "out.bib", minscore = 1.5) {
    y <- if(inherits(x, "bibentry")) x else bibtex::read.bib(x)
    dois <- sapply(y, get_doi, minscore = minscore)
    for(i in 1:length(dois)) 
        if(nchar(dois[i]) > 0) y[i]$doi <- dois[i]
          #or:# y[i]$url <- paste("http://dx.doi.org/", dois[i], sep = "")
    if(is.character(file)) {
      bibtex::write.bib(y, file = file)
      invisible(y)
    } else {
      return(y)
    }
}
