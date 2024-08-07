bibtool <- function(tex = NULL, orig = "_orig.bib")
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

  cit <- aux[substr(aux, 2L, 9L) == "citation"]
  cit <- substr(cit, 11L, nchar(cit) - 1L)
  cit <- strsplit(cit, ",", fixed = TRUE)
  cit <- unique(unlist(cit))

  bibfile <- aux[substr(aux, 2L, 8L) == "bibdata"]
  bibfile <- paste(substr(bibfile, 10L, nchar(bibfile) - 1L), "bib", sep = ".")
  stopifnot(file.exists(bibfile))

  bib <- bibtex::read.bib(bibfile)
  file.rename(bibfile, orig)
  
  bib <- bib[tolower(bib$key) %in% tolower(cit)]
  bibtex::write.bib(bib, bibfile)

  ## clean up temporary LaTeX files and rename .bib
  suppressWarnings(file.remove(paste0(texbase, ".", c(
    "aux", "bbl", "blg", "cp", "cps", "dvi", "fdx", "fn", "fns",
    "lof", "log", "ky", "kys", "nav", "out", "pg", "pgs", "snm",
    "toc", "tp", "vdx", "vr", "vrb", "vrs", "tpt"))))
}

shortcites <- function(x, maxlength = 6) {
  if(is.character(x)) x <- bibtex::read.bib(x)
  rval <- x$key[sapply(x$author, length) > maxlength]
  if(length(rval) < 1L) return(character(0))
  writeLines(sprintf("\\shortcites{%s}", paste(rval, collapse = ",")))
  invisible(rval)
}

fix_bib <- function(x = NULL, file = x, orig = "_orig.bib", bibtool = TRUE, doi = TRUE, shortcites = TRUE, escape = TRUE)
{
  if(is.null(x)) x <- dir(pattern = "\\.bib$")[1L]
  file <- file
  if(is.character(x)) {
    if(bibtool) {
      tex <- gsub("\\.bib$", ".tex", x)
      if(!file.exists(tex)) tex <- Sys.glob("*.tex")[1L]
      if(!is.na(tex)) bibtool(tex = tex, orig = orig)
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
      if(doi & !is.null(x[i]$url) & tolower(x[i]$bibtype) %in% c("manual", "misc")) x[i]$doi <- fix_bib_doi(x[i]$url)
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
    xbib <- toBibtex(x) ## FIXME: will get escape=FALSE argument in R-devel
    if(!isFALSE(escape)) {
      if(isTRUE(escape)) escape <- "UTF-8"
      xbib[xbib != ""] <- tools::encoded_text_to_latex(xbib[xbib != ""], escape)
    }
    writeLines(xbib, file)
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
      giv <- strsplit(paste(giv, collapse = " "), " ", fixed = TRUE)[[1L]]
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
  x <- toTitleCase(x, tolower = identity, alone = "", either = "",
    lower = readLines(system.file("case", "lower.txt", package = "jss")))
  if(is.character(x)) {
    if(x == "American Statistician") return("The American Statistician")
    if(x == "Annals of Applied Statistics") return("The Annals of Applied Statistics")
    if(x == "Annals of Probability") return("The Annals of Probability")
    if(x == "Annals of Statistics") return("The Annals of Statistics")
    if(x == "Computational Statistics and Data Analysis") return("Computational Statistics \\& Data Analysis")
    if(tolower(x) == "plos one") return("PLOS One")
    if(tolower(x) == tolower("Proceedings of the National Academy of Sciences"))
        return("Proceedings of the National Academy of Sciences of the United States of America")
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
  ## figure out DOIs from URLs:
  ## either from CRAN or from doi.org URLs
  if(grepl("cran.r-project.org", tolower(x), fixed = TRUE)) {

    if(grepl("package=", x, fixed = TRUE)) {
      x <- strsplit(x, "package=", fixed = TRUE)[[1L]][[2L]]
    } else if(grepl("web/packages/", x, fixed = TRUE)) {
      x <- strsplit(x, "web/packages/", fixed = TRUE)[[1L]][[2L]]
      x <- strsplit(x, "/", fixed = TRUE)[[1L]][[1L]]
    } else {
      x <- ""
    }
    
    if(x != "") x <- paste0("10.32614/CRAN.package.", x)
  
  } else {
  
    x <- strsplit(x, "/|//")
    x <- sapply(x, function(y) {
      ok <- length(y) >= 2L
      if(ok && substr(y[1L], 1L, 4L) == "http") {
        y <- y[-1L]
        if(length(y) < 2L) ok <- FALSE
      }
      if(ok && grepl("doi.org", y[1L], fixed = TRUE)) {
        y <- y[-1L]
        if(length(y) < 2L) ok <- FALSE    
      }
      if(ok && substr(y[1L], 1L, 3L) != "10.") {
        ok <- FALSE
      }
      if(ok && any(nchar(y) < 1L)) {
        ok <- FALSE
      }
      if(ok) paste(y, collapse = "/") else ""
    })
    x <- tolower(x)

  }
  
  if(grepl("/cran.", x, fixed = TRUE)) x <- gsub("/cran.", "/CRAN.", x, fixed = TRUE)
  
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
  i <- 0
  ok <- FALSE
  while(i < 3 & !ok) {
    i <- i + 1
    y <- try(rcrossref::cr_works(query = qry, limit = 1L)$data, silent = i < 3)
    ok <- !inherits(y, "try-error")
    if(!ok & i < 3) Sys.sleep(3)
  }
  if(!ok) return("")

  ## use the result only if score > minscore (1.5 is very ad hoc)
  y <- as.data.frame(y)
  ok <- y$score > minscore
  if(!is.null(type)) ok <- ok & (type == y$type)
  y <- if(ok) y$doi else ""
  
  ## canonicalize CRAN DOIs
  if(grepl("/cran.", y, fixed = TRUE)) y <- gsub("/cran.", "/CRAN.", y, fixed = TRUE)
  return(y)
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
