jss <- function(dir = ".")
{
  ## manage files/directories
  dir <- tools::file_path_as_absolute(dir)
  if(file_ext(dir) == "tex") {
    file_tex <- dir
    dir <- paste(head(strsplit(dir, "/", fixed = TRUE)[[1L]], -1L), collapse = "/")
  } else {
    if(file_ext(dir) == "") {
      file_tex <- file.path(dir, dir(dir, pattern = "\\.tex$"))
      n <- sapply(strsplit(file_tex, "/", fixed = TRUE), tail, 1L)      
      if(length(file_tex) > 1L) file_tex <- file_tex[substr(n, 1L, 1L) == "v"]
      if(length(file_tex) > 1L) file_tex <- file_tex[nchar(n) <= 11L]
      if(length(file_tex) > 1L) file_tex <- file_tex[nchar(n) <= 10L]
      if(length(file_tex) > 1L) file_tex <- file_tex[1L]
    } else {
      stop("unknown specification of 'dir'")
    }
  }
  file_tex0 <- tail(strsplit(file_tex, "/")[[1L]], 1L)

  ## supplementary files
  get_supplementary_file <- function(pattern) {
    file <- dir(dir, pattern = pattern)
    if(length(file) < 1L) return(NULL)
    structure(file.path(dir, file), .Names = file)
  }
  file_readme <- get_supplementary_file("^readme\\.txt$")
  file_rpack <- get_supplementary_file("\\.tar\\.gz$")
  file_rscript <- get_supplementary_file("\\.R$")

  ## convenience functions
  extract_arg <- function(x) {
    x <- paste(x, "%")
    x <- paste(tail(strsplit(x, "{", fixed = TRUE)[[1L]], -1L), collapse = "{")
    x <- paste(head(strsplit(x, "}", fixed = TRUE)[[1L]], -1L), collapse = "}")
    x
  }
  extract_cmd <- function(x, cmd) extract_arg(x[grep(cmd, x, fixed = TRUE)])

  get_type <- function(x) {
    x <- x[grep("\\documentclass", x, fixed = TRUE)[1]]
    x <- gsub("\\documentclass", "", x, fixed = TRUE)
    x <- gsub("{jss}", "", x, fixed = TRUE)
    x <- gsub("{jss0}", "", x, fixed = TRUE)
    x <- gsub("{jss2}", "", x, fixed = TRUE)
    x <- gsub("[", "", x, fixed = TRUE)
    x <- gsub("]", "", x, fixed = TRUE)
    x <- gsub(" ", "", x, fixed = TRUE)
    if(length(grep(",", x, fixed = TRUE)) > 0) x <- strsplit(x, ",", fixed = TRUE)[[1]][1]
    if(x == "") x <- "article"
    stopifnot(x %in% c("article", "codesnippet", "bookreview", "softwarereview"))
    return(x)
  }
  
  get_author <- function(x, type = "article") {
    field <- switch(type,
      "article" = "\\Plainauthor",
      "codesnippet" = "\\Plainauthor",
      "bookreview" = "\\Plainreviewer",
      "softwarereview" = "\\Plainreviewer"
    )
    extract_arg(x[grep(field, x)])
  }

  author2tex <- function(author)
    gsub(", ", " and ", author, fixed = TRUE)

  author2key <- function(author, year = NULL) {
    x <- strsplit(author, ", ", fixed = TRUE)[[1]]
    if(length(x) > 3) x <- x[1:3]
    x <- paste(sapply(strsplit(x, " "), tail, 1), collapse = "+")
    if(!is.null(year)) x <- paste(x, ":", year, sep = "")
    for(chr in c("{", "}", "\\", "\"", "'", "^", "`")) x <- gsub(chr, "", x, fixed = TRUE)
    return(x)
  }
  
  get_title <- function(x, type = "article") {
    field <- switch(type,
      "article" = "\\title",
      "codesnippet" = "\\title",
      "bookreview" = "\\Booktitle",
      "softwarereview" = "\\Softwaretitle"
    )
    tt <- extract_arg(x[grep(field, x, fixed = TRUE)])
    tt <- gsub("\\\\", " ", tt, fixed = TRUE)
    tt <- gsub("~", " ", tt, fixed = TRUE)
    return(tt)
  }
  
  strip_title <- function(x) {
    x <- gsub("\\\\proglang", "", x)
    x <- gsub("\\\\pkg", "", x)
    x <- gsub("\\\\code", "", x)
    return(x)
  }
  
  get_pages <- function(file) {
    file <- paste0(file_path_sans_ext(file), ".pdf")
    rval <- system(paste("pdfinfo", file), intern = TRUE)
    tail(strsplit(rval[grep("Pages", rval)], " ")[[1]], 1)
  }

  get_pkg_from_title <- function(x) {
    if(!grepl("\\pkg", x)) return(NULL)
    x <- strsplit(x, "\\pkg")[[1L]][-1L]
    x <- sapply(strsplit(x, "{", fixed = TRUE), "[", 2L)
    x <- sapply(strsplit(x, "}", fixed = TRUE), "[", 1L)
    return(x)
  }

  get_pkg_from_rpack <- function(x) {
    if(is.null(x)) return(NULL)
    sapply(strsplit(x, "_", fixed = TRUE), "[", 1L)
  }

  get_email <- function(x, type = "article") {
    ## convenience function for collapsing authors
    clps <- function(x) {
      x <- gsub(" ", "", x, fixed = TRUE)
      x <- gsub(".", "", x, fixed = TRUE)
      x <- gsub("\\", "", x, fixed = TRUE)
      tolower(strsplit(x, ",")[[1L]])    
    }
    ## extract list of authors names (collapsed)
    au <- clps(get_author(x, type))

    ## extract address parts
    em <- grep("\\Address", x, fixed = TRUE)
    em <- x[(em + 1L):(min(grep("^\\}", x)[grep("^}", x) > em]) - 1L)]
    em <- split(em, cumsum(em == "") + 1)

    ## extract e-mail addresses
    em <- lapply(em, function(z) {
      z <- z[z != ""]
      au <- clps(z[1L])
      z <- unlist(strsplit(z[-1L], ", ", fixed = TRUE))
      z <- sapply(z[grep("\\email", z, fixed = TRUE)], extract_arg)
      z <- as.list(as.character(z))
      if(length(z) < length(au)) {
        z <- c(z, vector(length = length(au) - length(z), mode = "list"))
      } else if(length(z) > length(au)) {
        z <- z[1L:length(au)]
      }
      names(z) <- au
      z
    })
    
    ## try to match to authors
    names(em) <- NULL
    em <- do.call("c", em)
    em <- em[au]
    names(em) <- NULL
    return(em)
  }

  ## read in TeX
  x <- readLines(file_tex)
  
  ## process type
  type <- get_type(x)
  journal <- switch(type,
    "article" =  "Journal of Statistical Software",
    "codesnippet" = "Journal of Statistical Software, Code Snippets",
    "bookreview" = "Journal of Statistical Software, Book Reviews",
    "softwarereview" = "Journal of Statistical Software, Software Reviews"
  )
  stype <- switch(type,
    "article" = "i",
    "codesnippet" = "c",
    "bookreview" = "b",
    "softwarereview" = "s"
  )

  ## get info
  auth <- get_author(x, type = type)
  vol <- extract_cmd(x, "\\Volume")
  vol2 <- gsub(" ", "0", format(c(as.numeric(vol), 10)))[-2L]
  vol3 <- gsub(" ", "0", format(c(as.numeric(vol), 100)))[-2L]
  num <- extract_cmd(x, "\\Issue")
  num2 <- strsplit(num, "/", fixed = TRUE)[[1L]][1L]
  num2 <- gsub(" ", "0", format(c(as.numeric(num2), 10)))[-2L]
  year <- extract_cmd(x, "\\Year")
  month <- extract_cmd(x, "\\Month")
  url <- paste0("http://www.jstatsoft.org/v", vol2, "/", stype, num2, "/")
  doi <- paste0("10.18637/jss.v", vol3, ".", stype, num2)
  DOI <- extract_cmd(x, "DOI")
  if(!(DOI %in% c("", doi))) warning(sprintf("DOI needs to be fixed: \\DOI{%s}", doi))
  jsskey <- paste("v", vol3, stype, num2, sep = "")
  tit <- get_title(x, type = type)
  ttit <- strip_title(tit)
  ptit <- gsub("{", "", gsub("}", "", ttit, fixed = TRUE), fixed = TRUE)
  pers <- as.person(auth)
  pers$email <- get_email(x, type = type)
  kwd <- extract_cmd(x, "\\Plainkeywords")
  if(kwd == "") kwd <- extract_cmd(x, "\\Keywords")

  if(!isTRUE(identical(file_tex0,
    as.character(paste(gsub("v0", "v", jsskey), ".tex", sep = ""))))) {
    warning("filename and volume/issue do not match")
  }

  rval <- list(
    key = c("number" = jsskey, "author" = author2key(auth, year)),
    author = author2tex(auth),
    person = pers,
    title = tit,
    textitle = ttit,    
    plaintitle = ptit,
    journal = journal,
    year = year,
    month = month,
    volume = vol,
    number = num,
    pages = get_pages(file_tex),
    keywords = kwd,
    url = url,
    doi = doi,
    type = type,
    submitdate = extract_cmd(x, "\\Submitdate"),
    acceptdate = extract_cmd(x, "\\Acceptdate"),
    directory = dir,
    pdf = paste0(file_path_sans_ext(file_tex0), ".pdf"),
    package = if(type %in% c("b", "s")) NULL else unique(c(get_pkg_from_title(tit), get_pkg_from_rpack(names(file_rpack)))),
    rpackage = (stype %in% c("i", "c")) & ((length(file_rpack) > 0L) | (length(file_rscript) > 0L)),
    readme = if(type %in% c("b", "s")) NULL else file_readme,
    rscript = if(type %in% c("b", "s")) NULL else file_rscript
  )
  class(rval) <- "jss"
  
  return(rval)
}

as.bibentry <- function(x, ...) UseMethod("as.bibentry")

as.bibentry.jss <- function(x, keytype = c("number", "author"), doi = TRUE, url = !doi, ...)
{
  b <- bibentry(
    bibtype = "Article",
    key = x$key[match.arg(keytype)],
    author = x$person,
    title = x$title,
    year = x$year,
    journal = x$journal,
    volume = x$volume,
    number = x$number,
    pages = paste0("1--", x$pages),
    doi = x$doi,
    url = x$url
  )
  if(!doi) b$doi <- NULL
  if(!url) b$url <- NULL
  return(b)
}

as.person.jss <- function(x) x$person

as.character.jss <- function(x, ...) {
  format.jss(x, ...)
}

format.jss <- function(x, style = "BibTeX", ...) {
  do.call(paste0("format_jss_to_", tolower(style)), list(x, ...))
}

format_jss_to_bibtex <- function(x, keyprefix = "", keytype = c("author", "number"), doi = TRUE, url = !doi)
{
  if(!inherits(x, "jss")) x <- jss(x)
  c(paste0("@Article{", keyprefix, x$key[match.arg(keytype)], ","),
    paste0("  author\t= {", x$author, "},"),
    paste0("  title\t\t= {", x$title, "},"),
    paste0("  journal\t= {", x$journal, "},"),
    paste0("  year\t\t= {", x$year, "},"),
    paste0("  volume\t= {", x$volume, "},"),
    paste0("  number\t= {", x$number, "},"),
    paste0("  pages\t\t= {1--", x$pages, "},"),
    if(doi) paste0("  doi\t\t= {", x$doi, "}", if(url) "," else "") else NULL,
    if(url) paste0("  url\t\t= {", x$url, "}") else NULL,
    "}")
}

format_jss_to_citation <- function(x, package = NULL)
{
  if(!inherits(x, "jss")) x <- jss(x)
  if(!x$rpackage) return(NULL)
  if(is.null(package)) package <- paste(x$package, collapse = "/")
  if(is.null(package)) package <- '<PKG>'
  c('bibentry(bibtype = "Article",',
    paste0('  title        = "', x$textitle, '",'),
    paste0('  author       = ', paste0(format(x$person, style = 'R'), collapse = '\n                 '), ","),
    paste0('  journal      = "', x$journal, '",'),
    paste0('  year         = "', x$year, '",'),
    paste0('  volume       = "', x$volume, '",'),
    paste0('  number       = "', x$number, '",'),
    paste0('  pages        = "1--', x$pages, '",'),
    paste0('  doi          = "', x$doi, '",'),
    '',
    paste0('  header       = "To cite ', package, ' in publications use:",'),
    '  textVersion  =',
    paste0('  paste("', paste0(format(x$person, include = c('given', 'family')), collapse = ', '), ' (', x$year, ').",'),
    paste0('        "', x$plaintitle, '.",'),
    paste0('        "Journal of Statistical Software, ', x$volume, '(', x$number, '), 1-', x$pages, '.",'),
    paste0('        "doi:', x$doi, '")'),
    ')',
    ''
  )
}

format_jss_to_readme <- function(x)
{
  if(!inherits(x, "jss")) x <- jss(x)

  fil <- dir(x$directory, pattern = "\\.")
  fil <- fil[!(fil %in% c(
    paste0(x$key["number"], c(".tex", ".bib", ".pdf")),
    gsub("v0", "v", paste0(x$key["number"], c(".tex", ".bib", ".pdf")), fixed = TRUE),
    "README.txt", "readme.txt", "_orig.bib", "_orig.R", "paper.pdf"))]

  if(length(fil) > 0L) {
    vfil <- substr(fil, 1L, 1L) == "v"
    descr <- ifelse(x$rpackage & file_ext(fil) == "gz", "R source package",
      ifelse(vfil & file_ext(fil) == "R", "R replication code",    
      ifelse(vfil & file_ext(fil) == "m", "MATLAB replication code",
      ifelse(vfil & file_ext(fil) == "py", "Python replication code",
      ifelse(vfil & file_ext(fil) == "sas", "SAS replication code",
      ifelse(vfil & file_ext(fil) == "c", "C replication code",
      ifelse(file_ext(fil) == "R", "R source code",
      ifelse(file_ext(fil) == "m", "MATLAB source code",
      ifelse(file_ext(fil) == "py", "Python source code",
      ifelse(file_ext(fil) == "sas", "SAS source code",
      ifelse(file_ext(fil) == "c", "C source code",
      ifelse(file_ext(fil) == "csv", "Supplementary data (CSV format)",
      ifelse(file_ext(fil) == "rda", "Supplementary data (R binary format)",
      ifelse(file_ext(fil) == "Rdata", "Supplementary data (R binary format)",
      ifelse(file_ext(fil) == "RData", "Supplementary data (R binary format)",
      ifelse(file_ext(fil) %in% c("gz", "zip", "tar") & !vfil, "Source code",
      "Replication materials"))))))))))))))))
  }
    
  rval <- c(
    "Source code and replication materials for",
    paste("  ", paste0(format(x$person, include = c('given', 'family')), collapse = ', '), sep = ""),
    paste("  ", x$plaintitle, sep = ""),
    sprintf("  %s, %s(%s), %s %s",
      x$journal, x$volume, x$number, x$month, x$year),
    "",
    if(length(file) > 0L) c(paste0(format(paste0(fil, ": ")), descr), "") else NULL
  )
  
  return(rval)
}
