jssbib <- function(file, keytype = c("author", "number"))
{
  keytype <- match.arg(keytype)

  ## convenience functions
  extract_arg <- function(x) {
    x <- paste(x, "%")
    x <- paste(tail(strsplit(x, "{", fixed = TRUE)[[1]], -1), collapse = "{")
    x <- paste(head(strsplit(x, "}", fixed = TRUE)[[1]], -1), collapse = "}")
    x
  }

  get_type <- function(x) {
    x <- x[grep("\\documentclass", x, fixed = TRUE)[1]]
    x <- gsub("\\documentclass", "", x, fixed = TRUE)
    x <- gsub("{jss}", "", x, fixed = TRUE)
    x <- gsub("[", "", x, fixed = TRUE)
    x <- gsub("]", "", x, fixed = TRUE)
    x <- gsub(" ", "", x, fixed = TRUE)
    if(length(grep(",", x, fixed = TRUE)) > 0) x <- strsplit(x, ",", fixed = TRUE)[[1]][1]
    if(x == "") x <- "article"
    stopifnot(x %in% c("article", "codesnippet", "bookreview", "softwarereview"))
    return(x)
  }
  
  get_author <- function(x)
    extract_arg(x[grep("Plainauthor", x)])

  author2tex <- function(author)
    gsub(", ", " and ", author, fixed = TRUE)

  author2key <- function(author, year = NULL) {
    x <- strsplit(author, ", ", fixed = TRUE)[[1]]
    if(length(x) > 3) x <- x[1:3]
    x <- paste(sapply(strsplit(x, " "), tail, 1), collapse = "+")
    if(!is.null(year)) x <- paste(x, ":", year, sep = "")
    x <- gsub("{", "", x, fixed = TRUE)
    x <- gsub("}", "", x, fixed = TRUE)
    x <- gsub("\\", "", x, fixed = TRUE)
    x <- gsub("\"", "", x, fixed = TRUE)
    x <- gsub("'", "", x, fixed = TRUE)
    x <- gsub("^", "", x, fixed = TRUE)
    return(x)
  }
  
  get_title <- function(x) {
    tt <- extract_arg(x[grep("\\title", x, fixed = TRUE)])
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
  
  get_volume <- function(x)
    extract_arg(x[grep("\\Volume", x, fixed = TRUE)])

  get_number <- function(x)
    extract_arg(x[grep("\\Issue", x, fixed = TRUE)])

  get_year <- function(x)
    extract_arg(x[grep("\\Year", x, fixed = TRUE)])

  get_month <- function(x)
    extract_arg(x[grep("\\Month", x, fixed = TRUE)])

  get_pages <- function(file) {
    file <- gsub("tex", "pdf", file)
    rval <- system(paste("pdfinfo", file), intern = TRUE)
    tail(strsplit(rval[grep("Pages", rval)], " ")[[1]], 1)
  }

  ## read in TeX
  x <- readLines(file)
  
  ## process type
  type <- get_type(x)
  journal <- if(type == "article") "Journal of Statistical Software"
     else if(type == "codesnippet") "Journal of Statistical Software, Code Snippets"
     else if(type == "bookreview") "Journal of Statistical Software, Book Reviews"
     else if(type == "softwarereview") "Journal of Statistical Software, Software Reviews"
  stype <- if(type == "article") "i"
     else if(type == "codesnippet") "c"
     else if(type == "bookreview") "b"
     else if(type == "softwarereview") "s"

  ## get info
  auth <- get_author(x)
  vol <- get_volume(x)
  vol2 <- gsub(" ", "0", format(c(as.numeric(vol), 10)))[-2]
  vol3 <- gsub(" ", "0", format(c(as.numeric(vol), 100)))[-2]
  num <- get_number(x)
  num2 <- gsub(" ", "0", format(c(as.numeric(num), 10)))[-2]
  year <- get_year(x)
  month <- get_month(x)
  url <- paste0("http://www.jstatsoft.org/v", vol2, "/", stype, num2, "/")
  doi <- paste0("10.18637/jss.v", vol3, ".", stype, num2)
  jsskey <- paste("v", vol3, stype, num2, sep = "")

  stopifnot(isTRUE(identical(tail(strsplit(file, "/")[[1]], 1),
    as.character(paste(gsub("v0", "v", jsskey), ".tex", sep = "")))))

  rval <- list(
    authorlist = strsplit(auth, ", ")[[1]],
    key = if(keytype == "author") author2key(auth, year) else jsskey,
    author = author2tex(auth),
    title = get_title(x),
    plaintitle = strip_title(get_title(x)),
    journal = journal,
    year = year,
    month = month,
    volume = vol,
    number = num,
    pages = get_pages(file),
    url = url,
    doi = doi
  )
  
  return(rval)
}

tex2BibTeX <- function(file, key_prefix = "", keytype = c("author", "number"), url = FALSE)
{
  x <- jssbib(file, keytype = keytype)

  rval <- c(
    paste("@Article{", key_prefix, x$key, ",", sep = ""),
    paste("  author\t= {", x$author, "},", sep = ""),
    paste("  title\t\t= {", x$title, "},", sep = ""),
    paste("  journal\t= {", x$journal, "},", sep = ""),
    paste("  year\t\t= {", x$year, "},", sep = ""),
    paste("  volume\t= {", x$volume, "},", sep = ""),
    paste("  number\t= {", x$number, "},", sep = ""),
    paste("  pages\t\t= {1--", x$pages, "},", sep = ""),
    paste("  doi\t\t= {", x$doi, "}", sep = ""),
    if(url) paste("  url\t\t= {", x$url, "}", sep = "") else NULL,
    "}")
  return(rval)
}

tex2CITATION <- function(file, name = NULL)
{
  x <- jssbib(file)

  if(is.null(name)) {
    name <- if(length(grep("\\pkg", x$title)) < 1) "<NAME>" else {
      nam <- strsplit(x$title, "\\pkg")[[1]][2]
      nam <- strsplit(nam, "{", fixed = TRUE)[[1]][2]
      nam <- strsplit(nam, "}", fixed = TRUE)[[1]][1]
      nam
    }
  }

  rval <- c(
    paste("citHeader(\"To cite ", name, " in publications use:\")", sep = ""),
    "",
    "citEntry(entry = \"Article\",",
    paste("  title        = \"", x$plaintitle, "\",", sep = ""),
    paste("  author       = personList(", paste("as.person(\"", x$authorlist, "\")",
      sep = "", collapse = ",\n                   "), "),", sep = ""),
    "  journal      = \"Journal of Statistical Software\",",
    paste("  year         = \"", x$year, "\",", sep = ""),
    paste("  volume       = \"", x$volume, "\",", sep = ""),
    paste("  number       = \"", x$number, "\",", sep = ""),
    paste("  pages        = \"1--", x$pages, "\",", sep = ""),
    paste("  doi          = \"", x$doi, "\",", sep = ""),
    "",
    "  textVersion  =",
    paste("  paste(\"", paste(x$authorlist, collapse = ", "), " (", x$year, ").\",", sep = ""),
    paste("        \"", gsub("}", "",
      gsub("{", "", x$plaintitle, fixed = TRUE), fixed = TRUE), ".\",", sep = ""),
    paste("        \"Journal of Statistical Software, ", x$volume, "(", x$number, "), 1-", x$pages, ".\",", sep = ""),
    paste("        \"doi:", x$doi, ".\")", sep = ""),
    ")",
    ""
  )
  
  return(rval)
}

tex2README <- function(file)
{
  x <- jssbib(file)
  x$plaintitle <- gsub("{", "", x$plaintitle, fixed = TRUE)
  x$plaintitle <- gsub("}", "", x$plaintitle, fixed = TRUE)

  Rpack <- dir(pattern = "\\.tar\\.gz$")
  Rcode <- if(file.exists(gsub("tex", "R", file))) gsub("tex", "R", file) else NULL
  Roth <- Sys.glob("*.*")
  Roth <- Roth[!(Roth %in% c(Rpack, Rcode, file, gsub("tex", "pdf", file), gsub("tex", "bib", file), "README.txt"))]

  rval <- c(
    "Source code for",
    paste("  ", paste(x$authorlist, collapse = ", "), sep = ""),
    paste("  ", x$plaintitle, sep = ""),
    sprintf("  Journal of Statistical Software, %s(%s), %s %s",
      x$volume, x$number, x$month, x$year),
    "",
    if(length(Rpack) + length(Rcode) < 1) "FILE: DESCRIPTION",
    if(length(Rpack) > 0) paste(Rpack, ": R source package", sep = ""),
    if(length(Rcode) > 0) paste(Rcode, ":", 
      if(length(Rpack) < 1) "" else paste(rep(" ", nchar(Rpack) - nchar(Rcode)), collapse = ""),
      " R example code from the paper", sep = ""),
    if(length(Roth) > 0L) paste(Roth, ":", sep = ""),
    ""
  )
  
  return(rval)
}

fix_bst_license <- function(bst = "jss.bst") {
  file <- bst
  bst <- readLines(bst)
  wi <- grep("======", bst, fixed = TRUE)
  bst[wi[1]:wi[2]] <- c(
    "%% License: GPL-2",
    " % ===============================================================",
    " % IMPORTANT NOTICE:",
    " % This bibliographic style (bst) file has been generated from one or",
    " % more master bibliographic style (mbs) files, listed above, provided",
    " % with kind permission of Patrick W Daly.",
    " %",
    " % This generated file can be redistributed and/or modified under the terms",
    " % of the General Public License (Version 2).",
    " % ===============================================================")
  writeLines(bst, file)
  invisible(bst)
}

write_info <- function(x, files = c("CITATION", "README"), ...) {
  stopifnot(require("tools"))
  tpt <- Sys.glob("*.tpt")
  if(length(tpt) > 0L) file.remove(tpt)
  if(missing(x)) x <- Sys.glob("v*.tex")
  if(length(x) > 1) stop("Only one file should be supplied.")
  files <- sapply(files, function(z) match.arg(z, c("CITATION", "README")))
  if("CITATION" %in% files) writeLines(tex2CITATION(x, ...), "CITATION")
  if("README" %in% files) writeLines(tex2README(x, ...), "README.txt")
  for(i in Sys.glob("*.pdf")) compactPDF(i) ##, gs_quality = "ebook")
  invisible()
}
