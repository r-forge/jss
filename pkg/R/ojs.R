format_jss_to_ojs <- function(x)
{
  ## author list
  person <- paste(format(x$person, include = c("given", "family")), collapse = ", ")

  ## supplemental files
  if(x$type %in% c("bookreview", "softwarereview")) {
    supp <- NULL
  } else {
    supp <- format(x, "README", paper = TRUE)
    if(length(supp) >= 6L) {
      nsupp <- length(supp) - 5L
      if(any(supp[-(1L:5L)] == "")) nsupp <- min(which(supp[-(1L:5L)] == "")) - 1L
    } else {
      nsupp <- 0L
    }
    supp <- if(nsupp > 0L) supp[6L:(5L + nsupp)] else NULL
  }

  c(sprintf('Title & Abstract\n================\nTitle:\n%s\n', x$plaintitle),
    sprintf('Abstract:\n%s\n', ojs_abstract(file.path(x$directory, x$pdf), x$type)),
    sprintf('Keywords:\n%s\n', x$keywords),
    
    sprintf('Contributors\n============\n%s\n', paste(sapply(format(x$person), latex_to_utf8), collapse = "\n")),

    sprintf('Identifiers\n===========\nDOI: %s', x$doi),

    '\nGalleys\n=======',
    supp,
    "",

    sprintf('Permissions & Disclosure\n========================\n%s\n', paste(sapply(format(x$person, include = c("given", "family")), latex_to_utf8), collapse = ", ")),

    sprintf('Issue\n=====\nVolume: %s', x$volume),
    sprintf('Section: %s', factor(x$type,
      levels = c("article", "codesnippet", "bookreview", "softwarereview"),
      labels = c("Articles", "Code Snippets", "Book Reviews", "Software Reviews"))),
    sprintf('Pages: 1 - %s', x$pages),
    sprintf('URL path: %s', x$key["number"])
    #sprintf('Date submitted: %s', x$submitdate),
    #sprintf('Date accepted:  %s', x$acceptdate),
    #sprintf('Date published: %s', Sys.Date()),
  )
}

latex_to_utf8 <- function(x) {
  ix <- grepl("\\", x, fixed = TRUE)
  if(!any(ix)) return(x)
  x <- tools::deparseLatex(tools::latexToUtf8(tools::parseLatex(x)))
  x <- gsub("{", "", x, fixed = TRUE)
  x <- gsub("}", "", x, fixed = TRUE)
  return(x)
}

ojs_abstract <- function(file, type) {
  if(type %in% c("bookreview", "softwarereview")) return('-')

  ## ensure a non-C locale
  if(identical(Sys.getlocale(), "C")) {
    Sys.setlocale("LC_ALL", "en_US.UTF-8")
  }

  ## use tm to read PDF
  x <- as.character(tm::Corpus(tm::URISource(file), readerControl = list(reader = tm::readPDF(engine = "xpdf")))[[1L]])

  ## extract abstract
  st <- which(x == "Abstract")[1L] + 1L
  en <- which(substr(x, 1L, 9L) == "Keywords:") - 1L
  while(x[st] == "") st <- st + 1L
  while(x[en] == "") en <- en - 1L
  x <- paste(x[st:en], collapse = " ")

  ## fixup UTF-8 quotes and hyphens
  fix <- rbind(
    c("\303\227", "x"),
    c("\342\210\222", "-"),
    c("\342\200\223", " - "),
    c("\342\200\224", " - "),
    c("\342\200\230", "'"),
    c("\342\200\231", "'"),
    c("\342\200\234", "\""),
    c("\342\200\235", "\"")
  )
  for(i in 1:nrow(fix)) x <- gsub(fix[i, 1], fix[i, 2], x, fixed = TRUE)

  return(x)
}
