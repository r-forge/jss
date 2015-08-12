jssabstract <- function(file = Sys.glob("v*.pdf")) {
  ## ensure a non-C locale
  if(identical(Sys.getlocale(), "C")) {
    Sys.setlocale("LC_ALL", "en_US.UTF-8")
  }
  x <- as.character(tm::Corpus(tm::URISource(file), readerControl = list(reader = tm::readPDF))[[1L]])
  st <- which(x == "Abstract")[1L] + 1L
  en <- which(substr(x, 1L, 9L) == "Keywords:") - 1L
  while(x[st] == "") st <- st + 1L
  while(x[en] == "") en <- en - 1L
  x <- paste(x[st:en], collapse = " ")
  ## FIXME: fixup quotes, hyphens, etc.
  return(x)
}
