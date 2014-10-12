make_jss_bib <- function(dir_pub = "/home/zeileis/Work/JSS/Publications") {
  dir_articles <- file.path(dir_pub, "Articles")
  dir_snippets <- file.path(dir_pub, "Codesnippets")
  file_list <- as.vector(unlist(lapply(dir(dir_articles, pattern = "v"), function(x) {
    if(nchar(x) > 3) return(file.path(dir_articles, x, paste(x, ".tex", sep = "")))
    x2 <- dir(file.path(dir_articles, x), pattern = "v")
    return(sapply(x2, function(z) file.path(dir_articles, x, z, paste(z, ".tex", sep = ""))))
  })))
  file_list <- c(file_list, as.vector(unlist(lapply(dir(dir_snippets, pattern = "v"), function(x) {
    if(nchar(x) > 3) return(file.path(dir_snippets, x, paste(x, ".tex", sep = "")))
    x2 <- dir(file.path(dir_snippets, x), pattern = "v")
    return(sapply(x2, function(z) file.path(dir_snippets, x, z, paste(z, ".tex", sep = ""))))
  }))))
  
  rval <- sapply(file_list, tex2BibTeX, key_prefix = "jss:", keytype = "number")
  return(rval)
}

if(FALSE) {
jssbib <- make_jss_bib()
writeLines(rbind(jssbib, ""), "jss.bib")
}
