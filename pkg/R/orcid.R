get_orcidlink <- function(person = NULL, rows = 1L, verbose = TRUE, ...) {
  if(is.null(person)) person <- dir(pattern = "\\.tex$")[1L]
  if(is.character(person) && identical(tools::file_ext(person), "tex")) person <- suppressWarnings(jss::jss(person))
  if(inherits(person, "jss")) person <- person$person
  if(inherits(person, "person")) person <- to_utf8(format(person, include = c("given", "family")))
  person <- as.character(person)
  if(any(grepl("\\", person, fixed = TRUE))) person <- tth::tth(person)
  rval <- lapply(person, function(p) {
    p <- cbind(p, as.matrix(rorcid::orcid(p, rows = rows, ...))[, 2L:1L, drop = FALSE])
    p[, 2L] <- paste0("\\orcidlink{", p[, 2L], "}")
    if(verbose) writeLines(c(p[1L, 1L], paste0("~", p[1L, 2L]), p[, 3L], ""))
    return(p)
  })
  rval <- do.call("rbind", rval)
  colnames(rval) <- c("person", "orcidlink", "orcidurl")
  if(verbose) invisible(rval) else return(rval)
}

to_utf8 <- function(x) {
  as.character(sapply(as.character(x), function(y) {
    tools::deparseLatex(tools::latexToUtf8(tools::parseLatex(y)))
  }))
}
