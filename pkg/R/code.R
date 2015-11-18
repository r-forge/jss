code_lines <- function(path = ".", comment = "#") {
  f <- list.files(path = path)
  f <- unlist(lapply(f, readLines))
  f <- gsub(" ", "", f, fixed = TRUE)
  f <- f[substr(f, 1, nchar(comment)) != comment]
  f <- f[f != ""]
  length(f)
}
