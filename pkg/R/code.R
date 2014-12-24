code_lines <- function(path = ".") {
  f <- list.files(path = path)
  f <- unlist(lapply(f, readLines))
  f <- gsub(" ", "", f, fixed = TRUE)
  f <- f[substr(f, 1, 1) != "#"]
  f <- f[f != ""]
  length(f)
}
