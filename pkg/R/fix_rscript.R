fix_rscript <- function(file = NULL) {
  if(is.null(file)) file <- Sys.glob("*.R")
  if(length(file) != 1L) stop("exactly one .R file should be supplied")
  file.copy(file, "_orig.R")
  formatR::tidy_source(source = file, file = file,
    comment = TRUE, blank = TRUE, keep.space = FALSE, arrow = TRUE,
    indent = 2, width.cutoff = 80)
  x <- readLines(file)
  if(x[length(x)] == " FALSE") writeLines(x[-length(x)], file)
}
