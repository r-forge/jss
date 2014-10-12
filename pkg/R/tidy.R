tidy <- function(file = NULL) {
  if(is.null(file)) file <- Sys.glob("*.R")
  if(length(file) != 1L) stop("there should be exactly one .R file")
  file.copy(file, "_orig.R")
  formatR::tidy.source(source = file, file = file,
    keep.comment = TRUE, keep.blank.line = TRUE,
    keep.space = FALSE, replace.assign = TRUE, reindent.spaces = 2,
    width.cutoff = 80)
}
