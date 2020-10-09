format_jss_to_twitter <- function(x, hashtags = NULL) {
  ## package name
  if(!x$rpackage) {
    fil <- dir(x$directory, pattern = "\\.")
    fil <- fil[!(fil %in% c(paste0(x$key["number"], c(".tex", 
        ".bib", ".pdf")), gsub("v0", "v", paste0(x$key["number"], 
        c(".tex", ".bib", ".pdf")), fixed = TRUE), "README.txt", 
        "readme.txt", "_orig.bib", "_orig.R", "paper.pdf"))]
    if(length(fil) > 0L) {
      vfil <- substr(fil, 1L, 1L) == "v"
      if(any(vfil)) fil <- fil[vfil]
      fil <- fil[1L]
      pkg <- switch(tools::file_ext(fil),
        "m" = " #matlab",
	"py" = " #python",
	"sas" = " #sas")
    } else {
      pkg <- ""
    }
  } else {
    pkg <- " #rstats"
  }
  pkg <- if(is.null(x$package)) "" else sprintf(' with%s pkg "%s"', pkg, x$package)
  
  ## author list in UTF-8
  aut <- format(x$person, include = "family")
  aut <- paste(aut, collapse = ", ")
  aut <- tools::latexToUtf8(tools::parseLatex(aut))
  aut <- paste(aut, collapse = "")

  ## hashtags
  hashtags <- c("#jstatsoft", hashtags)
  hashtags <- gsub("#| ", "", hashtags)
  hashtags <- paste0("#", hashtags)
  hashtags <- paste(hashtags, collapse = " ")

  ## tweet text
  c(
    '---',
    '',
    sprintf('New in @jstatsoft: %s%s.', x$plaintitle, pkg),
    hashtags,
    '',
    paste('By', aut),
    '',
    paste0('https://doi.org/', x$doi),
    '',
    '---'
  )
}

make_twitter <- function(x = ".", density = 300, ...) {
  ## JSS information
  if(!inherits(x, "jss")) x <- jss::jss(x)
  
  ## convert title page to PNG
  png <- gsub("\\.pdf$", ".png", x$pdf)
  pdf <- magick::image_read(paste0(x$pdf, "[0]"), density = density)
  magick::image_write(image = pdf, format = "png", flatten = TRUE,
    path = if(!file.exists("~/Work")) png else file.path("~/Work", png))
  
  ## tweet text
  twt <- format(x, "Twitter", ...)
  
  ## add to jss.txt file and write on screen
  if(file.exists("~/Work/jss.txt")) cat(
    paste0(c(readLines("~/Work/jss.txt", encoding = "latin1"), twt[-1L]), "\n"),
    sep = "",
    file = file("~/Work/jss.txt", encoding = "latin1")
  )
  writeLines(twt)
  
  invisible(twt)
}
