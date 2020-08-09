make_jss_cls <- function(dir = ".", pdf = FALSE)
{
  ## directory handling
  td <- tempfile()
  dir.create(td)
  od <- getwd()
  if(dir == ".") dir <- od
  setwd(td)
  on.exit(unlink(td))
  on.exit(setwd(od), add = TRUE)

  ## get and compile .dtx
  dtx <- system.file("style", "jss.dtx", package = "jss")
  file.copy(dtx, "jss.dtx")  
  system("pdflatex jss.dtx", ignore.stdout = TRUE)
  system("pdflatex jss.ins", ignore.stdout = TRUE)
  if(pdf) system("pdflatex jss.dtx", ignore.stdout = TRUE)
  
  ## copy output back
  if(pdf) file.copy("jss.pdf", dir)
  invisible(file.copy("jss.cls", dir))
}

make_jss_bst <- function(dir = ".")
{
  ## directory handling
  td <- tempfile()
  dir.create(td)
  od <- getwd()
  if(dir == ".") dir <- od
  setwd(td)
  on.exit(unlink(td))
  on.exit(setwd(od), add = TRUE)

  ## get and compile .dbj
  dbj <- system.file("style", "jss.dbj", package = "jss")
  file.copy(dbj, "jss.dbj")  
  system("pdflatex jss.dbj", ignore.stdout = TRUE)
  fix_bst_license("jss.bst")
  
  ## copy output back
  invisible(file.copy("jss.bst", dir))
}

fix_bst_license <- function(bst = "jss.bst") {
  file <- bst
  bst <- readLines(bst)
  wi <- c(
    grep("jstatsoft", bst, fixed = TRUE) - 1L,
    grep("======", bst, fixed = TRUE)[2L]
  )
  bst <- append(bst[-(wi[1L]:wi[2L])], after = wi[1L] - 1L, values = c(
    "%%",
    "%% ** BibTeX style file for JSS publications (http://www.jstatsoft.org/)",
    "%%",
    "%% License: GPL-2 | GPL-3",
    " % ===============================================================",
    " % IMPORTANT NOTICE:",
    " % This bibliographic style (bst) file has been generated from one or",
    " % more master bibliographic style (mbs) files, listed above, provided",
    " % with kind permission of Patrick W Daly.",
    " %",
    " % This generated file can be redistributed and/or modified under the terms",
    " % of the General Public License (Version 2 or 3).",
    " % ===============================================================")
  )
  writeLines(bst, file)
  invisible(bst)
}

make_jss_style <- function(dir = ".")
{
  ## directory handling
  td <- tempfile()
  dir.create(td)
  od <- getwd()
  if(dir == ".") dir <- od
  setwd(td)
  on.exit(unlink(td))
  on.exit(setwd(od), add = TRUE)

  ## all JSS style guide files
  sty <- c("README.txt", "article.tex", "codesnippet.tex", "bookreview.tex", "softwarereview.tex",
    "jsslogo.jpg", "jss.dtx", "jss.cls", "jss.pdf", "jss.bst")

  ## copy static files
  file.copy(system.file("style", sty, package = "jss"), ".")

  ## generate .bst, .cls, .pdf
  make_jss_cls(pdf = TRUE)
  make_jss_bst()

  ## zip everything
  zip("jss-style.zip", sty)
  invisible(file.copy("jss-style.zip", dir))
}
