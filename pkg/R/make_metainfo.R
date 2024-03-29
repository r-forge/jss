make_jss_bib <- function(dir = "/home/zeileis/Work/JSS/Publications", file = "jss.bib",
  keyprefix = "jss:", keytype = "number", ...)
{
  dir <- unlist(lapply(file.path(dir, base::dir(dir, pattern = "v")), function(d) {
    vd <- base::dir(d, pattern = "v")
    vd <- vd[order(gsub("i", "a", vd))]
    file.path(d, vd)
  }))
  rval <- sapply(dir, function(d) format(jss(d),
    "BibTeX", keyprefix = keyprefix, keytype = keytype, ...))
  if(is.null(file) || identical(file, FALSE)) {
    return(rval)
  } else {
    writeLines(rbind(rval, ""), file)
    invisible(rval)
  }
}

make_metainfo <- function(x = ".", ...) {
  if(!inherits(x, "jss")) x <- jss(x)  
  tpt <- dir(x$directory, pattern = "\\.tpt$")  
  if(length(tpt) > 0L) file.remove(tpt)
  for(i in dir(x$directory, pattern = "\\.pdf$")  ) compactPDF(i)
  make_citation(x)
  make_readme(x)
  #make_crossref(x, deposit = FALSE)
  invisible(x)
}

make_citation <- function(x = ".", ...) {
  if(!inherits(x, "jss")) x <- jss(x)  
  ct <- format(x, "CITATION")
  if(!is.null(ct)) writeLines(ct, file.path(x$directory, "CITATION"))
  invisible(ct)
}

make_readme <- function(x = ".", ...) {
  if(!inherits(x, "jss")) x <- jss(x)  
  rd <- format(x, "README")
  if(!is.null(rd)) writeLines(rd, file.path(x$directory, "README.txt"))
  invisible(rd)
}

make_crossref <- function(x = ".", ..., deposit = TRUE) {
  ## JSS info
  x <- if(inherits(x, "jss")) list(x) else lapply(x, jss)

  ## output file
  file <- if(length(x) > 1L) "CROSSREF.xml" else file.path(x[[1L]]$directory, "CROSSREF.xml")

  ## XML code
  x <- lapply(x, format, style = "CrossRef")
  x <- c(cr_head(), unlist(x), cr_foot())

  ## at the moment: just create file
  writeLines(x, file)

  if(deposit) {
    lp <- try(readLines(file.path(Sys.getenv("HOME"), ".crossref")))
    if(inherits(lp, "try-error") || length(lp) != 2L) deposit <- FALSE
  }
  if(!deposit) {
    lp <- c("<id>", "<passwd>")
  }

  ## test via http://test.crossref.org (see http://help.crossref.org/verifying_your_xml)
  ## make real deposits with https://doi.crossref.org/servlet/deposit
  ## curl -F 'operation=doMDUpload' -F 'login_id=<id>' -F 'login_passwd=<passwd>' -F 'fname=@CROSSREF.xml' https://test.crossref.org/servlet/deposit
  cmd <- sprintf("curl -F 'operation=doMDUpload' -F 'login_id=%s' -F 'login_passwd=%s' -F 'fname=@CROSSREF.xml' https://doi.crossref.org/servlet/deposit",
    lp[1L], lp[2L])
  if(deposit) {
    system(cmd)
    file.remove("CROSSREF.xml")
  } else {
    writeLines(c("To deposit with CrossRef:", cmd))
  }

  invisible(x)
}

make_ojs <- function(x = ".", ...) {
  if(!inherits(x, "jss")) x <- jss(x)  
  ojs <- format(x, "OJS", ...)
  if(!is.null(ojs)) writeLines(ojs, file.path(x$directory, "OJS.txt"))
  invisible(ojs)
}

make_ojs2 <- function(x = ".", ...) {
  if(!inherits(x, "jss")) x <- jss(x)  
  ojs <- format(x, "OJS2", ...)
  if(!is.null(ojs)) writeLines(ojs, file.path(x$directory, "OJS.xml"))
  invisible(ojs)
}

make_ojs3 <- function(x = ".", ...) {
  if(!inherits(x, "jss")) x <- jss(x)  
  ojs <- format(x, "OJS3", ...)
  if(!is.null(ojs)) writeLines(ojs, file.path(x$directory, "OJS.xml"))
  invisible(ojs)
}
