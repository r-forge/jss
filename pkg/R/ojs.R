format_jss_to_ojs <- function(x, article_id = NULL)
{
  ## supplemental files
  if(x$type %in% c("bookreview", "softwarereview")) {
    supp <- NULL
  } else {
    supp <- file.path(x$directory, "README.txt")
    supp <- if(file.exists(supp)) readLines(supp) else format(x, "README")
    if(length(supp) >= 6L) {
      nsupp <- length(supp) - 5L
      if(any(supp[-(1L:5L)] == "")) nsupp <- min(which(supp[-(1L:5L)] == "")) - 1L
    } else {
      nsupp <- 0L
    }
    if(nsupp > 0L) {
      supp <- strsplit(supp[6L:(5L + nsupp)], "\\:[[:space:]]+")
      supp <- lapply(supp, function(z) ojs_supplemental_file(z[1], z[2], x$directory))
      supp <- unlist(supp)
    } else {
      supp <- NULL
    }
  }

  c(ojs_head(x$key[1L], x$doi, x$textitle, article_id),
    ojs_abstract(file.path(x$directory, x$pdf), x$type),
    ojs_indexing(x$keywords),
    '',

    ojs_author(x$person),

    '',
    sprintf('  <pages>1 - %s</pages>', x$pages),
    sprintf('  <date_submitted>%s</date_submitted>', x$submitdate),
    sprintf('  <date_published>%s</date_published>', Sys.Date()),

    ojs_permissions(x$person),
    '',    

    ojs_galley(x$pdf, x$directory),
    supp,
    
    '</article>'
  )
}

ojs_indexing <- function(keywords) {
  if(is.null(keywords) || keywords == "") {
    return(NULL)
  } else {
    c('  <indexing>',
      sprintf('    <subject locale="en_US">%s</subject>', gsub(", ", "; ", keywords, fixed = TRUE)),
      '  </indexing>'
    )
  }
}

ojs_author <- function(person) {
  attr <- rep("", length(person))
  first <- sapply(listify(person$given), "[", 1L)
  middle <- sapply(listify(person$given), function(x) if(length(x) == 1L) "" else paste(x[-1L], collapse = " "))
  family <- sapply(listify(person$family), paste, collapse = " ")
  email <- sapply(listify(person$email), paste, collapse = " ")
  attr[which(email != "")[1L]] <- ' primary_contact="true"'
  email[email == ""] <- "no@e-mail.provided"
  sprintf(
    '  <author%s>\n    <firstname>%s</firstname>\n    <middlename>%s</middlename>\n    <lastname>%s</lastname>\n    <email>%s</email>\n  </author>',
    attr, tth::tth(first, mode = "hex"), middle, tth::tth(family, mode = "hex"), email
  )
}

ojs_permissions <- function(person) {
  person <- paste(format(person, include = c("given", "family")), collapse = ", ")
  person <- tth::tth(person, mode = "hex")
  c('  <permissions>',
    sprintf('    <copyright_holder locale="en_US">%s</copyright_holder>', person),
    sprintf('    <copyright_year>%s</copyright_year>', format(Sys.Date(), "%Y")),
    '  </permissions>'
  )
}


ojs_head <- function(key, doi, title, article_id) {
  article_id <- if(is.null(article_id)) "" else sprintf('article_id="%s" ', article_id)
  c('<?xml version="1.0" encoding="UTF-8"?>',
    '<!DOCTYPE article PUBLIC "-//PKP//OJS Articles and Issues XML//EN" "http://pkp.sfu.ca/ojs/dtds/2.4.6/native.dtd">',
    sprintf('<article locale="en_US" public_id="%s" %slanguage="en">', key, article_id),
    '',
    sprintf('  <id type="doi">%s</id>', doi),
    sprintf('  <title locale="en_US">%s</title>', htmlify(title))
  )
}

ojs_abstract <- function(file, type) {
  if(type %in% c("bookreview", "softwarereview")) return('  <abstract locale="en_US"></abstract>')

  ## ensure a non-C locale
  if(identical(Sys.getlocale(), "C")) {
    Sys.setlocale("LC_ALL", "en_US.UTF-8")
  }

  ## use tm to read PDF
  x <- as.character(tm::Corpus(tm::URISource(file), readerControl = list(reader = tm::readPDF(engine = "xpdf")))[[1L]])

  ## extract abstract
  st <- which(x == "Abstract")[1L] + 1L
  en <- which(substr(x, 1L, 9L) == "Keywords:") - 1L
  while(x[st] == "") st <- st + 1L
  while(x[en] == "") en <- en - 1L
  x <- paste(x[st:en], collapse = " ")

  ## fixup UTF-8 quotes and hyphens
  fix <- rbind(
    c("\303\227", "x"),
    c("\342\210\222", "-"),
    c("\342\200\223", " - "),
    c("\342\200\224", " - "),
    c("\342\200\230", "'"),
    c("\342\200\231", "'"),
    c("\342\200\234", "\""),
    c("\342\200\235", "\"")
  )
  for(i in 1:nrow(fix)) x <- gsub(fix[i, 1], fix[i, 2], x, fixed = TRUE)

  sprintf('  <abstract locale="en_US">%s</abstract>', x)
}

ojs_supplemental_file <- function(file, description, dir) {
  c(sprintf('  <supplemental_file type="other" public_id="%s" language="en" show_reviewers="true">', file),
    sprintf('    <title locale="en_US">%s</title>', description),
    ## '    <description locale="en_US">To be described</description>',
    sprintf('    <date_created>%s</date_created>', Sys.Date()),
    sprintf('    <file><embed filename="%s" encoding="base64" mime_type="SP">%s</embed></file>', file,
      base64enc::base64encode(file.path(dir, file))),
    '  </supplemental_file>',
    ''
  )
}

ojs_galley <- function(file, dir) {
  c(sprintf('  <galley locale="en_US" public_id="%s">', file),
    '    <label>PDF</label>',
    sprintf('    <file><embed filename="%s" encoding="base64" mime_type="PB">%s</embed></file>', file,
      base64enc::base64encode(file.path(dir, file))),
    '  </galley>',
    ''
  )
}
