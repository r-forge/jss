format_jss_to_ojs3 <- function(x, article_id = NULL) {
  supplementary_files <- NULL
  supplementary_galleys <- NULL
  if(! x$type %in% c("bookreview", "softwarereview")) {
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
      id <- 2:(1+nsupp)
      files <- mapply(function(y,z) ojs_supplemental_file(z[1], x$directory, y, z[2]), id, supp)
      supplementary_files <- unlist(files)
      galleys <- mapply(function(y,z) ojs_galley(z[2], y), id, supp)
      supplementary_galleys <- unlist(galleys)
    }
  }

  title <- htmlify(x$textitle)
  pages <- sprintf('1 - %s', x$pages)
  abstract <- ojs_abstract(file.path(x$directory, x$pdf), x$type)

  authors <- ojs_author_list(x$person)
  authors_attr <- c('xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                    'xsi:schemaLocation="http://pkp.sfu.ca native.xsd"'
                    )

  pub_id <- sprintf('v%03di%02d',as.numeric(x$volume),as.numeric(x$number))

  article <- c (xml_wrap('id', pub_id, 'type="public" advice="update"'),
                xml_wrap_locale('title', title),
                xml_wrap_locale('abstract', abstract),
                ojs_keywords(x$keywords),
                xml_wrap('comments_to_editor','Generated by jss R package'),
                xml_wrap_list('authors',authors,authors_attr),
                ojs_manuscript(x$pdf,x$directory,1),
                supplementary_files,
                ojs_galley("PDF",1),
                supplementary_galleys,
                # TODO: Permissions. Currently not possible?
                # ojs_permissions(x$person),
                xml_wrap('pages',pages)
                )

  date_submitted <- sprintf('date_submitted="%s"',x$submitdate)

  section <- "ART"
  if (x$type == "bookreview")     section <- "BR"
  if (x$type == "softwarereview") section <- "SR"
  if (x$type == "codesnippet")    section <- "CS"

  article_attr <- c('xmlns="http://pkp.sfu.ca"',
                     'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                     'locale="en_US"',
                     date_submitted,
                     'stage="production"',
                     sprintf('section_ref="%s"', section),
                     'xsi:schemaLocation="http://pkp.sfu.ca native.xsd"'
                     )

  c(ojs_xml_head,
    xml_wrap_list('article',article, article_attr)
    )
}

ojs_xml_head <- '<?xml version="1.0" encoding="UTF-8"?>'

xml_wrap <- function(tag, x, attribute_list = NULL) {
  opn <- paste(c(tag, attribute_list), collapse=" ")
  sprintf('<%s>%s</%s>',opn,x,tag)
}

xml_wrap_list <- function(tag, x, attribute_list = NULL) {
  opn <- paste(c(tag, attribute_list), collapse=" ")
  opn <- sprintf('<%s>',opn)
  cls <- sprintf('</%s>',tag)
  unlist(c(opn,x,cls))
}

xml_wrap_locale <- function(tag, x, attribute_list = NULL)
  xml_wrap(tag, x, c('locale="en_US" ', attribute_list))

str_trim <- function(s) gsub("(^ +)|( +$)", "", s)

ojs_keywords <- function(keywords) {
  if(is.null(keywords) || keywords == "") {
    return(NULL)
  } else {
    kw <- strsplit(keywords,",",fixed=TRUE)
    kw <- lapply(kw, str_trim)
    kw <- lapply(kw,function(s) xml_wrap('keyword',s))
    xml_wrap_list('keywords',kw,'locale="en_US"')
  }
}

ojs_author_list <- function(person) {
  attr <- rep(' include_in_browse="true" user_group_ref="Author"', length(person))
  first <- sapply(listify(person$given), "[", 1L)
  middle <- sapply(listify(person$given), function(x) if(length(x) == 1L) "" else paste(x[-1L], collapse = " "))
  family <- sapply(listify(person$family), paste, collapse = " ")
  email <- sapply(listify(person$email), paste, collapse = " ")
  attr[which(email != "")[1L]] <- ' include_in_browse="true" user_group_ref="Author" primary_contact="true"'
  email[email == ""] <- "no@e-mail.provided"

  first <- tth::tth(first, mode = "hex")
  family <- tth::tth(family, mode = "hex")

  author <- function (first, middle, family, email, attributes) {
    entries <- c(xml_wrap('firstname',first),
                 xml_wrap('middlename',middle),
                 xml_wrap('lastname',family),
                 xml_wrap('email',email)
                 )
    xml_wrap_list('author',entries,attributes)
  }

  authors <- mapply(author, first, middle, family, email, attr)
  unlist(authors)
}

ojs_permissions <- function(person) {
  # OJS2 Style permissions field
  person <- paste(format(person, include = c("given", "family")), collapse = ", ")
  person <- tth::tth(person, mode = "hex")
  perm <- c(xml_wrap_locale('copyright_holder', person),
            xml_wrap('copyright_year',  format(Sys.Date(), "%Y"))
            )
  xml_wrap_list('permissions', perm)
}

ojs_abstract <- function(file, type) {
  if(type %in% c("bookreview", "softwarereview")) return('  <abstract locale="en_US"></abstract>')

  ## ensure a non-C locale
  if(identical(Sys.getlocale(), "C")) {
    Sys.setlocale("LC_ALL", "en_US.UTF-8")
  }

  ## use tm to read PDF
  x <- as.character(tm::Corpus(tm::URISource(file), readerControl = list(reader = tm::readPDF))[[1L]])

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
  x
}

ojs_file <- function(file, dir, id, genre, filetype) {
  file_attr <- c('xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                 'stage="final"',
                 sprintf('id="%d"',id),
                 'xsi:schemaLocation="http://pkp.sfu.ca native.xsd"'
                 )
  rev_attr <- c('number="1"',
                sprintf('genre="%s"',genre),
                sprintf('filename="%s"',file),
                sprintf('filetype="%s"',filetype),
                'filesize="1"', # This tag is required in OJS 3.0.1, but not interpreted?
                'user_group_ref="Author"',
                'uploader="zeileis"', # TODO: OJS username required
                'viewable="false"'
                )
  data <-  base64enc::base64encode(file.path(dir, file))
  embed <- xml_wrap('embed', data, 'encoding="base64"')
  name <- xml_wrap_locale('name',file)
  revision <- xml_wrap_list('revision', c(name,embed), rev_attr)
  xml_wrap_list('submission_file', revision, file_attr)
}

ojs_manuscript <- function(file, dir, id) {
  ojs_file(file,dir,id,"Manuscript (compiled)", "application/pdf")
}

ojs_supplemental_file <- function(file, dir, id, description) {
  # TODO: parse filename and description to find out genre and mimetype
  genre <- "Other"
  filetype <- "n/a"
  ojs_file(file,dir,id,genre,filetype)
}

ojs_galley <- function(name, file_id){
  galley_attr <- c('xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                   'approved="false"',
                   'xsi:schemaLocation="http://pkp.sfu.ca native.xsd"'
                   )
  galley <- c(xml_wrap_locale('name',name),
              xml_wrap('seq', sprintf('%d',file_id)), # Set viewing order of galleys
              sprintf('<submission_file_ref id="%d" revision="1"/>',file_id)
              )
  xml_wrap_list('article_galley', galley, galley_attr)
}
