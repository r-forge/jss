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
      files <- mapply(function(y,z) ojs3_supplemental_file(z[1], x$directory, y, z[2]), id, supp)
      supplementary_files <- unlist(files)
      galleys <- mapply(function(y,z) ojs3_galley(z[2], y), id, supp)
      supplementary_galleys <- unlist(galleys)
    }
  }

  title <- htmlify(x$textitle)
  pages <- sprintf('1 - %s', x$pages)
  abstract <- ojs3_abstract(file.path(x$directory, x$pdf), x$type)
  
  authors <- ojs3_author_list(x$person)
  authors_attr <- c('xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                    'xsi:schemaLocation="http://pkp.sfu.ca native.xsd"'
                    )   

  article <- c (xml_wrap('id', article_id, 'type="internal" advice="update"'),
                xml_wrap('id', x$key["number"], 'type="public" advice="update"'),
                xml_wrap('id', x$doi, 'type="doi" advice="update"'),
                xml_wrap_locale('title', title),
                xml_wrap_locale('abstract', abstract),
                ojs3_copyright(x$person),
                xml_wrap('copyrightYear', format(Sys.Date(), "%Y")),
                ojs3_keywords(x$keywords),
                xml_wrap_list('authors',authors,authors_attr),
                ojs3_manuscript(x$pdf,x$directory,1),
                supplementary_files,
                ojs3_galley("PDF",1),
                supplementary_galleys,
                ojs3_issue(x$year, x$volume),
                xml_wrap('pages',pages)
                )

  date_submitted <- sprintf('date_submitted="%s"',x$submitdate)
  date_published <- sprintf('date_published="%s"',Sys.Date())
  
  section <- "ART"
  if (x$type == "bookreview")     section <- "BR"
  if (x$type == "softwarereview") section <- "SR"
  if (x$type == "codesnippet")    section <- "CS"

  article_attr <- c('xmlns="http://pkp.sfu.ca"',
                     'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                     'locale="en_US"',
                      date_submitted,
                     'stage="production"',
                      date_published,
                     sprintf('section_ref="%s"', section),
                     'xsi:schemaLocation="http://pkp.sfu.ca native.xsd"'
                     )

  c(ojs3_xml_head,
    xml_wrap_list('article',article, article_attr)
    )
}

ojs3_xml_head <- '<?xml version="1.0" encoding="UTF-8"?>'

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

ojs3_keywords <- function(keywords) {
  if(is.null(keywords) || keywords == "") {
    return(NULL)
  } else {
    kw <- strsplit(keywords,",",fixed=TRUE)
    kw <- lapply(kw, str_trim)
    kw <- lapply(kw,function(s) xml_wrap('keyword',s))
    xml_wrap_list('keywords',kw,'locale="en_US"')
  }
}

ojs3_author_list <- function(person) {
  attr <- rep(' include_in_browse="true" user_group_ref="Author"', length(person))
  given <- sapply(listify(person$given), paste, collapse = " ")
  family <- sapply(listify(person$family), paste, collapse = " ")
  email <- sapply(listify(person$email), paste, collapse = " ")
  primary <- which(email != "")[1L]
  attr[primary] <- paste(attr[primary], 'primary_contact="true"')
  email[email == ""] <- "no@e-mail.provided"

  given <- tth::tth(given, mode = "hex")
  family <- tth::tth(family, mode = "hex")

  author <- function (given, family, email, attributes) {
    entries <- c(xml_wrap('givenname', given),
                 xml_wrap('familyname', family),
                 xml_wrap('email', email)
                 )
    xml_wrap_list('author',entries,attributes)
  }

  authors <- mapply(author, given, family, email, attr)
  unlist(authors)
}

ojs3_copyright <- function(person){
  person <- paste(format(person, include = c("given", "family")), collapse = ", ")
  person <- tth::tth(person, mode = "hex")
  xml_wrap_locale('copyrightHolder', person)
}


ojs3_abstract <- function(file, type) {
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
  x
}

ojs3_file <- function(file, dir, id, genre, filetype, uploader = NULL) {
  file_attr <- c('xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                 'stage="final"',
                 sprintf('id="%d"',id),
                 'xsi:schemaLocation="http://pkp.sfu.ca native.xsd"'
                 )
  rev_attr <- c('number="1"',
                sprintf('genre="%s"',genre),
                sprintf('filename="%s"',file),
                sprintf('filetype="%s"',filetype),
                if(!is.null(uploader)) sprintf('uploader="%s"', uploader),
                'viewable="false"'
                )
  data <-  base64enc::base64encode(file.path(dir, file))
  embed <- xml_wrap('embed', data, 'encoding="base64"')
  name <- xml_wrap_locale('name',file)
  revision <- xml_wrap_list('revision', c(name,embed), rev_attr)
  xml_wrap_list('submission_file', revision, file_attr)
}

ojs3_manuscript <- function(file, dir, id) {
  ojs3_file(file,dir,id,"Manuscript (PDF)", "application/pdf")
}

ojs3_supplemental_file <- function(file, dir, id, description) {
  # FIXME: parse filename and descripion to find out filetype
  genre <- ojs3_genre(description)
  filetype <- "n/a"
  ojs3_file(file, dir, id, genre, filetype)
}

ojs3_genre <- function(description){
  if(grepl("replication",description)){return("Replication Material")}
  if(grepl("source", description)){return("Software")}
  return("Other")
}

ojs3_filetype <- function(file){
  # package mime needed:
  # return(guess_type(file, mime_extra = NULL, unknown =  "n/a"))
  # without mime: 
  # if(file_ext(file)=="py"){return("text/x-python")}
  # if(file_ext(file)=="c"){return("text/x-csrc")}
  # if(file_ext(file)=="csv"){return("text/comma-separated-values")}
  # else{return("n/a")}
}

ojs3_galley <- function(name, file_id){
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

ojs3_issue <- function(year, volume){
  entries <- c(xml_wrap('volume', volume),
               xml_wrap('year', year))
  xml_wrap_list('issue_identification', entries)
  
}
