format_jss_to_crossref <- function(x) {
  c('  <journal>',
    cr_journal_metadata(),
    '',
    cr_journal_issue(x$year, x$volume, x$number, x$type),
    '',
    cr_journal_article(x$title, x$person, x$year, x$doi, x$url),
    '  </journal>',
    ''
  )
}

htmlify <- function(x, collapse = " ") {   
  ## replace JSS markup by generic LaTeX
  tab <- rbind(
    c("\\proglang", "\\textsf"),
    c("\\pkg", "\\textbf"),
    c("\\code", "\\texttt"),
    c("\\bm", "")
  )
  for(i in 1:nrow(tab)) x <- gsub(tab[i,1], tab[i,2], x, fixed = TRUE)

  ## transform to HTML
  x <- tth::ttm(x, mode = "hex")
  if(is.character(collapse)) x <- paste(x, collapse = collapse)

  ## fix up MathML markup
  if(length(grep("<math", x)) > 0) {
      xx <- strsplit(x, "<math")[[1]]
      xx[1] <- paste(xx[1], "<mml:math")
      for(i in 2:length(xx)) {
	  xL <- strsplit(xx[i], "</math>")
	  if(length(xL[[1]]) == 2)  {# there is something after the mathml
	      p1 <- xL[[1]][1]
	      p2 <- paste("</mml:math>", xL[[1]][2])
	  }
	  else {
	      p1 <- xx[i]
	      p2 <- ""
	  }
	  p1 <- gsub("</", "!!!/mml:", p1)
	  p1 <- gsub("<", "<mml:", p1)
	  p1 <- gsub("!!!", "<", p1)
	  xx[i] <- paste(p1, p2)
	  if(i < length(xx))
	      xx[i] <- paste(xx[i], "<mml:math")       
      }
      x <- paste(xx, collapse=" ")
  }
  
  ## fix up <font face="helvetica">
  x <- gsub("<font face=\"helvetica\">", "<i>", x, fixed = TRUE)
  x <- gsub("</font>", "</i>", x, fixed = TRUE)

  return(x) 
}

cr_head <- function(id = format(Sys.time(), "%Y%m%d%H%M%S")) {
  c('<doi_batch version="4.3.4" schemaLocation="http://www.crossref.org/schema/4.3.4 http://www.crossref.org/schema/deposit/crossref4.3.4.xsd" xmlns="http://www.crossref.org/schema/4.3.4" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:mml="http://www.w3.org/1998/Math/MathML">',
    '<head>',
    sprintf('  <doi_batch_id>%s</doi_batch_id>', id),
    sprintf('  <timestamp>%s</timestamp>', id),
    '  <depositor>',
    '    <depositor_name>Foundation for Open Access Statistics</depositor_name>',
    '    <email_address>doi@foastat.org</email_address>',
    '  </depositor>',
    '  <registrant>CrossRef</registrant>',
    '</head>',
    '<body>',
    ''
  )
}

cr_foot <- function() c('</body>', '</doi_batch>')

cr_journal_metadata <- function()
  c('    <journal_metadata language="en">',
    '      <full_title>Journal of Statistical Software</full_title>',
    '      <abbrev_title>J. Stat. Soft.</abbrev_title>',
    '      <issn media_type="electronic">1548-7660</issn>',
    '      <coden>JSSOBK</coden>',
    '    </journal_metadata>'
  )

cr_journal_issue <- function(year, volume, number, type) {
  number <- switch(as.character(type),
    "article" = number,
    "codesnippet" = paste("Code Snippet", number),
    "bookreview" = paste("Book Review", number),
    "softwarereview" = paste("Software Review", number),
  )   
  c('    <journal_issue>',
    sprintf('      <publication_date media_type="online"><year>%s</year></publication_date>', year),
    sprintf('      <journal_volume><volume>%s</volume></journal_volume>', volume),
    sprintf('      <issue>%s</issue>', number),
    '    </journal_issue>'
  )
}

cr_journal_article <- function(title, person, year, doi, url) {
  c('    <journal_article publication_type="full_text">',
    sprintf('      <titles><title>%s</title></titles>', htmlify(title)),
    '      <contributors>',
    cr_person_name(person),
    '      </contributors>',
    '',
    sprintf('      <publication_date media_type="online"><year>%s</year></publication_date>', year),
    sprintf('      <publisher_item><identifier id_type="doi">%s</identifier></publisher_item>', doi),
    '      <doi_data>',
    sprintf('       <doi>%s</doi>', doi),
    sprintf('       <resource>%s</resource>', url),
    '      </doi_data>',
    '    </journal_article>'
  )
}

listify <- function(x) if(is.list(x)) x else list(x)

cr_person_name <- function(person) {
  sequence <- rep(c("first", "additional"), c(1L, length(person) - 1L))
  given <- sapply(listify(person$given), paste, collapse = " ")
  family <- sapply(listify(person$family), paste, collapse = " ")
  sprintf(
    '        <person_name sequence="%s" contributor_role="author">\n          <given_name>%s</given_name>\n          <surname>%s</surname>\n        </person_name>',
    sequence, tth::tth(given, mode = "hex"), tth::tth(family, mode = "hex")
  )
}
