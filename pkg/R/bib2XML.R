print_doi_batch_id <- function(file) {
    x <- as.POSIXlt(Sys.time())
    mon <- if(nchar(x$mon) == 1) paste("0", x$mon, sep="") else x$mon
    mday <- if(nchar(x$mday) == 1) paste("0", x$mday, sep="") else x$mday
    hour <-  if(nchar(x$hour) == 1) paste("0", x$hour, sep="") else x$hour
    min <-  if(nchar(x$min) == 1) paste("0", x$min, sep="") else x$min
    sec <-  if(nchar(round(x$sec)) == 1) paste("0", round(x$sec), sep="") else round(x$sec)
    cat("<doi_batch_id>", paste(x$year, mon, mday, hour, min, sec, sep=""), "</doi_batch_id>\n",
        sep="", file=file,
        append=TRUE)
}
print_timestamp <- function(file) {
    x <- as.POSIXlt(Sys.time())
    mon <- if(nchar(x$mon) == 1) paste("0", x$mon, sep="") else x$mon
    mday <- if(nchar(x$mday) == 1) paste("0", x$mday, sep="") else x$mday
    hour <-  if(nchar(x$hour) == 1) paste("0", x$hour, sep="") else x$hour
    min <-  if(nchar(x$min) == 1) paste("0", x$min, sep="") else x$min
    sec <-  if(nchar(round(x$sec)) == 1) paste("0", round(x$sec), sep="") else round(x$sec)
    cat("<timestamp>", paste(x$year, mon, mday, hour, min, sec, sep=""), "</timestamp>\n",
        sep="", file=file,
        append=TRUE)
}
print_depositor <- function(file) {
    cat("<depositor> \n
  <depositor_name>Foundation for Open Access Statistics</depositor_name> \n
			<email_address>doi@foastat.org</email_address> \n
                            </depositor>\n", file=file, append=TRUE)
}
print_journal_metadata <- function(file) {
    tit <- "Journal of Statistical Software"
    atit <- "J. Stat. Soft."
    cat("<journal_metadata language=\"en\"> \n
	          <full_title>", tit, "</full_title> \n
			<abbrev_title>", atit, "</abbrev_title> \n
			<issn media_type=\"electronic\">1548-7660</issn> \n
			<coden>JSSOBK</coden> \n
	</journal_metadata>\n\n", sep = "", file = file, append = TRUE)
}
print_journal_issue <- function(x, type, file) {
    sf <- switch(as.character(type),
                 article = "",
                 codesnippet = "Code Snippet ",
                 bookreview = "Book Review ",
                 softwarereview = "Software Review ",
                stop("type should be: article, codesnippet, bookreview or softwarereview"))   
    cat(paste("<journal_issue> \n
				<publication_date media_type=\"online\"> \n
				<year>", x$year, "</year> \n
				</publication_date> \n
				<journal_volume> \n
					<volume>", x$volume, "</volume> \n
				</journal_volume> \n
				<issue>", paste(sf, x$number, sep=""),
              "</issue> \n
			</journal_issue>\n\n", sep=""), file=file, append=TRUE)
}
get_authors <- function(x) {
    for(i in 1:length(x$author)) {
        if(i == 1)
            str <- paste("<person_name sequence=\"first\"
contributor_role=\"author\"> \n <given_name>",
                         tth::tth(paste(x$author[[i]]$given, collapse=" ")),
                         "</given_name> \n
			 <surname>",
                         tth::tth(x$author[[i]]$family), "</surname> \n
					</person_name>\n", sep="")
        else
            str <- paste(str, "<person_name sequence=\"additional\"
contributor_role=\"author\"> \n <given_name>",
                         tth::tth(paste(x$author[[i]]$given, collapse=" ")),
                         "</given_name> \n <surname>",
                         tth::tth(x$author[[i]]$family), "</surname> \n
					</person_name>\n", sep="")
    }
    str
}
get_doi <- function(x, type) {
    prefix <- "10.18637/"
    v <- switch(nchar(x$volume), "v00", "v0", "v")
    i <- switch(as.character(type), article = ".i", codesnippet = ".c",
                bookreview = ".b", softwarereview = ".s",
                stop("type should be: article, codesnippet, bookreview or softwarereview"))
    n <- switch(nchar(x$number), "0", "")
    if(is.null(v) || is.null(n)) stop("Problem with spec. of vol. or issue.")
    suffix <- paste("jss.", v, x$volume, i, n, x$number, sep="")
    paste(prefix,suffix, sep="")
}
print_journal_article <- function(x, type, file) {
    cat(paste("<journal_article publication_type=\"full_text\"> \n
				<titles> \n
				<title>", htmlify(x$title),
              "</title> \n
				</titles> \n
				<contributors> \n", get_authors(x), 
				"</contributors> \n
				<publication_date media_type=\"online\"> \n 
					<year>", x$year,"</year>\n 
				</publication_date> \n
				<publisher_item>
					<identifier id_type=\"doi\">",
              get_doi(x, type), "</identifier> \n
				</publisher_item> \n
				<doi_data>\n
					<doi>", get_doi(x, type),"</doi> \n
					<resource>", x$url,"</resource>\n
				</doi_data>\n
			</journal_article>\n", sep=""), file=file, append=TRUE)

}
newDeposit <- function(file, type="article", out="out.xml") {
    ## possible types are bookreview, article, codesnippet, softwarereview
    x <- bibtex::read.bib(file)
    cat("<?xml version=\"1.0\" encoding=\"UTF-8\"?> \n
<doi_batch version=\"4.3.4\" xmlns=\"http://www.crossref.org/schema/4.3.4\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.crossref.org/schema/4.3.4 http://www.crossref.org/schema/deposit/crossref4.3.4.xsd\"  xmlns:mml=\"http://www.w3.org/1998/Math/MathML\"> \n
<head>", file=out)
    print_doi_batch_id(out)
    print_timestamp(out)
    print_depositor(out)
    cat("<registrant>CrossRef</registrant> \n </head>\n
	<body>\n", file=out, append=TRUE)
    ## this generates xml for all bib items stored in a bib file
    for(i in 1:length(x)) {
        cat("<journal>\n", file=out, append=TRUE)
        print_journal_metadata(out)
        print_journal_issue(x[[i]], type, out)
        print_journal_article(x[[i]], type, out)
        cat("</journal> \n", file=out, append=TRUE)
    }
    cat("</body> \n  </doi_batch>", file=out, append=TRUE)
    ## test via
    ## http://test.crossref.org, see http://help.crossref.org/verifying_your_xml
    ## make real deposits with https://doi.crossref.org/servlet/deposit
    
    #system(paste("curl -F \'operation=doMDUpload\' -F \'login_id=fast\' -F \'login_passwd=fast_827\' -F \'fname=@", out, "\' ", "https://test.crossref.org/servlet/deposit", sep=""))

    system(paste("curl -F \'operation=doMDUpload\' -F \'login_id=fast\' -F \'login_passwd=fast_827\' -F \'fname=@", out, "\' ", "https://doi.crossref.org/servlet/deposit", sep=""))
                       
}
