print_doi_batch_id <- function(file) {
    x <- as.POSIXlt(Sys.time())
    cat("<doi_batch_id>", paste(x$year, x$mon, x$mday, x$hour, x$min, round(x$sec), sep=""), "</doi_batch_id>\n", sep="", file=file,
        append=TRUE)
}
print_timestamp <- function(file) {
    x <- as.POSIXlt(Sys.time())
    cat("<timestamp>", paste(x$year, x$mon, x$mday, x$hour, x$min, round(x$sec), sep=""), "</timestamp>\n", sep="", file=file,
        append=TRUE)
}
print_depositor <- function(file) {
    cat("<depositor> \n
			<depositor_name>Journal of Statistical Software</depositor_name> \n
			<email_address>doi@jstatsoft.org</email_address> \n
                            </depositor>\n", file=file, append=TRUE)
}
print_journal_metadata <- function(file, type) {
    if(type == "article") {
        tit <- "Journal of Statistical Software"
        atit <- "J. Stat. Soft."
    }
    else if(type == "bookreview") {
        tit <- "Journal of Statistical Software, Book Reviews"
        atit <- "J. Stat. Soft., B. Rev."
    }
    else if(type == "softwarereview") {
        tit <- "Journal of Statistical Software, Software Reviews"
        atit <- "J. Stat. Soft., S. Rev."
    }
    else if(type == "codesnippet") {
        tit <- "Journal of Statistical Software, Code Snippets"
        atit <- "J. Stat. Soft., C. Snip."
    }
        
    cat("<journal_metadata language=\"en\"> \n
	          <full_title>", tit, "</full_title> \n
				<abbrev_title>", atit, "</abbrev_title> \n
				<issn media_type=\"electronic\">1548-7660</issn> \n
				<coden>JSSOBK</coden> \n
	</journal_metadata>\n\n", file=file, append=TRUE)
}
print_journal_issue <- function(x, file) {
    cat(paste("<journal_issue> \n
				<publication_date media_type=\"online\"> \n
				<year>", x$year, "</year> \n
				</publication_date> \n
				<journal_volume> \n
					<volume>", x$volume, "</volume> \n
				</journal_volume> \n
				<issue>", x$number, "</issue> \n
			</journal_issue>\n\n", sep=""), file=file, append=TRUE)
}
get_authors <- function(x) {
    for(i in 1:length(x$author)) {
        if(i == 1)
            str <- paste("<person_name sequence=\"first\" contributor_role=\"author\"> \n
						<given_name>", paste(x$author[[i]]$given, collapse=" "), "</given_name> \n
						<surname>", x$author[[i]]$family, "</surname> \n
					</person_name>\n", sep="")
        else
            str <- paste(str, "<person_name sequence=\"additional\" contributor_role=\"author\"> \n
						<given_name>", paste(x$author[[i]]$given, collapse=" "), "</given_name> \n
						<surname>", x$author[[i]]$family, "</surname> \n
					</person_name>\n", sep="")
    }
    str
}
get_doi <- function(x) {
    prefix <- "10.18637/"
    if(nchar(x$volume) == 2)
        v <- "v0"
    else v <- "v" 
    suffix <- paste("jss.", v, x$volume, ".i", x$number, sep="")
    paste(prefix,suffix, sep="")
}
strip_title <- function(x) {
    x <- gsub("\\\\proglang", "", x)
    x <- gsub("\\\\pkg", "", x)
    x <- gsub("\\\\code", "", x)
    return(x)
}
print_journal_article <- function(x, file) {
    cat(paste("<journal_article publication_type=\"full_text\"> \n
				<titles> \n
					<title>", strip_title(x$title),
              "</title> \n
				</titles> \n
				<contributors> \n", get_authors(x), 
				"</contributors> \n
				<publication_date media_type=\"online\"> \n 
					<year>", x$year,"</year>\n 
				</publication_date> \n
				<publisher_item>
					<identifier id_type=\"doi\">", get_doi(x), "</identifier> \n
				</publisher_item> \n
				<doi_data>\n
					<doi>", get_doi(x),"</doi> \n
					<resource>", x$url,"</resource>\n
				</doi_data>\n
			</journal_article>\n", sep=""), file=file, append=TRUE)
}
newDeposit <- function(file, type="article", out="out.xml") {
    ## possible types are bookreview, article, codesnippet, softwarereview
    x <- bibtex::read.bib(file)
    cat("<?xml version=\"1.0\" encoding=\"UTF-8\"?> \n
<doi_batch version=\"4.3.4\" xmlns=\"http://www.crossref.org/schema/4.3.4\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.crossref.org/schema/4.3.4 http://www.crossref.org/schema/deposit/crossref4.3.4.xsd\"> \n
<head>", file=out)
    print_doi_batch_id(out)
    print_timestamp(out)
    print_depositor(out)
    cat("<registrant>CrossRef</registrant> \n
	</head>\n
	<body>\n
            <journal>\n", file=out, append=TRUE)
   
    print_journal_metadata(out, type)
    ## this generates xml for all bib items stored in a bib file
    ## do not put multiple journal titles in one bib file for deposit
    for(i in 1:length(x)) 
        print_journal_article(x[[i]], out)
    cat("</journal> \n
	</body> \n 
       </doi_batch>", file=out, append=TRUE)
    ## once we have a password, login we can test via
    ## http://test.crossref.org, see http://help.crossref.org/verifying_your_xml
    ## make real deposits with https://doi.crossref.org/servlet/deposit
    system(paste("curl -F \'operation=doMDUpload\' -F \'login_id=fast\' -F \'login_passwd=fast_827\' -F \'fname=@", out, "\' ", "https://test.crossref.org/servlet/deposit", sep=""))
                       
}
