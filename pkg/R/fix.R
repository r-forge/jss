## JSS: from utils
as.person <-
function(x)
    UseMethod("as.person")

as.person.default <-
function(x)
{
    if(inherits(x, "person")) return(x)

    x <- as.character(x)

    if(!length(x)) return(person())

    ## Need to split the strings into individual person components.
    ## We used to split at ',' and 'and', but of course these could be
    ## contained in roles or comments as well.
    ## Hence, try the following.
    ## A. Replace all comment, role and email substrings by all-z
    ##    substrings of the same length.
    ## B. Tokenize the strings according to the split regexp matches in
    ##    the corresponding z-ified strings.
    ## C. Extract the persons from the thus obtained tokens.

    ## Create strings consisting of a given character c with given
    ## numbers n of characters.
    strings <- function(n, c = "z") {
        vapply(Map(rep.int, rep.int(c, length(n)), n,
                   USE.NAMES = FALSE),
               paste, "", collapse = "")
    }

    ## Replace matches of pattern in x by all-z substrings of the same
    ## length.
    zify <- function(pattern, x) {
        if(!length(x)) return(character())
        m <- gregexpr(pattern, x)
        regmatches(x, m) <-
            Map(strings, lapply(regmatches(x, m), nchar))
        x
    }

    ## Step A.
    y <- zify("\\([^)]*\\)", x)
    y <- zify("\\[[^]]*\\]", y)
    y <- zify("<[^>]*>", y)

    ## Step B.
    pattern <- "[[:space:]]?(,|,?[[:space:]]and)[[:space:]]+"
    x <- do.call("c",
                 regmatches(x, gregexpr(pattern, y), invert = TRUE))
    x <- x[!sapply(x, utils:::.is_not_nonempty_text)] ## for JSS testing with utils:::

    ## <JSS>
    ## don't expect Jr. to be a person
    jr <- which(x %in% c("Jr", "Jr.", "jr", "jr."))
    if(length(jr)) {
      jr <- jr[jr > 1L]
      x[jr - 1L] <- paste(x[jr - 1L], x[jr], sep = ", ")
      x <- x[-jr]
    }
    ## </JSS>

    if(!length(x)) return(person())

    ## Step C.
    as_person1 <- function(x) {
        comment <- if(grepl("\\(.*\\)", x))
            sub(".*\\(([^)]*)\\).*", "\\1", x)
        else NULL
        x <- sub("[[:space:]]*\\([^)]*\\)", "", x)
        email <- if(grepl("<.*>", x))
            sub(".*<([^>]*)>.*", "\\1", x)
        else NULL
        x <- sub("[[:space:]]*<[^>]*>", "", x)
        role <- if(grepl("\\[.*\\]", x))
            unlist(strsplit(gsub("[[:space:]]*", "",
                                 sub(".*\\[([^]]*)\\].*", "\\1", x)),
                            ",", fixed = TRUE))
        else NULL
        x <- sub("[[:space:]]*\\[[^)]*\\]", "", x)
        x <- unlist(strsplit(x, "[[:space:]]+"))
	
	## <JSS>
	## try to correctly guess von/van/de, Jr., etc.
	jr <- c("Jr", "Jr.")
	von <- c("De", "Den", "Der", "La", "Le", "Ten", "Van", "Von")
	family <- x[length(x)]
	given <- x[-length(x)]
	if(!is.null(family) && family %in% c(jr, tolower(jr))) {
	  family <- paste(given[length(given)], family)
	  given <- given[-length(given)]
	}
	if(length(given) && given[length(given)] %in% c(von, tolower(von))) {
	  family <- paste(given[length(given)], family)
	  given <- given[-length(given)]
	}
	if(length(given) && given[length(given)] %in% c(von, tolower(von))) {
	  family <- paste(given[length(given)], family)
	  given <- given[-length(given)]
	}
	## </JSS>
	
        z <- person(given = given, family = family,
                    email = email, role = role, comment = comment)
        return(z)
    }

    as.list(do.call("c", lapply(x, as_person1)))
}

## JSS: from tools, export arguments alone/lower/either/tolower
toTitleCase <- function(text, alone = NULL, lower = NULL, either = NULL, tolower = base::tolower)
{
    ## leave these alone: the internal caps rule would do that
    ## in some cases.  We could insist on this exact capitalization.
    if (is.null(alone)) alone <- c("2D", "3D", "AIC", "BayesX", "GoF", "HTML", "LaTeX",
          "MonetDB", "OpenBUGS", "TeX", "U.S.", "U.S.A.", "WinBUGS",
          "aka", "et", "al.", "ggplot2", "i.e.", "jar", "jars",
          "ncdf", "netCDF", "rgl", "rpart", "xls", "xlsx")
    ## These should be lower case except at the beginning (and after :)
    if (is.null(lower)) lower <- c("a", "an", "and", "are", "as", "at", "be", "but", "by", "en",
	  "for", "if", "in", "is", "nor", "not", "of", "on", "or", "per",
	  "so", "the", "to", "v[.]?", "via", "vs[.]?", "from", "into",
	  "than", "that", "with")
    lpat <- paste0("^(", paste(lower, collapse = "|"), ")$")
    ## These we don't care about
    if(is.null(either)) either <- c("all", "above", "after", "along", "also", "among",
                "any", "both", "can", "few", "it", "less", "log",
                "many", "may", "more", "over", "some", "their",
                "then", "this", "under", "until", "using", "von",
                "when", "where", "which", "will", "without",
                "yet", "you", "your")
    titleCase1 <- function(x) {
        ## A quote might be prepended.
        do1 <- function(x) {
            x1 <- substring(x, 1L, 1L)
            if(nchar(x) >= 3L && x1 %in% c("'", '"'))
                paste0(x1, toupper(substring(x, 2L, 2L)),
                       tolower(substring(x, 3L)))
            else paste0(toupper(x1), tolower(substring(x, 2L)))
        }
	
        xx <- .Call(tools:::C_splitString, x, ' -/"()') ## for JSS testing with tools:::	
	## RSplitString <- function(s) {
        ##   chars <- unlist(strsplit(s, NULL))
        ##   ind <- chars %in% unlist(strsplit(" -/\"()", NULL))
        ##   pos <- cumsum(rle(ind)$lengths)
        ##   substring(s, c(0L, head(pos, -1L)) + 1L, pos)
        ## }
	
        ## for 'alone' we could insist on that exact capitalization
        alone <- xx %in% c(alone, either)
        alone <- alone | grepl("^'.*'$", xx)
        havecaps <- grepl("^[[:alpha:]].*[[:upper:]]+", xx)
        l <- grepl(lpat, xx, ignore.case = TRUE)
        l[1L] <- FALSE
        ## do not remove capitalization immediately after ": " or "- "
        ind <- grep("[-:]$", xx); ind <- ind[ind + 2L <= length(l)]
        ind <- ind[(xx[ind + 1L] == " ") & grepl("^['[:alnum:]]", xx[ind + 2L])]
        l[ind + 2L] <- FALSE
        ## Also after " (e.g. "A Book Title")
        ind <- which(xx == '"'); ind <- ind[ind + 1L <= length(l)]
        l[ind + 1L] <- FALSE
        xx[l] <- tolower(xx[l])
        keep <- havecaps | l | (nchar(xx) == 1L) | alone
        xx[!keep] <- sapply(xx[!keep], do1)
        paste(xx, collapse = "")
    }
    if(typeof(text) != "character")
        stop("'text' must be a character vector")
    sapply(text, titleCase1, USE.NAMES = FALSE)
}
