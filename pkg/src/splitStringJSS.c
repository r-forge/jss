/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003-2025   The R Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#include <string.h>
#include <R.h>
#include <Rinternals.h>

/* renamed copy from tools */
SEXP splitStringJSS(SEXP string, SEXP delims)
{
    if(!isString(string) || length(string) != 1)
	error("first arg must be a single character string");
    if(!isString(delims) || length(delims) != 1)
	error("first arg must be a single character string");

    if(STRING_ELT(string, 0) == NA_STRING)
	return ScalarString(NA_STRING);
    if(STRING_ELT(delims, 0) == NA_STRING)
	return ScalarString(NA_STRING);

    const char *in = CHAR(STRING_ELT(string, 0)),
	*del = CHAR(STRING_ELT(delims, 0));
    cetype_t ienc = getCharCE(STRING_ELT(string, 0));
    int nc = (int) strlen(in), used = 0;

    // Used for short strings, so OK to over-allocate wildly
    SEXP out = PROTECT(allocVector(STRSXP, nc)), ans;

    // UBSAN objects if nc = 0, but we can skip that case.
    if (nc > 0) {
	char tmp[nc], *this = tmp;
	int nthis = 0;
	const char *p;
	for(p = in; *p ; p++) {
	    if(strchr(del, *p)) {
		// put out current string (if any)
		if(nthis)
		    SET_STRING_ELT(out, used++, mkCharLenCE(tmp, nthis, ienc));
		// put out delimiter
		SET_STRING_ELT(out, used++, mkCharLen(p, 1));
		// restart
		this = tmp; nthis = 0;
	    } else {
		*this++ = *p;
		nthis++;
	    }
	}
	if(nthis) SET_STRING_ELT(out, used++, mkCharLenCE(tmp, nthis, ienc));
	
	ans = lengthgets(out, used);
    } else
	ans = out;
    UNPROTECT(1);
    return ans;
}
