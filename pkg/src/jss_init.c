#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP splitStringJSS(SEXP string, SEXP delims);

static R_CallMethodDef callMethods[] = {
  {"splitStringJSS", (DL_FUNC) &splitStringJSS, 2},
  {NULL, NULL, 0}
};

void R_init_jss(DllInfo* info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}

