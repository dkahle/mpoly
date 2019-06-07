#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _mvp_simplify(SEXP, SEXP, SEXP);
extern SEXP _mvp_mvp_prod(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mvp_mvp_add(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mvp_mvp_power(SEXP, SEXP, SEXP, SEXP);
extern SEXP _mvp_mvp_deriv(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_mvp_simplify",   (DL_FUNC) &_mvp_simplify, 3},
    {"_mvp_mvp_prod",   (DL_FUNC) &_mvp_simplify, 6},
    {"_mvp_mvp_add",   (DL_FUNC) &_mvp_simplify, 6},
    {"_mvp_mvp_power",   (DL_FUNC) &_mvp_simplify, 4},
    {"_mvp_mvp_deriv",   (DL_FUNC) &_mvp_simplify, 4},
    {NULL, NULL, 0}
};

void R_init_spray(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
