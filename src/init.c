#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP Cdist_sinusoidal(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Cdo_is_within(SEXP, SEXP, SEXP);
extern SEXP Cdo_minmax(SEXP, SEXP);
extern SEXP Cengrid(SEXP, SEXP, SEXP);
extern SEXP Cfirst_non_na_dbl(SEXP, SEXP);
extern SEXP Chaversine_dist(SEXP, SEXP, SEXP, SEXP);
extern SEXP Cis_sorted2(SEXP, SEXP, SEXP);
extern SEXP CSinusoidal(SEXP, SEXP, SEXP);
extern SEXP Ctest_sum_identities(SEXP, SEXP, SEXP);
extern SEXP do_which_within(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP is_within_pixels(SEXP, SEXP, SEXP, SEXP);
extern SEXP Z4P(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"Cdist_sinusoidal",     (DL_FUNC) &Cdist_sinusoidal,     5},
    {"Cdo_is_within",        (DL_FUNC) &Cdo_is_within,        3},
    {"Cdo_minmax",           (DL_FUNC) &Cdo_minmax,           2},
    {"Cengrid",              (DL_FUNC) &Cengrid,              3},
    {"Cfirst_non_na_dbl",    (DL_FUNC) &Cfirst_non_na_dbl,    2},
    {"Chaversine_dist",      (DL_FUNC) &Chaversine_dist,      4},
    {"Cis_sorted2",          (DL_FUNC) &Cis_sorted2,          3},
    {"CSinusoidal",          (DL_FUNC) &CSinusoidal,          3},
    {"Ctest_sum_identities", (DL_FUNC) &Ctest_sum_identities, 3},
    {"do_which_within",      (DL_FUNC) &do_which_within,      5},
    {"is_within_pixels",     (DL_FUNC) &is_within_pixels,     4},
    {"Z4P",                  (DL_FUNC) &Z4P,                  3},
    {NULL, NULL, 0}
};

void R_init_whichWithin(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}