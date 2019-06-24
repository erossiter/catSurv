#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _catSurv_checkStopRules(SEXP);
extern SEXP _catSurv_d1LL(SEXP, SEXP, SEXP);
extern SEXP _catSurv_d2LL(SEXP, SEXP, SEXP);
extern SEXP _catSurv_estimateSE(SEXP);
extern SEXP _catSurv_estimateTheta(SEXP);
extern SEXP _catSurv_expectedKL(SEXP, SEXP);
extern SEXP _catSurv_expectedObsInf(SEXP, SEXP);
extern SEXP _catSurv_expectedPV(SEXP, SEXP);
extern SEXP _catSurv_fisherInf(SEXP, SEXP, SEXP);
extern SEXP _catSurv_fisherTestInfo(SEXP, SEXP);
extern SEXP _catSurv_likelihood(SEXP, SEXP);
extern SEXP _catSurv_likelihoodKL(SEXP, SEXP);
extern SEXP _catSurv_lookAhead(SEXP, SEXP);
extern SEXP _catSurv_obsInf(SEXP, SEXP, SEXP);
extern SEXP _catSurv_posteriorKL(SEXP, SEXP);
extern SEXP _catSurv_prior(SEXP, SEXP);
extern SEXP _catSurv_probability(SEXP, SEXP, SEXP);
extern SEXP _catSurv_selectItem(SEXP);


static const R_CallMethodDef CallEntries[] = {
    {"_catSurv_checkStopRules", (DL_FUNC) &_catSurv_checkStopRules, 1},
    {"_catSurv_d1LL",           (DL_FUNC) &_catSurv_d1LL,           3},
    {"_catSurv_d2LL",           (DL_FUNC) &_catSurv_d2LL,           3},
    {"_catSurv_estimateSE",     (DL_FUNC) &_catSurv_estimateSE,     1},
    {"_catSurv_estimateTheta",  (DL_FUNC) &_catSurv_estimateTheta,  1},
    {"_catSurv_expectedKL",     (DL_FUNC) &_catSurv_expectedKL,     2},
    {"_catSurv_expectedObsInf", (DL_FUNC) &_catSurv_expectedObsInf, 2},
    {"_catSurv_expectedPV",     (DL_FUNC) &_catSurv_expectedPV,     2},
    {"_catSurv_fisherInf",      (DL_FUNC) &_catSurv_fisherInf,      3},
    {"_catSurv_fisherTestInfo", (DL_FUNC) &_catSurv_fisherTestInfo, 2},
    {"_catSurv_likelihood",     (DL_FUNC) &_catSurv_likelihood,     2},
    {"_catSurv_likelihoodKL",   (DL_FUNC) &_catSurv_likelihoodKL,   2},
    {"_catSurv_lookAhead",      (DL_FUNC) &_catSurv_lookAhead,      2},
    {"_catSurv_obsInf",         (DL_FUNC) &_catSurv_obsInf,         3},
    {"_catSurv_posteriorKL",    (DL_FUNC) &_catSurv_posteriorKL,    2},
    {"_catSurv_prior",          (DL_FUNC) &_catSurv_prior,          2},
    {"_catSurv_probability",    (DL_FUNC) &_catSurv_probability,    3},
    {"_catSurv_selectItem",     (DL_FUNC) &_catSurv_selectItem,     1},
    {NULL, NULL, 0}
};

void R_init_catSurv(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
