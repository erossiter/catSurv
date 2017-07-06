#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP catSurv_checkStopRules(SEXP);
extern SEXP catSurv_d1LL(SEXP, SEXP, SEXP);
extern SEXP catSurv_d2LL(SEXP, SEXP, SEXP);
extern SEXP catSurv_estimateSE(SEXP);
extern SEXP catSurv_estimateTheta(SEXP);
extern SEXP catSurv_expectedKL(SEXP, SEXP);
extern SEXP catSurv_expectedObsInf(SEXP, SEXP);
extern SEXP catSurv_expectedPV(SEXP, SEXP);
extern SEXP catSurv_fisherInf(SEXP, SEXP, SEXP);
extern SEXP catSurv_fisherTestInfo(SEXP);
extern SEXP catSurv_likelihood(SEXP, SEXP);
extern SEXP catSurv_likelihoodKL(SEXP, SEXP);
extern SEXP catSurv_lookAhead(SEXP, SEXP);
extern SEXP catSurv_obsInf(SEXP, SEXP, SEXP);
extern SEXP catSurv_posteriorKL(SEXP, SEXP);
extern SEXP catSurv_prior(SEXP, SEXP, SEXP);
extern SEXP catSurv_probability(SEXP, SEXP, SEXP);
extern SEXP catSurv_selectItem(SEXP);
extern SEXP catSurv_estimateThetas(SEXP,SEXP);
extern SEXP catSurv_simulateThetas(SEXP,SEXP);


static const R_CallMethodDef CallEntries[] = {
    {"catSurv_checkStopRules", (DL_FUNC) &catSurv_checkStopRules, 1},
    {"catSurv_d1LL",           (DL_FUNC) &catSurv_d1LL,           3},
    {"catSurv_d2LL",           (DL_FUNC) &catSurv_d2LL,           3},
    {"catSurv_estimateSE",     (DL_FUNC) &catSurv_estimateSE,     1},
    {"catSurv_estimateTheta",  (DL_FUNC) &catSurv_estimateTheta,  1},
    {"catSurv_expectedKL",     (DL_FUNC) &catSurv_expectedKL,     2},
    {"catSurv_expectedObsInf", (DL_FUNC) &catSurv_expectedObsInf, 2},
    {"catSurv_expectedPV",     (DL_FUNC) &catSurv_expectedPV,     2},
    {"catSurv_fisherInf",      (DL_FUNC) &catSurv_fisherInf,      3},
    {"catSurv_fisherTestInfo", (DL_FUNC) &catSurv_fisherTestInfo, 1},
    {"catSurv_likelihood",     (DL_FUNC) &catSurv_likelihood,     2},
    {"catSurv_likelihoodKL",   (DL_FUNC) &catSurv_likelihoodKL,   2},
    {"catSurv_lookAhead",      (DL_FUNC) &catSurv_lookAhead,      2},
    {"catSurv_obsInf",         (DL_FUNC) &catSurv_obsInf,         3},
    {"catSurv_posteriorKL",    (DL_FUNC) &catSurv_posteriorKL,    2},
    {"catSurv_prior",          (DL_FUNC) &catSurv_prior,          3},
    {"catSurv_probability",    (DL_FUNC) &catSurv_probability,    3},
    {"catSurv_selectItem",     (DL_FUNC) &catSurv_selectItem,     1},
    {"catSurv_estimateThetas", (DL_FUNC) &catSurv_estimateThetas, 2},
    {"catSurv_simulateThetas",    (DL_FUNC) &catSurv_simulateThetas,    2},
    {NULL, NULL, 0}
};

void R_init_catSurv(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}