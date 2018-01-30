// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// singleCCDr
List singleCCDr(NumericVector cors, List init_betas, NumericVector init_sigmas, unsigned int nn, double lambda, NumericVector params, IntegerVector blocks, int verbose);
RcppExport SEXP _nicer_singleCCDr(SEXP corsSEXP, SEXP init_betasSEXP, SEXP init_sigmasSEXP, SEXP nnSEXP, SEXP lambdaSEXP, SEXP paramsSEXP, SEXP blocksSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type cors(corsSEXP);
    Rcpp::traits::input_parameter< List >::type init_betas(init_betasSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type init_sigmas(init_sigmasSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type nn(nnSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type params(paramsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type blocks(blocksSEXP);
    Rcpp::traits::input_parameter< int >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(singleCCDr(cors, init_betas, init_sigmas, nn, lambda, params, blocks, verbose));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_nicer_singleCCDr", (DL_FUNC) &_nicer_singleCCDr, 8},
    {NULL, NULL, 0}
};

RcppExport void R_init_nicer(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
