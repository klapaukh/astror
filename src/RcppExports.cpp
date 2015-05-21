// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// gradientDomainHDRCompression
NumericMatrix gradientDomainHDRCompression(NumericMatrix extractedLuminance, double alpha = 0.1, double beta = 0.1, double delta = 1.1, double theta = 0, double epsilon = 0.0001, double saturation = 1);
RcppExport SEXP astror_gradientDomainHDRCompression(SEXP extractedLuminanceSEXP, SEXP alphaSEXP, SEXP betaSEXP, SEXP deltaSEXP, SEXP thetaSEXP, SEXP epsilonSEXP, SEXP saturationSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< NumericMatrix >::type extractedLuminance(extractedLuminanceSEXP );
        Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP );
        Rcpp::traits::input_parameter< double >::type beta(betaSEXP );
        Rcpp::traits::input_parameter< double >::type delta(deltaSEXP );
        Rcpp::traits::input_parameter< double >::type theta(thetaSEXP );
        Rcpp::traits::input_parameter< double >::type epsilon(epsilonSEXP );
        Rcpp::traits::input_parameter< double >::type saturation(saturationSEXP );
        NumericMatrix __result = gradientDomainHDRCompression(extractedLuminance, alpha, beta, delta, theta, epsilon, saturation);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}