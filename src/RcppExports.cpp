// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "interpolators_types.h"
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// ipr_barycentricRational
Rcpp::XPtr<ipr_barycentric_rational> ipr_barycentricRational(Rcpp::NumericVector x, Rcpp::NumericVector y, size_t order);
RcppExport SEXP _interpolators_ipr_barycentricRational(SEXP xSEXP, SEXP ySEXP, SEXP orderSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< size_t >::type order(orderSEXP);
    rcpp_result_gen = Rcpp::wrap(ipr_barycentricRational(x, y, order));
    return rcpp_result_gen;
END_RCPP
}
// eval_barycentricRational
Rcpp::NumericVector eval_barycentricRational(Rcpp::XPtr<ipr_barycentric_rational> ipr_xptr, Rcpp::NumericVector x, int derivative);
RcppExport SEXP _interpolators_eval_barycentricRational(SEXP ipr_xptrSEXP, SEXP xSEXP, SEXP derivativeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::XPtr<ipr_barycentric_rational> >::type ipr_xptr(ipr_xptrSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type derivative(derivativeSEXP);
    rcpp_result_gen = Rcpp::wrap(eval_barycentricRational(ipr_xptr, x, derivative));
    return rcpp_result_gen;
END_RCPP
}
// ipr_catmullRom
Rcpp::XPtr<ipr_catmull_rom> ipr_catmullRom(Rcpp::NumericMatrix X, bool closed, double alpha);
RcppExport SEXP _interpolators_ipr_catmullRom(SEXP XSEXP, SEXP closedSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type X(XSEXP);
    Rcpp::traits::input_parameter< bool >::type closed(closedSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(ipr_catmullRom(X, closed, alpha));
    return rcpp_result_gen;
END_RCPP
}
// eval_catmullRom
Rcpp::NumericMatrix eval_catmullRom(Rcpp::XPtr<ipr_catmull_rom> ipr_xptr, Rcpp::NumericVector s, int derivative);
RcppExport SEXP _interpolators_eval_catmullRom(SEXP ipr_xptrSEXP, SEXP sSEXP, SEXP derivativeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::XPtr<ipr_catmull_rom> >::type ipr_xptr(ipr_xptrSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type s(sSEXP);
    Rcpp::traits::input_parameter< int >::type derivative(derivativeSEXP);
    rcpp_result_gen = Rcpp::wrap(eval_catmullRom(ipr_xptr, s, derivative));
    return rcpp_result_gen;
END_RCPP
}
// ipr_Makima
Rcpp::XPtr<ipr_makima> ipr_Makima(Rcpp::NumericVector x, Rcpp::NumericVector y);
RcppExport SEXP _interpolators_ipr_Makima(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(ipr_Makima(x, y));
    return rcpp_result_gen;
END_RCPP
}
// eval_makima
Rcpp::NumericVector eval_makima(Rcpp::XPtr<ipr_makima> ipr_xptr, Rcpp::NumericVector x, int derivative);
RcppExport SEXP _interpolators_eval_makima(SEXP ipr_xptrSEXP, SEXP xSEXP, SEXP derivativeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::XPtr<ipr_makima> >::type ipr_xptr(ipr_xptrSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type derivative(derivativeSEXP);
    rcpp_result_gen = Rcpp::wrap(eval_makima(ipr_xptr, x, derivative));
    return rcpp_result_gen;
END_RCPP
}
// ipr_PCHIP
Rcpp::XPtr<ipr_pchip> ipr_PCHIP(Rcpp::NumericVector x, Rcpp::NumericVector y);
RcppExport SEXP _interpolators_ipr_PCHIP(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(ipr_PCHIP(x, y));
    return rcpp_result_gen;
END_RCPP
}
// eval_PCHIP
Rcpp::NumericVector eval_PCHIP(Rcpp::XPtr<ipr_pchip> ipr_xptr, Rcpp::NumericVector x, int derivative);
RcppExport SEXP _interpolators_eval_PCHIP(SEXP ipr_xptrSEXP, SEXP xSEXP, SEXP derivativeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::XPtr<ipr_pchip> >::type ipr_xptr(ipr_xptrSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type derivative(derivativeSEXP);
    rcpp_result_gen = Rcpp::wrap(eval_PCHIP(ipr_xptr, x, derivative));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_interpolators_ipr_barycentricRational", (DL_FUNC) &_interpolators_ipr_barycentricRational, 3},
    {"_interpolators_eval_barycentricRational", (DL_FUNC) &_interpolators_eval_barycentricRational, 3},
    {"_interpolators_ipr_catmullRom", (DL_FUNC) &_interpolators_ipr_catmullRom, 3},
    {"_interpolators_eval_catmullRom", (DL_FUNC) &_interpolators_eval_catmullRom, 3},
    {"_interpolators_ipr_Makima", (DL_FUNC) &_interpolators_ipr_Makima, 2},
    {"_interpolators_eval_makima", (DL_FUNC) &_interpolators_eval_makima, 3},
    {"_interpolators_ipr_PCHIP", (DL_FUNC) &_interpolators_ipr_PCHIP, 2},
    {"_interpolators_eval_PCHIP", (DL_FUNC) &_interpolators_eval_PCHIP, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_interpolators(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
