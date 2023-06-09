// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// delaunay_cpp
Rcpp::List delaunay_cpp(Rcpp::NumericMatrix pts, double radius, Rcpp::NumericVector O, int niter);
RcppExport SEXP _sphereTessellation_delaunay_cpp(SEXP ptsSEXP, SEXP radiusSEXP, SEXP OSEXP, SEXP niterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type pts(ptsSEXP);
    Rcpp::traits::input_parameter< double >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type O(OSEXP);
    Rcpp::traits::input_parameter< int >::type niter(niterSEXP);
    rcpp_result_gen = Rcpp::wrap(delaunay_cpp(pts, radius, O, niter));
    return rcpp_result_gen;
END_RCPP
}
// sTriangle
Rcpp::List sTriangle(Rcpp::NumericVector A, Rcpp::NumericVector B, Rcpp::NumericVector C, double radius, Rcpp::NumericVector O, int iterations);
RcppExport SEXP _sphereTessellation_sTriangle(SEXP ASEXP, SEXP BSEXP, SEXP CSEXP, SEXP radiusSEXP, SEXP OSEXP, SEXP iterationsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type A(ASEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type B(BSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type C(CSEXP);
    Rcpp::traits::input_parameter< double >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type O(OSEXP);
    Rcpp::traits::input_parameter< int >::type iterations(iterationsSEXP);
    rcpp_result_gen = Rcpp::wrap(sTriangle(A, B, C, radius, O, iterations));
    return rcpp_result_gen;
END_RCPP
}
// voronoi_cpp
Rcpp::List voronoi_cpp(Rcpp::NumericMatrix pts, double radius, Rcpp::NumericVector O, int niter);
RcppExport SEXP _sphereTessellation_voronoi_cpp(SEXP ptsSEXP, SEXP radiusSEXP, SEXP OSEXP, SEXP niterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type pts(ptsSEXP);
    Rcpp::traits::input_parameter< double >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type O(OSEXP);
    Rcpp::traits::input_parameter< int >::type niter(niterSEXP);
    rcpp_result_gen = Rcpp::wrap(voronoi_cpp(pts, radius, O, niter));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_sphereTessellation_delaunay_cpp", (DL_FUNC) &_sphereTessellation_delaunay_cpp, 4},
    {"_sphereTessellation_sTriangle", (DL_FUNC) &_sphereTessellation_sTriangle, 6},
    {"_sphereTessellation_voronoi_cpp", (DL_FUNC) &_sphereTessellation_voronoi_cpp, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_sphereTessellation(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
