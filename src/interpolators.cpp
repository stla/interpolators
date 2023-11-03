#include "interpolators_types.h"

// [[Rcpp::export]]
Rcpp::XPtr<ipr_barycentric_rational> ipr_barycentricRational(
  Rcpp::NumericVector x, Rcpp::NumericVector y, size_t order
) {
  std::vector<double> u(x.begin(), x.end());
  std::vector<double> v(y.begin(), y.end());
  ipr_barycentric_rational* ipr_ptr =
    new ipr_barycentric_rational(std::move(u), std::move(v), order);
  Rcpp::XPtr<ipr_barycentric_rational> ipr_xptr(ipr_ptr, false);
  return ipr_xptr;
}

// [[Rcpp::export]]
Rcpp::NumericVector eval_barycentricRational(
    Rcpp::XPtr<ipr_barycentric_rational> ipr_xptr,
    Rcpp::NumericVector x,
    int derivative
) {
  ipr_barycentric_rational ipr = *(ipr_xptr.get());
  int n = x.size();
  Rcpp::NumericVector y(n);
  if(derivative == 0) {
    for(int i = 0; i < n; i++) {
      y(i) = ipr(x(i));
    }
  } else {
    for(int i = 0; i < n; i++) {
      y(i) = ipr.prime(x(i));
    }
  }
  return y;
}

// [[Rcpp::export]]
Rcpp::XPtr<ipr_pchip> ipr_PCHIP(
    Rcpp::NumericVector x, Rcpp::NumericVector y
) {
  std::vector<double> u(x.begin(), x.end());
  std::vector<double> v(y.begin(), y.end());
  ipr_pchip* ipr_ptr = new ipr_pchip(std::move(u), std::move(v));
  Rcpp::XPtr<ipr_pchip> ipr_xptr(ipr_ptr, false);
  return ipr_xptr;
}

// [[Rcpp::export]]
Rcpp::NumericVector eval_PCHIP(
    Rcpp::XPtr<ipr_pchip> ipr_xptr, Rcpp::NumericVector x, int derivative
) {
  ipr_pchip ipr = *(ipr_xptr.get());
  int n = x.size();
  Rcpp::NumericVector y(n);
  if(derivative == 0) {
    for(int i = 0; i < n; i++) {
      y(i) = ipr(x(i));
    }
  } else {
    for(int i = 0; i < n; i++) {
      y(i) = ipr.prime(x(i));
    }
  }
  return y;
}
