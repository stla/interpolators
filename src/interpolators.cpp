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
Rcpp::XPtr<ipr_catmull_rom> ipr_catmullRom(
    Rcpp::NumericMatrix X, bool closed, double alpha
) {
  const int nrow = X.nrow();
  const int dim = X.ncol();
  std::vector<std::array<double, 2>> points(nrow);
  for(int i = 0; i < nrow; i++) {
    std::array<double, 2> pti;
    for(int j = 0; j < dim; j++) {
      pti[j] = X(i, j);
    }
    points[i] = pti;
  }
  ipr_catmull_rom* ipr_ptr =
    new ipr_catmull_rom(std::move(points), closed, alpha);
  Rcpp::XPtr<ipr_catmull_rom> ipr_xptr(ipr_ptr, false);
  return ipr_xptr;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix eval_catmullRom(
    Rcpp::XPtr<ipr_catmull_rom> ipr_xptr,
    Rcpp::NumericVector s,
    int derivative
) {
  ipr_catmull_rom ipr = *(ipr_xptr.get());
  const int n = s.size();
  //std::vector<double> ipr0 = ipr(0.0);
  const int dim = 2;
  Rcpp::Rcout << dim << "\n";
  const double max_s = ipr.max_parameter();
  Rcpp::Rcout << max_s << "\n";
  Rcpp::NumericMatrix Y(n, dim);
  if(derivative == 0) {
    for(int i = 0; i < n; i++) {
      std::array<double, 2> yi = ipr(s(i) * max_s);
      Rcpp::Rcout << yi[0] << "\n";
      Rcpp::NumericVector Yi = Rcpp::NumericVector::create(yi[0], yi[1]);
      Y(i, Rcpp::_) = Yi;
    }
  } else {
    for(int i = 0; i < n; i++) {
      std::array<double, 2> yi = ipr.prime(s(i) * max_s);
      Rcpp::NumericVector Yi(yi.begin(), yi.end());
      Y(i, Rcpp::_) = Yi;
    }
  }
  return Y;
}

// [[Rcpp::export]]
Rcpp::XPtr<ipr_makima> ipr_Makima(
    Rcpp::NumericVector x, Rcpp::NumericVector y
) {
  std::vector<double> u(x.begin(), x.end());
  std::vector<double> v(y.begin(), y.end());
  ipr_makima* ipr_ptr = new ipr_makima(std::move(u), std::move(v));
  Rcpp::XPtr<ipr_makima> ipr_xptr(ipr_ptr, false);
  return ipr_xptr;
}

// [[Rcpp::export]]
Rcpp::NumericVector eval_makima(
    Rcpp::XPtr<ipr_makima> ipr_xptr, Rcpp::NumericVector x, int derivative
) {
  ipr_makima ipr = *(ipr_xptr.get());
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
