#include <Rcpp.h>
#include <boost/math/interpolators/barycentric_rational.hpp>

typedef boost::math::interpolators::barycentric_rational<double>
    ipr_barycentric_rational;
