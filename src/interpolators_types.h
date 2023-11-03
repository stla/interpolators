#include <Rcpp.h>
#include <boost/math/interpolators/barycentric_rational.hpp>
#include <boost/math/interpolators/pchip.hpp>

typedef boost::math::interpolators::barycentric_rational<double>
        ipr_barycentric_rational;
typedef boost::math::interpolators::pchip<std::vector<double>>
        ipr_pchip;
