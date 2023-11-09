#include <Rcpp.h>
#include <array>
#include <boost/math/interpolators/barycentric_rational.hpp>
#include <boost/math/interpolators/catmull_rom.hpp>
#include <boost/math/interpolators/makima.hpp>
#include <boost/math/interpolators/pchip.hpp>


typedef boost::math::interpolators::barycentric_rational<double>
  ipr_barycentric_rational;
typedef boost::math::catmull_rom<std::array<double, 2>>
  ipr_catmull_rom2;
typedef boost::math::catmull_rom<std::array<double, 3>>
  ipr_catmull_rom3;
typedef boost::math::interpolators::makima<std::vector<double>>
  ipr_makima;
typedef boost::math::interpolators::pchip<std::vector<double>>
  ipr_pchip;
