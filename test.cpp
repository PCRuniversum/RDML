#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
Rcpp::NumericVector convertToDouble(Rcpp::StringVector x) {
  std::vector<double> res;
  double converted_double;
  for(Rcpp::StringVector::iterator it = x.begin(); it != x.end(); ++it) {
    int index = std::distance(x.begin(), it);
    
    std::string temp = Rcpp::as<std::string>(x[index]);
    replace(temp.begin(), temp.end(), ',', '.');
    converted_double = std::stod(temp);
  
    res.push_back(converted_double);
  }
  
  // Convert and return the Rcpp type as desired.
  return Rcpp::wrap(res);
}

/*** R
convertToDouble(c("42,2", "42.2"))
*/

class Uniform { public:
  Uniform(double min_, double max_) : min(min_), max(max_) {}
  NumericVector draw(int n) const {
    RNGScope scope;
    return runif( n, min, max ); }
  double min, max;
};
double uniformRange( Uniform* w) { return w->max - w->min;
}
RCPP_MODULE(unif_module) {
  class_<Uniform>( "Uniform" )
  .constructor<double,double>()
  .field( "min", &Uniform::min )
  .field( "max", &Uniform::max )
  .method( "draw", &Uniform::draw )
  .method( "range", &uniformRange )
  ;
}