#include <boost/math/distributions/non_central_t.hpp>
#include <boost/math/distributions/uniform.hpp>
#include <boost/math/distributions/normal.hpp>

#include "Prior.h"

using namespace boost::math;

double Prior::dt(double x, int df, double mu) {
  return pdf(non_central_t_distribution<>(df, mu), x);
}

double Prior::uniform(double x, double min, double max) {
  return pdf(uniform_distribution<>(min, max), x);
}

double Prior::normal(double x, double mean, double sd){
  return pdf(normal_distribution<>(mean, sd), x);
}

double Prior::prior(double x) {
  if (name == "STUDENT_T") {
    return dt(x, (int) parameters.at(1), parameters.at(0));
  }
  if (name == "UNIFORM") {
    return uniform(x, parameters.at(0), parameters.at(1));
  }
  if (name == "NORMAL"){
    return normal(x, parameters.at(0), parameters.at(1));
  }
  
  Rcpp::stop("%s is not a valid prior name.", name);
  throw std::invalid_argument("Invalid prior name"); 
}

Prior::Prior(Rcpp::S4 cat_df) : name(Rcpp::as<std::string>(cat_df.slot("priorName"))),
parameters(Rcpp::as<std::vector<double> >(cat_df.slot("priorParams"))) { }

Prior::Prior(const std::string &prior_name, const std::vector<double> &distribution_parameters) : 
  name(prior_name),
  parameters(distribution_parameters) { }