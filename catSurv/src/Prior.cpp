#include <boost/math/distributions/non_central_t.hpp>
#include <boost/math/distributions/uniform.hpp>
#include "Prior.h"

using namespace boost::math;

double Prior::dt(double x, int df, double mu) {
	return pdf(non_central_t_distribution<>(df, mu), x);
}

double Prior::uniform(double x, double min, double max) {
	return pdf(uniform_distribution<>(min, max), x);
}

double Prior::prior(double x) {
  if (name == "STUDENT_T") {
	  return dt(x, (int) parameters[1], parameters[0]);
	}
	else if (name == "UNIFORM") {
	  return uniform(x, parameters[0], parameters[1]);
	}
	else {
		return dnorm4(Rcpp::NumericVector::create(x), parameters[0], parameters[1], 0)[0];
	}
}

Prior::Prior(Rcpp::S4 cat_df) : name(Rcpp::as<std::string>(cat_df.slot("priorName"))),
                                parameters(Rcpp::as<std::vector<double> >(cat_df.slot("priorParams"))) { }

Prior::Prior(const std::string &prior_name, const std::vector<double> &distribution_parameters) : 
  name(prior_name),
  parameters(distribution_parameters) { }