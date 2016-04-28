#include <boost/math/distributions/non_central_t.hpp>
#include "Prior.h"

using namespace boost::math;

double Prior::dt(double x, int df, double mu) {
	return pdf(non_central_t_distribution<>(df, mu), x);
  
}

double Prior::prior(double x) {
	if (name == "NORMAL") {
		return dnorm4(Rcpp::NumericVector::create(x), parameters[0], parameters[1], 0)[0];
	}
	return dt(x, (int) parameters[1], parameters[0]);
}

Prior::Prior(Rcpp::S4 cat_df) : name(Rcpp::as<std::string>(cat_df.slot("priorName"))),
                                parameters(Rcpp::as<std::vector<double> >(cat_df.slot("priorParams"))),
                                values(Rcpp::as<std::vector<double> >(cat_df.slot("X"))) { }

Prior::Prior(const std::string &prior_name, const std::vector<double> &distribution_parameters) : name(
		prior_name), parameters(distribution_parameters) { }