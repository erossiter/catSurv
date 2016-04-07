#include "Prior.h"

double Prior::dchi(double x, double k) {
	return pdf(chi_squared(k), x);
}

double Prior::dt(double x, double mu, int df) {
	double dnorm_value = dnorm4(Rcpp::NumericVector::create(x), 0.0, true)[0];
	return (dchi(x, df) + mu) / std::sqrt(dnorm_value / df);
}

double Prior::prior(double x) {
	if (name == "normal") {
		return dnorm4(Rcpp::NumericVector::create(x), parameters[0], (bool) (parameters[1]))[0];
	}
	return dt(x, parameters[0], (int) parameters[1]);
}

Prior::Prior(Rcpp::S4 cat_df) : name(Rcpp::as<std::string>(cat_df.slot("priorName"))),
                                parameters(Rcpp::as<std::vector<double> >(cat_df.slot("priorParams"))) {
	for (auto i : (Rcpp::List) cat_df.slot("X")) {
		values.push_back(prior(i));
	}

}

Prior::Prior(const std::string &name, const std::vector<double> &parameters) : name(
		name), parameters(parameters) { }