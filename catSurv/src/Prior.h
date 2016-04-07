#pragma once
#include <string>
#include <vector>
#include <Rcpp.h>
#include <boost/math/special_functions/gamma.hpp>
#include <boost/math/distributions/chi_squared.hpp>

using namespace boost::math;

class Prior {
private:
	double dchi(double x, double k);

	double dt(double x, double mu, int df);

public:
	const std::string name;
	const std::vector<double> parameters;
	std::vector<double> values;

	double prior(double x);

	Prior(Rcpp::S4 cat_df);;

	Prior(const std::string &name, const std::vector<double> &parameters);
};

