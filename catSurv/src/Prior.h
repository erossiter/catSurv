#pragma once
#include <string>
#include <vector>
#include <Rcpp.h>

class Prior {
private:
	double dchi(double x, double k);

	double dt(double x, int df);

public:
	const std::string name;
	const std::vector<double> parameters;
	std::vector<double> values;

	double prior(double x);

	Prior(Rcpp::S4 cat_df);

	Prior(const std::string &prior_name, const std::vector<double> &distribution_parameters);
};

