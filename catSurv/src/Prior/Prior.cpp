#include "Prior.h"

double Prior::dchi(double x, double k) {
	return pdf(chi_squared(k), x);
}

double Prior::dt(double x, double mu, int df) {
	return (dchi(x, df) + mu) / std::sqrt(dnorm(x, 0, 1) / df);
}

double Prior::prior(double x, std::string name, std::vector<double> params) {
	if (name == "normal") {
		return dnorm(x, params[0], params[1]);
	}
	return dt(x, params[0], int(params[1]));
}