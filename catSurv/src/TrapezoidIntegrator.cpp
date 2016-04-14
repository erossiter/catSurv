#include "TrapezoidIntegrator.h"


const double TrapezoidIntegrator::integrate(const std::vector<double> &x, const std::vector<double> &fx) const {
	double val = 0;
	for (size_t i = 0; i < x.size() - 1; ++i) {
		val += (x[i + 1] - x[i]) * (fx[i + 1] + fx[i]) / 2.0;
	}
	return val;
}
