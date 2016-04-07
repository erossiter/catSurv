#include "TrapezoidIntegrator.h"

const IntegrationType virtual TrapezoidIntegrator::get_integration_type() const {
	return IntegrationType::Trapezoid;
}

const double virtual TrapezoidIntegrator::integrate(std::vector<double> &x, std::vector<double> &fx) const {
	double val = 0;
	for (unsigned int i = 0; i < x.size() - 1; ++i) {
		val += (x[i + 1] - x[i]) * (fx[i + 1] + fx[i]) / 2.0;
	}
	return val;
}