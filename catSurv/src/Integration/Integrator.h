#pragma once
#include <string>
#include <vector>

enum class IntegrationType {
	Trapezoid, Hermite, QAG
};

class Integrator {
public:
	virtual const IntegrationType get_integration_type() const = 0;

	virtual const double integrate(std::vector<double> &x, std::vector<double> &fx) const = 0;
};