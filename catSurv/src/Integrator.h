#pragma once
#include <string>
#include <vector>


enum class IntegrationType {
	Trapezoid, Hermite, QAG
};

class Integrator {
public:
	virtual const double integrate(const std::vector<double> &x, const std::vector<double> &fx) const = 0;
};