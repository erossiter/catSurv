#pragma once
#include <string>
#include <vector>


enum class IntegrationType {
	Trapezoid, Hermite, QAG
};

class Integrator {
public:
	virtual const double integrate(std::vector<double> &x, std::vector<double> &fx) const = 0;
};