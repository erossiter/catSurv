#pragma once
#include <string>
#include <vector>
#include <gsl/gsl_math.h>

class Integrator {
	constexpr static double bound = 6;
public:
	double integrate(const gsl_function *function, const size_t intervals,
	                 const double lower = -bound, const double upper = bound) const;
};