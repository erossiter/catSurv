#pragma once
#include <string>
#include <vector>
#include <gsl/gsl_math.h>


/**
 * Handles the task of integration, using GSL's adaptive quadrature functions.
 */
class Integrator {
  
public:
	double integrate(const gsl_function *function, const size_t intervals,
	                 const double lower, const double upper) const;
  
};