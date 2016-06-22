#pragma once
#include <string>
#include <vector>
#include <gsl/gsl_math.h>


/**
 * Handles the task of integration, using GSL's adaptive quadrature functions.
 */
class Integrator {
	/**
	 * The likelihood of a value outside of the range [-6,6] is vanishingly small,
	 * so 6 is set as the bound.
	 */
	constexpr static double bound = 6;
  
public:
	double integrate(const gsl_function *function, const size_t intervals,
	                 const double lower = -bound, const double upper = bound) const;
  
  int solve_root();
  
  //double solve_root(const gsl_function *function, double lower, double upper) const;
};

struct quadratic_params
  {
    double a, b, c;
  };

double quadratic (double x, void *params);
double quadratic_deriv (double x, void *params);
void quadratic_fdf (double x, void *params, double *y, double *dy);