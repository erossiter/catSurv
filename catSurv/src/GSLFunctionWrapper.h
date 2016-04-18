#include <gsl/gsl_integration.h>
#include <functional>
#pragma once

/**
 * GSL's integration library requires a function that conforms
 * to gsl_function. This template class enables lambdas which
 * would otherwise meet the requirements to be used as gsl_functions.
 * If this ends up being too slow, std::function can be replaced with a
 * template class.
 */

class GSLFunctionWrapper : public gsl_function {
public:
	GSLFunctionWrapper(std::function<double(double)> const &func);

	gsl_function * asGSLFunction();

private:
	std::function<double(double)> _func;

	static double invoke(double x, void *params);
};