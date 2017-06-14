#include "Integrator.h"
#include <gsl/gsl_integration.h>
#include <gsl/gsl_errno.h>
#include <stdexcept>

double Integrator::integrate(const gsl_function *function, const size_t intervals,
                             const double lower, const double upper) const {
	gsl_integration_workspace *workspace = gsl_integration_workspace_alloc(intervals);
	// Malloc returns a null pointer if there is insufficient memory available
	// If the workspace allocator returns that null pointer, nothing below
	// will work - it will rely on dereferencing a null pointer.
	if (workspace == nullptr) {
		// No error message is permitted when throwing bad_alloc,
		// because it needs to be able to be constructed without
		// using any additional memory
		throw std::bad_alloc();
	}

	double result, absolute_error;
	const int integration_method = GSL_INTEG_GAUSS61;
	const double absolute_error_limit = GSL_SQRT_DBL_EPSILON;
	const double relative_error_limit = GSL_SQRT_DBL_EPSILON;


	int error_code = gsl_integration_qag(function, lower, upper, relative_error_limit, absolute_error_limit,
	                                     intervals, integration_method, workspace, &result, &absolute_error);

	// Always ensure that any allocated memory is freed
	gsl_integration_workspace_free(workspace);

	if (error_code != GSL_SUCCESS) {
		const char *error_message = gsl_strerror(error_code);
		throw std::runtime_error(error_message);
	}

	return result;
}

