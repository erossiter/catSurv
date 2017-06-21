#include "EAPEstimator.h"
#include "GSLFunctionWrapper.h"

GSLFunctionWrapper::GSLFunctionWrapper(std::function<double(double)> const &func) : _func(func) {
	function = &GSLFunctionWrapper::invoke;
	params = this;
}

double GSLFunctionWrapper::invoke(double x, void *params) {
	return static_cast<GSLFunctionWrapper *>(params)->_func(x);
}

gsl_function *GSLFunctionWrapper::asGSLFunction() {
	return static_cast<gsl_function*>(this);
}
