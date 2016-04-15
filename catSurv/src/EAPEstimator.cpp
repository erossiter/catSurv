#include "EAPEstimator.h"


template<typename F>
class gsl_function_pp : public gsl_function {
public:
	gsl_function_pp(const F &func) : _func(func) {
		function = &gsl_function_pp::invoke;
		params = this;
	}

private:
	const F &_func;

	static double invoke(double x, void *params) {
		return static_cast<gsl_function_pp *>(params)->_func(x);
	}
};

const double EAPEstimator::estimateTheta(Prior prior) {
	auto numerator_function = [&](double theta, void *params = nullptr) {
		return theta * likelihood(theta) * prior.prior(theta);
	};

	auto denominator_function = [&](double theta, void *params = nullptr) {
		return likelihood(theta) * prior.prior(theta);
	};


	gsl_function_pp<decltype(numerator_function)> Fp(numerator_function);
	gsl_function *n = static_cast<gsl_function *>(&Fp);

	gsl_function_pp<decltype(denominator_function)> Fp2(denominator_function);
	gsl_function *d = static_cast<gsl_function *>(&Fp2);


	const double numerator = integrator.integrate(n, 10);
	const double denominator = integrator.integrate(d, 10);
	return numerator / denominator;
}

const EstimationType EAPEstimator::getIntegrationType() const {
	return EstimationType::EAP;
}