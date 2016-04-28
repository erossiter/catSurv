#include "EAPEstimator.h"

double EAPEstimator::estimateTheta(Prior prior) {

	/**
	 * Because these denominator and numerator functions
	 * are used nowhere else, it makes sense to give them
	 * the smallest scope possible. As a result, they are
	 * declared as lambdas. They capture by reference because
	 * they need access to prior and likelihood, but, because
	 * they are passed to GSL's integration function, cannot
	 * take prior and likelihood as arguments.
	 */

	integrableFunction denominator = [&](double theta) {
		return likelihood(theta) * prior.prior(theta);
	};

	integrableFunction numerator = [&](double theta) {
		return theta * denominator(theta);
	};

	return integralQuotient(numerator, denominator);
}

EstimationType EAPEstimator::getEstimationType() const {
	return EstimationType::EAP;
}

EAPEstimator::EAPEstimator(Integrator &integrator, QuestionSet &questionSet) : Estimator(integrator, questionSet) { }

