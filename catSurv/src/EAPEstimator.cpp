#include "EAPEstimator.h"

double EAPEstimator::estimateTheta(Prior prior) {

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

