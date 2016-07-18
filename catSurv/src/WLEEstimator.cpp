#include "WLEEstimator.h"
#include "GSLFunctionWrapper.h"

double WLEEstimator::estimateTheta(Prior prior) {
  return 1.0;
}

double WLEEstimator::estimateSE(Prior prior) {
  return 2.0;
}


EstimationType WLEEstimator::getEstimationType() const {
	return EstimationType::WLE;
}

WLEEstimator::WLEEstimator(Integrator &integrator, QuestionSet &questionSet) : Estimator(integrator, questionSet) { }

