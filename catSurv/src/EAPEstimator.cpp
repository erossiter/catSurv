#include "EAPEstimator.h"

const double EAPEstimator::estimateTheta(QuestionSet questionSet, Prior prior) {
	size_t x_size = questionSet.X.size();
	std::vector<double> fx(x_size);
	std::vector<double> fx_x(x_size);

	for (size_t i = 0; i < questionSet.X.size(); ++i) {
		fx.push_back(likelihood(questionSet.X[i], questionSet.applicable_rows) * prior.values[i]);
		fx_x.push_back(questionSet.X[i] * fx[i]);
	}

	return integrator.integrate(questionSet.X, fx_x) / integrator.integrate(questionSet.X, fx);
}

const EstimationType EAPEstimator::get_integration_type() const {
	return EstimationType::EAP;
}