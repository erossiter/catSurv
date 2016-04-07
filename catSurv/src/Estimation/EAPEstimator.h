#pragma once
#include "Estimator.h"
#include "../Prior/Prior.h"

class EAPEstimator : public Estimator {
private:


public:

	EAPEstimator(Integrator integrator) : Estimator(integrator) { };

	virtual const EstimationType get_integration_type() const override {
		return EstimationType::EAP;
	}

	virtual const double estimateTheta(QuestionSet questionSet, Prior prior) override {
		size_t x_size = questionSet.X.size();
		std::vector<double> fx(x_size);
		std::vector<double> fx_x(x_size);

		for (size_t i = 0; i < questionSet.X.size(); ++i) {
			fx.push_back(likelihood(questionSet.X[i], questionSet.applicable_rows) * prior.values[i]);
			fx_x.push_back(questionSet.X[i] * fx[i]);
		}
		return integrator.integrate(questionSet.X, fx_x) / integrator.integrate(questionSet.X, fx);
	}


};

// MAP Estimator
//
//double theta_hat_old = 0.0, theta_hat_new = 1.0;
//double tolerance = 0.0000001;
//double difference = std::abs(theta_hat_new - theta_hat_old);
//while (difference > tolerance) {
//theta_hat_new = theta_hat_old - (dLL(cat, theta_hat_old, true) / d2LL(cat, theta_hat_old, true));
//difference = std::abs(theta_hat_new - theta_hat_old);
//theta_hat_old = theta_hat_new;
//}
//results = theta_hat_new;