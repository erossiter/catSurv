#pragma once
#include <string>
#include <vector>
#include "Integrator.h"
#include "QuestionSet.h"
#include "Prior.h"

enum class EstimationType {
	EAP, MAP
};

class Estimator {
protected:
	const Integrator &integrator;
public:
	Estimator(Integrator integrator) : integrator(integrator) { };

	virtual const EstimationType get_integration_type() const = 0;

	virtual const double estimateTheta(QuestionSet questionSet, Prior prior) = 0;

	double likelihood(double x, std::vector<int> y){
		return 0;
	}
	virtual double estimateSE(QuestionSet questionSet, Prior prior) {
		std::vector<double> fx;
		std::vector<double> fx_theta;

		const double theta_hat = estimateTheta(questionSet, prior);

		for (size_t i = 0; i < questionSet.X.size(); ++i) {
			fx.push_back(likelihood(questionSet.X[i], questionSet.applicable_rows) * prior.values[i]);
			fx_theta.push_back((questionSet.X[i] - theta_hat) * (questionSet.X[i] - theta_hat) * fx[i]);
		}
		return sqrt(integrator.integrate(questionSet.X, fx_theta) / integrator.integrate(questionSet.X, fx));
	}

};