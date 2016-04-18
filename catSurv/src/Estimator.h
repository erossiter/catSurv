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
	QuestionSet questionSet;

	double polynomial_likelihood(double theta);

	double binary_likelihood(double theta);

public:
	Estimator(Integrator integration, QuestionSet question);

	virtual EstimationType getIntegrationType() const = 0;

	virtual double estimateTheta(Prior prior) = 0;

	double likelihood(double theta);

	std::vector<double> probability(double theta, int question);

	virtual double estimateSE(Prior prior) = 0;

	virtual double expectedPV(int item, Prior &prior) = 0;


};