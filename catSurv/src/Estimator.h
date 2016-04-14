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
private:
	static std::vector<double> calculate_probability(double guessing, double discrimination,
	                                                 std::vector<double> difficulty_level, bool poly,
	                                                 double theta);
protected:
	const Integrator &integrator;
	QuestionSet questionSet;

	double polynomial_likelihood(double theta);

	double binary_likelihood(double theta);

public:
	Estimator(Integrator integrator, QuestionSet questionSet);

	virtual const EstimationType getIntegrationType() const = 0;

	virtual const double estimateTheta(Prior prior) = 0;

	double likelihood(double theta);

	std::vector<double> probability(double theta, int question);

	virtual double estimateSE(Prior prior);


};