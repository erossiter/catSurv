#pragma once
#include <string>
#include <vector>
#include "Integrator.h"
#include "QuestionSet.h"
#include "Prior.h"

enum class EstimationType {
	NONE, EAP, MAP
};



class Estimator {
	constexpr static int integrationSubintervals = 10;

	double polytomous_posterior_variance(int item, Prior &prior);

	double binary_posterior_variance(int item, Prior &prior);

protected:
	const Integrator &integrator;
	QuestionSet questionSet;

	double polynomial_likelihood(double theta);

	double binary_likelihood(double theta);

	typedef std::function<double(double)> integrableFunction;

	/**
	* Computes the quotient of the integrals of the functions provided
	* - that is, it computes: ∫(numerator) / ∫(denominator).
	*/
	double integralQuotient(const integrableFunction &numerator,
	                        const integrableFunction &denominator);

public:
	Estimator(Integrator integration, QuestionSet question);

	virtual EstimationType getEstimationType() const;

	virtual double estimateTheta(Prior prior);

	double likelihood(double theta);

	std::vector<double> probability(double theta, int question);


	double estimateSE(Prior prior);

	virtual double expectedPV(int item, Prior &prior);
};