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
	constexpr static int integrationSubintervals = 10;

	double polytomous_posterior_variance(int item, Prior &prior);

	double binary_posterior_variance(int item, Prior &prior);

protected:
	const Integrator &integrator;
	QuestionSet &questionSet;

	std::vector<double> paddedProbability(double theta, size_t question);

	double polytomous_likelihood(double theta);

	double binary_likelihood(double theta);

	typedef std::function<double(double)> integrableFunction;

	/**
	* Computes the quotient of the integrals of the functions provided
	* - that is, it computes: ∫(numerator) / ∫(denominator).
	*/
	double integralQuotient(const integrableFunction &numerator,
	                        const integrableFunction &denominator);

public:
	Estimator(Integrator &integration, QuestionSet &question);

	virtual EstimationType getEstimationType() const = 0;

	virtual double estimateTheta(Prior prior) = 0;

	double likelihood(double theta);

	std::vector<double> probability(double theta, size_t question);

	double estimateSE(Prior prior);

	double obsInf(double theta, int item);

	double fisherInf(double theta, int item);

	double partial_second_derivative(double theta, size_t question);

	virtual double expectedPV(int item, Prior &prior);

	double expectedObsInf(int item, Prior &prior);
};