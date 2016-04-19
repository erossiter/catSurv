#pragma once
#include <string>
#include <vector>
#include "Integrator.h"
#include "QuestionSet.h"
#include "Prior.h"

enum class EstimationType {
	NONE, EAP, MAP
};

class NotImplementedException : public std::exception {

public:

	NotImplementedException(const std::string &error = "Functionality not yet implemented!") {
		errorMessage = error;
	}

	const char *what() const noexcept {
		return errorMessage.c_str();
	}

private:
	std::string errorMessage;
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

	virtual EstimationType getEstimationType() const {
		return EstimationType::NONE;
	}

	virtual double estimateTheta(Prior prior) {
		(void) prior;
		const std::string message = "This function is not implemented in the base class.";
		throw NotImplementedException(message);
	}

	double likelihood(double theta);

	std::vector<double> probability(double theta, int question);


	double estimateSE(Prior prior);

	virtual double expectedPV(int item, Prior &prior);
};