#pragma once
#include <string>
#include <vector>
#include <gsl/gsl_math.h>
#include "Integrator.h"
#include "QuestionSet.h"
#include "Prior.h"

enum class EstimationType {
	EAP, MAP, MLE
};


/**
 * Handles the actual task of computing estimations according to the various techniques.
 */
class Estimator {
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
	
	double findRoot();

protected:
	const Integrator &integrator;
	QuestionSet &questionSet;

	std::vector<double> paddedProbability(double theta, size_t question);

	double polytomous_likelihood(double theta);

	double binary_likelihood(double theta);

	/**
	 * GSL's integration library requires a function taking a double (and, optionally, a void pointer),
	 * and returning a double. To make things easier for usage here, a std::function with those parameters
	 * is typedef-ed as integrableFunction. If integralQuotient is ever moved into Integrator (which it probably should
	 * be), then this should be moved as well, and made public.
	 */
	typedef std::function<double(double)> integrableFunction;

	/**
	* Computes the quotient of the integrals of the functions provided
	* - that is, it computes: ∫(numerator) / ∫(denominator).
	*/
	double integralQuotient(const integrableFunction &numerator,
	                        const integrableFunction &denominator);
	
	double brentMethod(const integrableFunction &function);

private:
	/**
	 * This number is currently hard-coded, but it's entirely arbitrary - it was just decided upon
	 * as a temporary measure during a meeting. It is possible to use infinite subintervals, but
	 * requires a change in the GSL integration function used in Integrator.
	 */
	constexpr static int integrationSubintervals = 10;

	double polytomous_posterior_variance(int item, Prior &prior);

	double binary_posterior_variance(int item, Prior &prior);
	
};


