#pragma once
#include <string>
#include <vector>
#include <gsl/gsl_math.h>
#include "Integrator.h"
#include "QuestionSet.h"
#include "Prior.h"

enum class EstimationType {
	EAP, MAP, MLE, WLE
};


/**
 * Handles the actual task of computing estimations according to the various techniques.
 */
class Estimator {
public:
	Estimator(Integrator &integration, QuestionSet &question);

	virtual EstimationType getEstimationType() const = 0;

	virtual double estimateTheta(Prior prior) = 0;

	virtual double estimateSE(Prior prior) = 0;

	double likelihood(double theta);

	std::vector<double> probability(double theta, size_t question);

	double obsInf(double theta, int item);

	double fisherInf(double theta, int item);

	double partial_second_derivative(double theta, size_t question);

	virtual double expectedPV(int item, Prior &prior);

	double expectedObsInf(int item, Prior &prior);
	
	//double fisherTestInfo(Prior prior);
	double fisherTestInfo(double theta);
	double pwi(int item, Prior prior);
	
	double lwi(int item);
	
	double fii(int item, Prior prior);
	
	double expectedKL(int item, Prior prior);
	
	double likelihoodKL(int item, Prior prior);
	
	double posteriorKL(int item, Prior prior);
	
	double dLL(double theta, bool use_prior, Prior &prior);
	double d2LL(double theta, bool use_prior, Prior &prior);
	

protected:
	const Integrator &integrator;
	QuestionSet &questionSet;

	std::vector<double> paddedProbability(double theta, size_t question);

	double polytomous_likelihood(double theta);

	double binary_likelihood(double theta);
	
	double kl(double theta_not, int item, Prior prior);

	/**
	 * GSL's integration library requires a function taking a double (and, optionally, a void pointer),
	 * and returning a double. To make things easier for usage here, a std::function with those parameters
	 * is typedef-ed as integrableFunction. If integralQuotient is ever moved into Integrator (which it probably should
	 * be), then this should be moved as well, and made public.
	 */
	typedef std::function<double(double)> integrableFunction;
	
	double brentMethod(const integrableFunction &function);
	
	double integrate_selectItem(const integrableFunction &function, const double lower, const double upper);

private:
	/**
	 * This number is currently hard-coded, but it's entirely arbitrary - it was just decided upon
	 * as a temporary measure during a meeting. It is possible to use infinite subintervals, but
	 * requires a change in the GSL integration function used in Integrator.
	 */
	constexpr static double integrationSubintervals = 10;

	double polytomous_posterior_variance(int item, Prior &prior);
	double binary_posterior_variance(int item, Prior &prior);
	
	double polytomous_dLL(double theta);
	double binary_dLL(double theta);
	
	double polytomous_d2LL(double theta);
	double binary_d2LL(double theta);
	
};


