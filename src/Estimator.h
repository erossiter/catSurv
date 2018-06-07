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
	virtual double estimateTheta(Prior prior, size_t question, int answer) = 0;

	virtual double estimateSE(Prior prior) = 0;
	virtual double estimateSE(Prior prior, size_t question, int answer) = 0;

	double likelihood(double theta);
	double likelihood(double theta, size_t question, int answer);

	std::vector<double> probability(double theta, size_t question);

	double obsInf(double theta, int item);
	double obsInf(double theta, int item, int answer);
	double obsInf_grm(double theta, int item);
	double obsInf_grm(double theta, int item, int answer);
	double obsInf_gpcm(double theta, int item);
	double obsInf_gpcm(double theta, int item, int answer);
	double obsInf_ltm(double theta, int item);
	double obsInf_ltm(double theta, int item, int answer);

	double fisherInf(double theta, int item);
	double fisherInf(double theta, int item, int answer);

	virtual double expectedPV(int item, Prior &prior);
	virtual double expectedPV_ltm_tpm(int item, Prior &prior);
	virtual double expectedPV_grm(int item, Prior &prior);
	virtual double expectedPV_gpcm(int item, Prior &prior);

	double expectedObsInf(int item, Prior &prior);
	double expectedObsInf_grm(int item, Prior &prior);
	double expectedObsInf_gpcm(int item, Prior &prior);
	double expectedObsInf_rest(int item, Prior &prior);

	double fisherTestInfo(double theta);	
	double fisherTestInfo(Prior prior);
	double fisherTestInfo(Prior prior, size_t question, int answer);
	
	double pwi(int item, Prior prior);
	
	double lwi(int item);
	
	double fii(int item, Prior prior);
	
	double expectedKL(int item, Prior prior);
	
	double likelihoodKL(int item, Prior prior);
	
	double posteriorKL(int item, Prior prior);
	
	double d1LL(double theta, bool use_prior, Prior &prior);
	double d1LL(double theta, bool use_prior, Prior &prior, size_t question, int answer);

	double d2LL(double theta, bool use_prior, Prior &prior);
	double d2LL(double theta, bool use_prior, Prior &prior, size_t question, int answer);

protected:

	//for WLEEstimator
	void prob_derivs_gpcm(double theta, size_t question, std::vector<double>& probs, std::vector<double>& first, std::vector<double>& second);
	std::vector<double> prob_derivs_gpcm_first(double theta, size_t question);

	double prob_ltm(double theta, size_t question);
  	std::vector<double> prob_grm(double theta, size_t question);
  	std::pair<double,double> prob_grm_pair(double theta, size_t question, size_t at);
  	std::vector<double> prob_gpcm(double theta, size_t question);
  	double prob_gpcm_at(double theta, size_t question, size_t at);


protected:
	const Integrator &integrator;
	QuestionSet &questionSet;
	
	double kl(double theta_not, int item, double theta);

	/**
	 * GSL's integration library requires a function taking a double (and, optionally, a void pointer),
	 * and returning a double. To make things easier for usage here, a std::function with those parameters
	 * is typedef-ed as integrableFunction. If integralQuotient is ever moved into Integrator (which it probably should
	 * be), then this should be moved as well, and made public.
	 */
	typedef std::function<double(double)> integrableFunction;
	
	//double brentMethod(const integrableFunction &function);
	double brentMethod(integrableFunction function);
	
	double integrate_selectItem(const integrableFunction &function, const double lower, const double upper);

private:
	/**
	 * This number is currently hard-coded, but it's entirely arbitrary - it was just decided upon
	 * as a temporary measure during a meeting. It is possible to use infinite subintervals, but
	 * requires a change in the GSL integration function used in Integrator.
	 */
	constexpr static double integrationSubintervals = 10;
  
    
  
  double likelihood_ltm(double theta);
	double likelihood_grm(double theta);
	double likelihood_gpcm(double theta);

	double likelihood_ltm(double theta, size_t question, int answer);
	double likelihood_grm(double theta, size_t question, int answer);
	double likelihood_gpcm(double theta, size_t question, int answer);
  
  double grm_d1LL(double theta);
	double gpcm_d1LL(double theta);
	double ltm_d1LL(double theta);

	double grm_d1LL(double theta, size_t question, int answer);
	double gpcm_d1LL(double theta, size_t question, int answer);
	double ltm_d1LL(double theta, size_t question, int answer);
	
	double grm_partial_d2LL(double theta, size_t question);	
	double gpcm_partial_d2LL(double theta, size_t question);	
	double gpcm_partial_d1LL(double theta, size_t question);	

	double grm_partial_d2LL(double theta, size_t question, int answer);	
	double gpcm_partial_d2LL(double theta, size_t question, int answer);	
	double gpcm_partial_d1LL(double theta, size_t question, int answer);	

  double grm_d2LL(double theta);
	double gpcm_d2LL(double theta);
	double ltm_d2LL(double theta);

	double grm_d2LL(double theta, size_t question, int answer);
	double gpcm_d2LL(double theta, size_t question, int answer);
	double ltm_d2LL(double theta, size_t question, int answer);

	double polytomous_posterior_variance(int item, Prior &prior);
	double binary_posterior_variance(int item, Prior &prior);
	

};


