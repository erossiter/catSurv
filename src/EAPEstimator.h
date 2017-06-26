#pragma once
#include "Estimator.h"
#include "Prior.h"
#include "Integrator.h"


class EAPEstimator : public Estimator {


public:

	EAPEstimator(Integrator &integrator, QuestionSet &questionSet);

	virtual EstimationType getEstimationType() const override;

	virtual double estimateTheta(Prior prior) override;
	virtual double estimateTheta(Prior prior, size_t question, int answer) override;
	
	virtual double estimateSE(Prior prior) override;
	virtual double estimateSE(Prior prior, size_t question, int answer) override;
	
protected:
	typedef std::function<double(double)> integrableFunction;

	/**
	* Computes the quotient of the integrals of the functions provided
	* - that is, it computes: ∫(numerator) / ∫(denominator).
	*/
	double integralQuotient(const integrableFunction &numerator,
	                        const integrableFunction &denominator,
                          const double lower, const double upper);
	
private:
	/**
	 * This number is currently hard-coded, but it's entirely arbitrary - it was just decided upon
	 * as a temporary measure during a meeting. It is possible to use infinite subintervals, but
	 * requires a change in the GSL integration function used in Integrator.
	 */
	constexpr static double integrationSubintervals = 10;

};
