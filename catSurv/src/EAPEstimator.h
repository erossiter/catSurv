#pragma once
#include "Estimator.h"
#include "Prior.h"


class EAPEstimator : public Estimator {
private:
	constexpr static int integrationSubintervals = 10;
	typedef std::function<double(double)> integrableFunction;

	/**
    * Computes the quotient of the integrals of the functions provided
    * - that is, it computes: ∫(numerator) / ∫(denominator).
    */
	double integralQuotient(const integrableFunction &numerator,
	                        const integrableFunction &denominator);

	double polytomous_posterior_variance(int item, Prior &prior);


	double binary_posterior_variance(int item, Prior &prior);

public:

	EAPEstimator(const Integrator &integrator, const QuestionSet &questionSet);

	virtual EstimationType getIntegrationType() const override;

	virtual double estimateTheta(Prior prior) override;

	double estimateSE(Prior prior) override;

	virtual double expectedPV(int item, Prior &prior) override;

};