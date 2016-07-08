#pragma once
#include "Estimator.h"
#include "Prior.h"


class MAPEstimator : public Estimator {

public:

	MAPEstimator(Integrator &integrator, QuestionSet &questionSet);

	double dLL(double theta, bool use_prior, Prior &prior);

	double d2LL(double theta, bool use_prior, Prior &prior);

	virtual EstimationType getEstimationType() const override;

	virtual double estimateTheta(Prior prior) override;
	
	virtual double estimateSE(Prior prior) override;

	double polytomous_dLL(double theta);

	double binary_dLL(double theta);

private:
	constexpr static int integrationSubintervals = 10; //might delete this??

	double polytomous_d2LL(double theta);
	double binary_d2LL(double theta);
};