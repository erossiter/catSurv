#pragma once
#include "Estimator.h"
#include "Prior.h"
#include "MAPEstimator.h"


class MLEEstimator : public Estimator {


public:
  
	double dLL(double theta);

	double d2LL(double theta);

	MLEEstimator(Integrator &integrator, QuestionSet &questionSet);

	virtual EstimationType getEstimationType() const override;

	virtual double estimateTheta(Prior prior) override;
	
	double polytomous_dLL(double theta);

	double binary_dLL(double theta);
	
	double polytomous_d2LL(double theta);
	
	double binary_d2LL(double theta);

};