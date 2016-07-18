#pragma once
#include "Estimator.h"
#include "Prior.h"


class MLEEstimator : public Estimator {

public:

	MLEEstimator(Integrator &integrator, QuestionSet &questionSet);

	virtual EstimationType getEstimationType() const override;

	virtual double estimateTheta(Prior prior) override;
	
	virtual double estimateSE(Prior prior) override;

protected:
  
  double dLL_root();
	
};