#pragma once
#include "Estimator.h"
#include "Prior.h"
#include "Integrator.h"


class WLEEstimator : public Estimator {


public:

	WLEEstimator(Integrator &integrator, QuestionSet &questionSet);

	virtual EstimationType getEstimationType() const override;

	virtual double estimateTheta(Prior prior) override;
	
	virtual double estimateSE(Prior prior) override;

private:
  
  double ltm_estimateTheta(Prior prior);
  
  double grm_estimateTheta(Prior prior);

  double gpcm_estimateTheta(Prior prior);

};
