#pragma once
#include "Estimator.h"
#include "Prior.h"
#include "Integrator.h"


class WLEEstimator : public Estimator {


public:

	WLEEstimator(Integrator &integrator, QuestionSet &questionSet);

  ~WLEEstimator(){};

	virtual EstimationType getEstimationType() const override;

	virtual double estimateTheta(Prior prior) override;
	virtual double estimateTheta(Prior prior, size_t question, int answer) override;

	virtual double estimateSE(Prior prior) override;
  virtual double estimateSE(Prior prior, size_t question, int answer) override;

private:
  
  double ltm_estimateTheta(Prior prior);
  double ltm_estimateTheta(Prior prior, size_t question, int answer);
  
  double grm_estimateTheta(Prior prior);
  double grm_estimateTheta(Prior prior, size_t question, int answer);

  double gpcm_estimateTheta(Prior prior);
  double gpcm_estimateTheta(Prior prior, size_t question, int answer);

};
