#pragma once
#include "QuestionSet.h"
#include "Estimator.h"
#include "Prior.h"



class MLEEstimator : public Estimator {

public:

	MLEEstimator(Integrator &integrator, QuestionSet &questionSet);

	virtual EstimationType getEstimationType() const override;

	virtual double estimateTheta(Prior prior) override;
	virtual double estimateTheta(Prior prior, size_t question, int answer) override;
	
	virtual double estimateSE(Prior prior) override;
	virtual double estimateSE(Prior prior, size_t question, int answer) override;

//protected:
  
  double d1LL_root();
  double d1LL_root(size_t question, int answer);
	
};