#pragma once
#include "Estimator.h"
#include "Prior.h"


class MAPEstimator : public Estimator {

public:

	MAPEstimator(Integrator &integrator, QuestionSet &questionSet);

	virtual EstimationType getEstimationType() const override;

	virtual double estimateTheta(Prior prior) override;
	virtual double estimateTheta(Prior prior, size_t question, int answer) override;
	
	virtual double estimateSE(Prior prior) override;
	virtual double estimateSE(Prior prior, size_t question, int answer) override;

};