#pragma once
#include "Estimator.h"
#include "Prior.h"


class MAPEstimator : public Estimator {

public:

	MAPEstimator(Integrator &integrator, QuestionSet &questionSet);

	~MAPEstimator();

	virtual EstimationType getEstimationType() const override;

	virtual double estimateTheta(Prior prior) override;
	virtual double estimateTheta(Prior prior, size_t question, int answer) override;
	
	virtual double estimateSE(Prior prior) override;
	virtual double estimateSE(Prior prior, size_t question, int answer) override;
	
	double newton_raphson(Prior prior, double theta_hat_old, double theta_hat_new, bool second_try);
	double newton_raphson(Prior prior, size_t question, int answer, double theta_hat_old, double theta_hat_new, bool second_try);

};