#pragma once
#include "Estimator.h"
#include "Prior.h"

class EAPEstimator : public Estimator {
private:


public:

	EAPEstimator(Integrator integrator) : Estimator(integrator) { };

	virtual const EstimationType get_integration_type() const;

	virtual const double estimateTheta(QuestionSet questionSet, Prior prior);
};