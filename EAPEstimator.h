#pragma once
#include "Estimator.h"
#include "Prior.h"


class EAPEstimator : public Estimator {


public:

	EAPEstimator(Integrator &integrator, QuestionSet &questionSet);

	virtual EstimationType getEstimationType() const override;

	virtual double estimateTheta(Prior prior) override;

};