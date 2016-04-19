#pragma once
#include "Estimator.h"
#include "Prior.h"


class EAPEstimator : public Estimator {


public:

	EAPEstimator(const Integrator &integrator, const QuestionSet &questionSet);

	virtual EstimationType getEstimationType() const override;

	virtual double estimateTheta(Prior prior) override;

};