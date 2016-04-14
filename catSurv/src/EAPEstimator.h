#pragma once
#include "Estimator.h"
#include "Prior.h"

class EAPEstimator : public Estimator {
public:

	EAPEstimator(const Integrator &integrator, const QuestionSet &questionSet) : Estimator(integrator, questionSet) { }

	virtual const EstimationType getIntegrationType() const override;

	virtual const double estimateTheta(Prior prior) override;

	void setQuestionSet(QuestionSet question) {
		questionSet = question;
	}
};