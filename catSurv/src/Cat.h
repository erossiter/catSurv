#pragma once
#include <Rcpp.h>
#include "Prior.h"
#include "QuestionSet.h"
#include "EAPEstimator.h"
using namespace Rcpp;


class Cat {
	std::vector<double> theta_est;

	Integrator integrator = Integrator();
	EAPEstimator estimator = EAPEstimator(integrator, QuestionSet());
	Prior prior;

public:
	QuestionSet initialize_questionSet(S4 &cat_df);
	QuestionSet questionSet;

	Cat(QuestionSet &questions, Prior &prior);

	Cat(S4 cat_df);

	double estimateTheta();

	double estimateSE();

	double likelihood(double theta);

	double expectedPV(int item);

	Rcpp::List nextItem();

	std::vector<double> probability(double theta, int question);
};


