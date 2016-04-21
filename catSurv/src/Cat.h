#pragma once
#include <Rcpp.h>
#include "Prior.h"
#include "QuestionSet.h"
#include "EAPEstimator.h"
using namespace Rcpp;


class Cat {
public:
	QuestionSet initialize_questionSet(S4 &cat_df);

	QuestionSet questionSet;

	Cat(QuestionSet &questions, Prior &priorData);

	Cat(S4 cat_df);

	double estimateTheta();

	double estimateSE();

	double likelihood(double theta);

	double expectedPV(int item);

	double dLL(double theta, bool use_prior);

	double d2LL(double theta, bool use_prior);

	Rcpp::List nextItem();

	std::vector<double> probability(double theta, int question);

private:
	std::vector<double> theta_est;

	std::unique_ptr<Estimator> createEstimator(S4 cat_df);
	Integrator integrator;
	std::unique_ptr<Estimator> estimator;
	Prior prior;


};


