#pragma once
#include <Rcpp.h>
#include "Prior.h"
#include "QuestionSet.h"
#include "TrapezoidIntegrator.h"
#include "EAPEstimator.h"
using namespace Rcpp;


class Cat {
	std::vector<double> theta_est;

	TrapezoidIntegrator integrator = TrapezoidIntegrator();
	EAPEstimator estimator = EAPEstimator(integrator);
	Prior prior;
	double D;
public:
	QuestionSet questionSet;

	Cat(QuestionSet &questionSet, Prior &prior);

	Cat(S4 cat_df);

	double estimateTheta();

	double estimateSE();

	double likelihood(double theta, std::vector<int> items);

	double expectedPV(int item);

	Rcpp::List nextItem();

	std::vector<double> probability(double theta, int question);
};


