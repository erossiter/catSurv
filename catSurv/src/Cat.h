#pragma once
#include <Rcpp.h>
#include "Integration/Integrator.h"
#include "Estimation/Estimator.h"
#include "Prior/Prior.h"
#include "QuestionSet.h"
using namespace Rcpp;


class Cat {
	std::vector<double> theta_est;
	QuestionSet questionSet;
	Integrator integrator;
	Estimator estimator;
	Prior prior;
	double D;
protected:
	double likelihood(double theta, std::vector<int> items);

public:
	Cat(QuestionSet questionSet, Integrator integrator, Estimator estimator, Prior prior);

	Cat(S4 cat_df);

	std::vector<double> probability(double theta, int question);
};


