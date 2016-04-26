#pragma once
#include <Rcpp.h>
#include "Prior.h"
#include "QuestionSet.h"
#include "Estimator.h"
#include "Selector.h"
using namespace Rcpp;


class Cat {
public:

	Cat(S4 cat_df);

	double estimateTheta();

	double estimateSE();

	double likelihood(double theta);

	double expectedPV(int item);

	double dLL(double theta, bool use_prior);

	double d2LL(double theta, bool use_prior);

	double obsInf(double theta, int item);

	double expectedObsInf(int item);

	Rcpp::List nextItem();

	std::vector<double> probability(double theta, int question);

private:
	std::vector<double> theta_est;
	QuestionSet questionSet;
	Integrator integrator;
	std::unique_ptr<Estimator> estimator;
	Prior prior;
	std::unique_ptr<Selector> selector;


	static std::unique_ptr<Estimator> createEstimator(S4 &cat_df, Integrator &integrator, QuestionSet &questionSet);
	static std::unique_ptr<Selector> createSelector(std::string selection_type, QuestionSet &questionSet,
	                                                Estimator &estimator,
	                                                Prior &prior);


};


