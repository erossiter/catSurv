#include "Rcpp.h"
#include "Cat.h"
#include <stdexcept>
#include "MAPEstimator.h"

using namespace Rcpp;

Cat::Cat(QuestionSet &questions, Prior &priorData)
		: questionSet(questions), integrator(Integrator()), estimator(integrator, questionSet), prior(priorData) {
}

Cat::Cat(S4 cat_df) : questionSet(initialize_questionSet(cat_df)),
                      integrator(Integrator()),
                      estimator(createEstimator(cat_df)),
                      prior(cat_df) {
	theta_est = Rcpp::as<std::vector<double> >(cat_df.slot("Theta.est"));
}


double Cat::likelihood(double theta) {
	return estimator.likelihood(theta);
}

std::vector<double> Cat::probability(double theta, int question) {
	return estimator.probability(theta, question);
}


double Cat::estimateTheta() {
	return estimator.estimateTheta(prior);
}

double Cat::estimateSE() {
	return estimator.estimateSE(prior);
}

double Cat::expectedPV(int item) {
	return estimator.expectedPV(item, prior);
}

List Cat::nextItem() {
	// For every unanswered item, calculate the epv of that item
	std::vector<double> epvs;
	int min_item = -1;
	double min_epv = DBL_MAX;

	for (int row : questionSet.nonapplicable_rows) {
		double epv = expectedPV(row - 1);
		epvs.push_back(epv);

		if (epv < min_epv) {
			min_item = row;
			min_epv = epv;
		}
	}
	DataFrame all_estimates = Rcpp::DataFrame_Impl<Rcpp::PreserveStorage>::create(Named("questions") = questionSet.nonapplicable_rows, Named("EPV") = epvs);
	return Rcpp::List::create(Named("all.estimates") = all_estimates, Named("next.item") = wrap(min_item));
}

QuestionSet Cat::initialize_questionSet(S4 &cat_df) {
	QuestionSet questions;

	questions.answers = Rcpp::as<std::vector<int> >(cat_df.slot("answers"));
	questions.guessing = Rcpp::as<std::vector<double> >(cat_df.slot("guessing"));
	questions.discrimination = Rcpp::as<std::vector<double> >(cat_df.slot("discrimination"));


	for (size_t i = 0; i < questions.answers.size(); i++) {
		if (questions.answers[i] == NA_INTEGER) {
			questions.nonapplicable_rows.push_back(i); // + 1
		} else {
			questions.applicable_rows.push_back(i);
		}
	}

	for (auto item : (List) cat_df.slot("difficulty")) {
		questions.difficulty.push_back(Rcpp::as<std::vector<double> >(item));
	}
	questions.poly = cat_df.slot("poly");
	return questions;


}

double Cat::dLL(double theta, bool use_prior) {
	if (typeid(estimator) == typeid(MAPEstimator)) {
		stop("Error: dLL is only available when using MAP estimation.");
	}
	MAPEstimator mapEstimator = static_cast<MAPEstimator&>(estimator);
	return mapEstimator.dLL(theta, use_prior, prior);
}

double Cat::d2LL(double theta, bool use_prior) {
	if (typeid(estimator) == typeid(MAPEstimator)) {
		stop("Error: d2LL is only available when using MAP estimation.");
	}
	MAPEstimator mapEstimator = static_cast<MAPEstimator&>(estimator);
	return mapEstimator.d2LL(theta, use_prior, prior);
}

Estimator Cat::createEstimator(S4 cat_df) {
	std::string estimation_type = cat_df.slot("estimation");
	if (estimation_type == "EAP") {
		return EAPEstimator(integrator, questionSet);
	}

	if (estimation_type == "MAP") {
		return MAPEstimator(integrator, questionSet);
	}

	stop("%s is not a valid estimation type.", estimation_type);
	throw std::invalid_argument("Invalid estimation type");
}





