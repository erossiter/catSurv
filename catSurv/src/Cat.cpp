#include "Rcpp.h"
#include "Cat.h"
#include "EAPEstimator.h"
#include "MAPEstimator.h"
#include "EPVSelector.h"
#include "MFISelector.h"

using namespace Rcpp;

Cat::Cat(S4 cat_df) : questionSet(cat_df),
                      integrator(Integrator()),
                      estimator(createEstimator(cat_df, integrator, questionSet)),
                      prior(cat_df),
                      selector(createSelector(cat_df.slot("selection"), questionSet, *estimator, prior)) {
	theta_est = Rcpp::as<std::vector<double> >(cat_df.slot("Theta.est"));
}


double Cat::likelihood(double theta) {
	return estimator->likelihood(theta);
}

std::vector<double> Cat::probability(double theta, int question) {
	return estimator->probability(theta, size_t(question) - 1);
}

double Cat::estimateTheta() {
	return estimator->estimateTheta(prior);
}

double Cat::estimateSE() {
	return estimator->estimateSE(prior);
}

double Cat::expectedPV(int item) {
	return estimator->expectedPV(item, prior);
}

List Cat::nextItem() {
	Selection selection = selector->nextItem();
	DataFrame all_estimates = Rcpp::DataFrame::create(Named("questions") = selection.questions,
	                                                  Named(selection.name) = selection.values);
	return Rcpp::List::create(Named("all.estimates") = all_estimates, Named("next.item") = wrap(selection.item));
}

double Cat::dLL(double theta, bool use_prior) {
	if (typeid(estimator) == typeid(MAPEstimator)) {
		stop("Error: dLL is only available when using MAP estimation.");
	}
	MAPEstimator &mapEstimator = static_cast<MAPEstimator &>(*estimator);
	return mapEstimator.dLL(theta, use_prior, prior);
}

double Cat::d2LL(double theta, bool use_prior) {
	if (typeid(estimator) == typeid(MAPEstimator)) {
		stop("Error: d2LL is only available when using MAP estimation.");
	}
	MAPEstimator &mapEstimator = static_cast<MAPEstimator &>(*estimator);
	return mapEstimator.d2LL(theta, use_prior, prior);
}

double Cat::obsInf(double theta, int item) {
	return estimator->obsInf(theta, item);
}

/**
 * A fairly naive implementation of a factory method for Estimators. Ideally, this will be refactored
 * into a separate factory with registration.
 */
std::unique_ptr<Estimator> Cat::createEstimator(S4 &cat_df, Integrator &integrator, QuestionSet &questionSet) {
	std::string estimation_type = cat_df.slot("estimation");
	if (estimation_type == "EAP") {
		return std::unique_ptr<EAPEstimator>(new EAPEstimator(integrator, questionSet));
	}

	if (estimation_type == "MAP") {
		return std::unique_ptr<MAPEstimator>(new MAPEstimator(integrator, questionSet));
	}

	stop("%s is not a valid estimation type.", estimation_type);
	throw std::invalid_argument("Invalid estimation type");
}

/**
 * A fairly naive implementation of a factory method for Estimators. Ideally, this will be refactored
 * into a separate factory with registration.
 */
std::unique_ptr<Selector> Cat::createSelector(std::string selection_type, QuestionSet &questionSet,
                                              Estimator &estimator, Prior &prior) {

	if (selection_type == "EPV") {
		return std::unique_ptr<EPVSelector>(new EPVSelector(questionSet, estimator, prior));
	}

	if (selection_type == "MFI") {
		return std::unique_ptr<MFISelector>(new MFISelector(questionSet, estimator, prior));
	}

	if (selection_type == "MEI") {
		return std::unique_ptr<MEISelector>(new MEISelector(questionSet, estimator, prior));
	}

	stop("%s is not a valid selection type.", selection_type);
	throw std::invalid_argument("Invalid selection type");
}

double Cat::expectedObsInf(int item) {
	return estimator.expectedObsInf(item);
}




