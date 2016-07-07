#include "Rcpp.h"
#include <algorithm>
#include "Cat.h"
#include "EAPEstimator.h"
#include "MAPEstimator.h"
#include "MLEEstimator.h"
#include "EPVSelector.h"
#include "MEISelector.h"
#include "MFISelector.h"
#include "MPWISelector.h"
#include "MLWISelector.h"
#include "KLSelector.h"
#include "LKLSelector.h"
#include "PKLSelector.h"

using namespace Rcpp;

Cat::Cat(S4 cat_df) : questionSet(cat_df),
                      integrator(Integrator()),
                      prior(cat_df),
                      estimator(createEstimator(cat_df, integrator, questionSet)),
                      selector(createSelector(cat_df.slot("selection"), questionSet, *estimator, prior)) {}

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

List Cat::selectItem() {
	Selection selection = selector->selectItem();
  // Adding 1 to each row index so it prints the correct question number for user
	std::transform(selection.questions.begin(), selection.questions.end(), selection.questions.begin(),
                bind2nd(std::plus<int>(), 1.0));
	DataFrame all_estimates = Rcpp::DataFrame::create(Named("row.name") = selection.questions,
                                                   Named("questions") = selection.questions,
	                                                 Named(selection.name) = selection.values);
	return Rcpp::List::create(Named("all.estimates") = all_estimates, Named("next.item") = wrap(selection.item + 1));
}

List Cat::lookAhead(int item) {
  questionSet.nonapplicable_rows.erase(std::remove(questionSet.nonapplicable_rows.begin(),
                                                   questionSet.nonapplicable_rows.end(),
                                                   item), questionSet.nonapplicable_rows.end());
  questionSet.applicable_rows.push_back(item);

  std::vector<int> items;
  std::vector<int> response_options;
  for (size_t i = 1; i <= questionSet.difficulty[item].size()+1; ++i) {
      questionSet.poly[0] ? questionSet.answers[item] = i : questionSet.answers[item] = i - 1;
      Selection selection = selector->selectItem();
      items.push_back(selection.item + 1);
      response_options.push_back(questionSet.answers[item]);
	}
  
  questionSet.nonapplicable_rows.push_back(item); // add item back to unanswered q's
	questionSet.applicable_rows.pop_back(); // remove item from answered q's
	questionSet.answers[item] = NA_INTEGER; // remove answer
	  
	DataFrame all_estimates = Rcpp::DataFrame::create(Named("response.option") = response_options,
                                                   Named("next.item") = items);
	return Rcpp::List::create(Named("all.estimates") = all_estimates);
}


double Cat::findRoot() {
  estimator->findRoot();
}

void Cat::showCppCat() {
  std::cout << "discrimination: " << ' ';
  for (auto i: questionSet.discrimination){
    std::cout << i << ' ';
  }
  std::cout << "\nguessing: " << ' ';
  for (auto i: questionSet.guessing){
    std::cout << i << ' ';
  }
  std::cout << "\ndifficulty: " << ' ';
  for ( std::vector<std::vector<int>>::size_type i = 0; i < questionSet.difficulty.size(); i++ ){
    for ( std::vector<int>::size_type j = 0; j < questionSet.difficulty[i].size(); j++ ){
      std::cout << questionSet.difficulty[i][j] << ' ';
    }
    std::cout << std::endl;
  }
  std::cout << "nonapplicable_rows: " << ' ';
  for (auto i: questionSet.nonapplicable_rows){
    std::cout << i << ' ';
  }
  std::cout << "\napplicable_rows: " << ' ';
  for (auto i: questionSet.applicable_rows){
    std::cout << i << ' ';
  }
  std::cout << "\nanswers: " << ' ';
  for (auto i: questionSet.answers){
    std::cout << i << ' ';
  }
  std::cout << "\npriorName: " << prior.name << std::endl;
  std::cout << "parameters: " << ' ';
  for (auto i: prior.parameters){
    std::cout << i << ' ';
  }
  std::cout<<std::boolalpha;
  std::cout << "\nall_extreme: " << questionSet.all_extreme << std::endl;
  std::cout << "poly: " << questionSet.poly[0] << std::endl;
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

double Cat::fisherInf(double theta, int item) {
	return estimator->fisherInf(theta, item);
}

/**
 * A fairly naive implementation of a factory method for Estimators. Ideally, this will be refactored
 * into a separate factory with registration.
 */
std::unique_ptr<Estimator> Cat::createEstimator(S4 &cat_df, Integrator &integrator, QuestionSet &questionSet) {
	std::string estimation_type = cat_df.slot("estimation");
  std::string estimation_default = cat_df.slot("estimationDefault");
  
	// Note that this comparison is only legal because std::string, which overrides ==, is being used.
	// If, for some reason, C-style strings are ever used here, strncmp will have to be inserted.

	if (estimation_type == "EAP") {
		return std::unique_ptr<EAPEstimator>(new EAPEstimator(integrator, questionSet));
	}

	if (estimation_type == "MAP") {
		return std::unique_ptr<MAPEstimator>(new MAPEstimator(integrator, questionSet));
	}
	
	if (estimation_type == "MLE") {
	  if (questionSet.applicable_rows.size() == 0 || questionSet.all_extreme){
	    if (estimation_default == "MAP") return std::unique_ptr<MAPEstimator>(new MAPEstimator(integrator, questionSet));
	    if (estimation_default == "EAP") return std::unique_ptr<EAPEstimator>(new EAPEstimator(integrator, questionSet));
	  } else {
	    return std::unique_ptr<MLEEstimator>(new MLEEstimator(integrator, questionSet));
	  }
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
	
	if (selection_type == "MPWI") {
		return std::unique_ptr<MPWISelector>(new MPWISelector(questionSet, estimator, prior));
	}
	
	if (selection_type == "MLWI") {
		return std::unique_ptr<MLWISelector>(new MLWISelector(questionSet, estimator, prior));
	}
	
	if (selection_type == "KL") {
		return std::unique_ptr<KLSelector>(new KLSelector(questionSet, estimator, prior));
	}
	
	if (selection_type == "LKL") {
		return std::unique_ptr<LKLSelector>(new LKLSelector(questionSet, estimator, prior));
	}
	
	if (selection_type == "PKL") {
		return std::unique_ptr<PKLSelector>(new PKLSelector(questionSet, estimator, prior));
	}

	stop("%s is not a valid selection type.", selection_type);
	throw std::invalid_argument("Invalid selection type");
}

double Cat::expectedObsInf(int item) {
	return estimator->expectedObsInf(item, prior);
}

double Cat::expectedKL(int item) {
	return estimator->expectedKL(item, prior);
}

double Cat::likelihoodKL(int item) {
	return estimator->likelihoodKL(item, prior);
}

double Cat::posteriorKL(int item) {
	return estimator->posteriorKL(item, prior);
}

double Cat::fisherTestInfo() {
	return estimator->fisherTestInfo(prior);
}






