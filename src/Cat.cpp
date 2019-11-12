#include "Rcpp.h"
#include <algorithm>
#include <math.h>
#include "Cat.h"
#include "EAPEstimator.h"
#include "MAPEstimator.h"
#include "MLEEstimator.h"
#include "WLEEstimator.h"
#include "EPVSelector.h"
#include "MEISelector.h"
#include "MFISelector.h"
#include "MFIISelector.h"
#include "MPWISelector.h"
#include "MLWISelector.h"
#include "KLSelector.h"
#include "LKLSelector.h"
#include "PKLSelector.h"
#include "RANDOMSelector.h"


using namespace Rcpp;

Cat::Cat(S4 cat_df) : questionSet(cat_df),
                      integrator(Integrator()),
                      prior(cat_df),
                      checkRules(cat_df),
                      estimator(createEstimator(cat_df, integrator, questionSet)),
                      selector(createSelector(cat_df.slot("selection"), questionSet, *estimator, prior)){}

bool Cat::checkStopRules() { 
  if(noneOfOverrides())
  {
    return anyOfThresholds();
  }
  return false;  
}

bool Cat::anyOfThresholds()
{  
  if (! std::isnan(checkRules.lengthThreshold)){
      //added non-response to length count
      if((questionSet.applicable_rows.size() + questionSet.skipped.size()) >= checkRules.lengthThreshold){
          return true;
      }
  }

  
  
  if (! std::isnan(checkRules.seThreshold)) {
      double se = estimator->estimateSE(prior);
    if(se < checkRules.seThreshold ) {
      return true;
    }
  }

  if (! std::isnan(checkRules.gainThreshold)){
    bool answer_gainThreshold  = std::all_of(questionSet.nonapplicable_rows.begin(), questionSet.nonapplicable_rows.end(), [&](int item)
    {
        double se = estimator->estimateSE(prior);
        double gain = std::abs(se - std::pow(expectedPV(item), 0.5));
        return gain < checkRules.gainThreshold;
    });

    if(answer_gainThreshold){
      return true;
    }
  }


  if (! std::isnan(checkRules.infoThreshold)){
    double theta = estimator->estimateTheta(prior);
    bool answer_infoThreshold  = std::all_of(questionSet.nonapplicable_rows.begin(), questionSet.nonapplicable_rows.end(), [&](int item)
    {
        double info = estimator->fisherInf(theta, item);
        return info < checkRules.infoThreshold;
    });

    if(answer_infoThreshold) {
      return true;
    }
  }
   
  return false;
}

bool Cat::noneOfOverrides(){
  if (! std::isnan(checkRules.lengthOverride)) {
    if((questionSet.applicable_rows.size() + questionSet.skipped.size()) < checkRules.lengthOverride){
      return false;
    }
  }

  
  if (! std::isnan(checkRules.gainOverride)){
    bool answer_gainOverride  = std::all_of(questionSet.nonapplicable_rows.begin(), questionSet.nonapplicable_rows.end(), [&](int item)
    {
        double se = estimator->estimateSE(prior);
        double gain = std::abs(se - std::pow(expectedPV(item), 0.5));
        return gain >= checkRules.gainOverride;
    });

    if(answer_gainOverride){
      return false;
    }
  }
  return true;
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

List Cat::selectItem() {
  if(questionSet.nonapplicable_rows.empty()){
    throw std::domain_error("selectItem should not be called if all items have been answered.");
  }
  
  Selection selection = selector->selectItem();
    // Adding 1 to each row index so it prints the correct question number for user
    //std::transform(selection.questions.begin(), selection.questions.end(), selection.questions.begin(),
    //             bind2nd(std::plus<int>(), 1.0));
	std::for_each(selection.questions.begin(), selection.questions.end(), [](int& d) { d+=1.0;});
	
	DataFrame all_estimates = Rcpp::DataFrame::create(Named("q_number") = selection.questions,
                                                   Named("q_name") = selection.question_names,
	                                                 Named(selection.name) = selection.values);
                                                     
	return Rcpp::List::create(Named("estimates") = all_estimates,
                              Named("next_item") = wrap(selection.item + 1),
	                          Named("next_item_name") = questionSet.question_names.at(selection.item));
}

DataFrame Cat::lookAhead(int item) {

    //if item has been previously skipped
    if(std::find(questionSet.skipped.begin(), questionSet.skipped.end(),
                 item) != questionSet.skipped.end()){
        Rcpp::Rcout << "lookAhead should not be called for a skipped item." << std::endl;
        
        // return empty dataframe
        std::vector<std::string> items(questionSet.difficulty.at(item).size()+2, "NULL");
        std::vector<std::string> response_options(questionSet.difficulty.at(item).size()+2, "NULL");
        
        DataFrame all_estimates = Rcpp::DataFrame::create(Named("response_option") = response_options,
                                                          Named("next_item") = items);
        return all_estimates;
    }  


  //item is item index and 0-indexed
  if(std::find(questionSet.applicable_rows.begin(), questionSet.applicable_rows.end(),
               item) != questionSet.applicable_rows.end()){
      Rcpp::Rcout << "lookAhead should not be called for an answered item." << std::endl;
      
      // return empty dataframe
      std::vector<std::string> items(questionSet.difficulty.at(item).size()+2, "NULL");
      std::vector<std::string> response_options(questionSet.difficulty.at(item).size()+2, "NULL");
      
      DataFrame all_estimates = Rcpp::DataFrame::create(Named("response_option") = response_options,
                                                        Named("next_item") = items);
      return all_estimates;
  }

  if(questionSet.nonapplicable_rows.size() == 1){
      Rcpp::Rcout << "lookAhead should not be called for last unanswered item." << std::endl;
      
      // return empty dataframe
      std::vector<std::string> items(questionSet.difficulty.at(item).size()+2, "NULL");
      std::vector<std::string> response_options(questionSet.difficulty.at(item).size()+2, "NULL");
      
      DataFrame all_estimates = Rcpp::DataFrame::create(Named("response_option") = response_options,
                                                        Named("next_item") = items);
      return all_estimates;
  }
  
  // storage vectors
  std::vector<int> items;
  std::vector<int> response_options;
  
  
  // say item has been skipped
  questionSet.skipped.push_back(item);
  
  // take item out of unanswered questions
  questionSet.nonapplicable_rows.erase(std::remove(questionSet.nonapplicable_rows.begin(),
                                                   questionSet.nonapplicable_rows.end(),
                                                   item), questionSet.nonapplicable_rows.end());

  questionSet.answers.at(item) = -1;
  Selection selection = selector->selectItem();

  items.push_back(selection.item + 1);
  response_options.push_back(questionSet.answers.at(item));

  // undo that item has been skipped
  questionSet.skipped.pop_back(); // remove item from answered q's


  // // take item out of unanswered questions
  // questionSet.nonapplicable_rows.erase(std::remove(questionSet.nonapplicable_rows.begin(),
  //                                                  questionSet.nonapplicable_rows.end(),
  //                                                  item), questionSet.nonapplicable_rows.end());
  // say item has been answered
  questionSet.applicable_rows.push_back(item);


  // now possible answers
  for (size_t i = 1; i <= questionSet.difficulty.at(item).size()+1; ++i) {
      // if binary response options, iterate from 0, otherwise iterate from 1
      questionSet.answers.at(item) = ((questionSet.model == "ltm") | (questionSet.model == "tpm")) ?  i - 1 : i;
      Selection selection = selector->selectItem();
      items.push_back(selection.item + 1);
      response_options.push_back(questionSet.answers.at(item));
  }


  questionSet.nonapplicable_rows.push_back(item); // add item back to unanswered q's
  questionSet.applicable_rows.pop_back(); // remove item from answered q's
  questionSet.answers.at(item) = NA_INTEGER; // remove answer
    
  DataFrame all_estimates = Rcpp::DataFrame::create(Named("response_option") = response_options,
                                                   Named("next_item") = items);
  return all_estimates;
}







double Cat::d1LL(double theta, bool use_prior) {
	return estimator->d1LL(theta, use_prior, prior);
}

double Cat::d2LL(double theta, bool use_prior) {
	return estimator->d2LL(theta, use_prior, prior);
}

double Cat::obsInf(double theta, int item) {
	if (questionSet.applicable_rows.empty()) {
		throw std::domain_error("ObsInf should not be called if no items have been answered.");
	}
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
	
	
	if (estimation_type == "MLE" || estimation_type == "WLE") {

	    if (questionSet.applicable_rows.size() == 0 || questionSet.all_extreme){
	        Rcpp::Rcout<<"Warning: estimate_default will be used to estimate theta as the maximum likelihood cannot be computed with an answer profile of all extreme response options."<<std::endl;
	        if (estimation_default == "MAP") return std::unique_ptr<MAPEstimator>(new MAPEstimator(integrator, questionSet));
	        if (estimation_default == "EAP") return std::unique_ptr<EAPEstimator>(new EAPEstimator(integrator, questionSet));
	    } 
	    else if (estimation_type == "MLE") {
	        return std::unique_ptr<MLEEstimator>(new MLEEstimator(integrator, questionSet));
	    }
	    else if (estimation_type == "WLE") {
	        return std::unique_ptr<WLEEstimator>(new WLEEstimator(integrator, questionSet));
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
	
	
	if (selection_type == "LKL") {
		return std::unique_ptr<LKLSelector>(new LKLSelector(questionSet, estimator, prior));
	}
	
	if (selection_type == "PKL") {
		return std::unique_ptr<PKLSelector>(new PKLSelector(questionSet, estimator, prior));
	}
	
	
	if (selection_type == "RANDOM") {
		return std::unique_ptr<RANDOMSelector>(new RANDOMSelector(questionSet, estimator, prior));
	}
	
	// uses EPV for selection methods that fail when no questions asked
	if (selection_type == "MFII" || selection_type == "KL") {
	    if (questionSet.applicable_rows.size() == 0){
	        Rcpp::Rcout<<"Warning: EPV will be used select first question since MFII and KL routines fail when no answers have been recorded."<<std::endl;
	        return std::unique_ptr<EPVSelector>(new EPVSelector(questionSet, estimator, prior));
	    }else{
	        if(selection_type == "MFII"){
	            return std::unique_ptr<MFIISelector>(new MFIISelector(questionSet, estimator, prior));
	        }
	        if (selection_type == "KL") {
	            return std::unique_ptr<KLSelector>(new KLSelector(questionSet, estimator, prior));
	        }
	    }
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

double Cat::fisherTestInfo(double theta) {
    return estimator->fisherTestInfo(theta);
}







