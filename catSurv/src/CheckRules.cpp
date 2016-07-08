#include "CheckRules.h"
#include <math.h>



bool CheckRules::check_lengthThreshold(){
  return questionSet.applicable_rows.size() >= lengthThreshold ? true : false;
}

bool CheckRules::check_seThreshold(Prior &prior){
  return estimator.estimateSE(prior) >= seThreshold ? true : false;
}

// bool CheckRules::check_infoThreshold(Prior &prior){
//   return estimator.fisherInf(prior) >= infoThreshold ? true : false;
// }
// 
// bool CheckRules::check_gainThreshold(Prior &prior){
//   return estimator.estimatSE(prior) - estimator.fisherInf(estimator.estimateTheta(prior),???? )  >= gainThreshold ? true : false;
// }

bool CheckRules::checkRules(Prior &prior) {
  return true;
}

CheckRules::CheckRules(Rcpp::S4 &cat_df, QuestionSet &question, Estimator &estimation, Prior &priorModel)  : 
  lengthThreshold(Rcpp::as<double> (cat_df.slot("lengthThreshold"))),
  seThreshold(Rcpp::as<double> (cat_df.slot("seThreshold"))), 
  infoThreshold(Rcpp::as<double> (cat_df.slot("infoThreshold"))),
  gainThreshold(Rcpp::as<double> (cat_df.slot("gainThreshold"))),
  lengthOverride(Rcpp::as<double> (cat_df.slot("lengthOverride"))),
  gainOverride(Rcpp::as<double> (cat_df.slot("gainOverride"))),
  questionSet(question),
  estimator(estimation),
  prior(priorModel) { }



  





