#include "CheckRules.h"
#include <math.h>


CheckRules::CheckRules(Rcpp::S4 &cat_df) {
  lengthThreshold = Rcpp::as<double> (cat_df.slot("lengthThreshold"));
  seThreshold = Rcpp::as<double> (cat_df.slot("seThreshold"));
  infoThreshold = Rcpp::as<double> (cat_df.slot("infoThreshold"));
  gainThreshold = Rcpp::as<double> (cat_df.slot("gainThreshold"));
  lengthOverride = Rcpp::as<double> (cat_df.slot("lengthOverride"));
  gainOverride = Rcpp::as<double> (cat_df.slot("gainOverride"));
}

// bool CheckRules::check_seThreshold(Prior &prior){
//   return estimator.estimateSE(prior) < seThreshold ? true : false;
// }

// bool CheckRules::check_infoThreshold(Prior &prior){
//   return estimator.fisherInf(prior) >= infoThreshold ? true : false;
// }
// 
// bool CheckRules::check_gainThreshold(Prior &prior){
//   return estimator.estimatSE(prior) - estimator.fisherInf(estimator.estimateTheta(prior),???? )  >= gainThreshold ? true : false;
// }


// bool CheckRules::check_gainOverride(){
//   return ........... <= gainOverride ? true : false;
// }

// bool CheckRules::checkRules(Prior &prior) {
//   bool answer_lengthTheshold = questionSet.applicable_rows.size() >= lengthThreshold ? true : false;
//   bool answer_lengthOverride = questionSet.applicable_rows.size() <= lengthOverride ? true : false;
//   bool answer_seThreshold = estimator.estimateSE(prior) < seThreshold ? true : false;
//   
//   std::cout<<std::boolalpha;
//   std::cout << "lengthThreshold: "<< answer_lengthTheshold << std::endl;
//   std::cout << "lengthOverride: "<< answer_lengthOverride << std::endl;
//   std::cout << "seThreshold: "<< answer_seThreshold << std::endl;
//   return true;
// }





  





