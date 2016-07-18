#include "WLEEstimator.h"


double WLEEstimator::binary_estimateTheta(Prior prior){
  
  integrableFunction w_fctn = [&](double theta) {
    double B = 0.0;
    double I = 0.0;
    for (auto item : questionSet.applicable_rows) {
      double a = questionSet.discrimination.at(item);
      double b = questionSet.difficulty.at(item)[0];
      double c = questionSet.guessing.at(item);

      double exp_part = exp(a + b * theta);
      double P_1 = b * (1 - c) * (exp_part / pow((1 + exp_part), 2.0));
      double P_2 = pow(b, 2.0) * exp_part * (1 - exp_part) * ((1 - c) / pow((1 + exp_part), 3.0));

      double prob_correct = probability(theta, item)[0];
      B += (P_1 * P_2) / (prob_correct * (1.0 - prob_correct));
      I += fisherInf(theta, item);
    }
    double L_theta = dLL(theta, false, prior);
    return L_theta + (B / (2 * I));
  };
  return brentMethod(w_fctn);
}

double WLEEstimator::poly_estimateTheta(Prior prior){
  
  // integrableFunction w_fctn = [&](double theta) {
  //   double B = 0.0;
  //   double I = 0.0;
  //   for (auto item : questionSet.applicable_rows) {
  //     double a = questionSet.discrimination.at(item);
  //     double b = questionSet.difficulty.at(item)[0];
  //     double c = questionSet.guessing.at(item);
  // 
  //     double exp_part = exp(a + b * theta);
  //     double P_1 = b * (1 - c) * (exp_part / pow((1 + exp_part), 2.0));
  //     double P_2 = pow(b, 2.0) * exp_part * (1 - exp_part) * ((1 - c) / pow((1 + exp_part), 3.0));
  // 
  //     double prob_correct = probability(theta, item)[0];
  //     B += (P_1 * P_2) / (prob_correct * (1.0 - prob_correct));
  //     I += fisherInf(theta, item);
  //   }
  //   double L_theta = dLL(theta, false, prior);
  //   return L_theta + (B / (2 * I));
  // };
  // return brentMethod(w_fctn);
  
  return 1.0;
}



double WLEEstimator::estimateTheta(Prior prior) {
  if(questionSet.poly[0]){
    return poly_estimateTheta(prior);
  }

  return binary_estimateTheta(prior);
}
    
//     for (size_t i = 0; i < questionSet.nonapplicable_rows.size(); ++i) {
// 		  int item = questionSet.nonapplicable_rows.at(i);
//       
//       double a = questionSet.discrimination[item];
//       double b = questionSet.difficulty[item];
//       double c = questionSet.guessing[item];
//       
//       std::vector<double> probs = padded
//       
//       for (auto k : questionSet.difficulty.at(item)) {
//       
// 
//       
//       double exp_part = exp(a + b * theta_est);
//       double P_1 = b * (1 - c) * (exp_part / pow((1 + exp_part), 2.0));
//       double P_2 = pow(b, 2.0) * exp_part * (1 - exp_part) * ((1 - c) / pow((1 + exp_part), 3.0));
//   
//       double prob_correct = probability(theta_est, item);
//       B += (P_1 * P_2) / (prob_correct * (1 - prob_correct);
//
//    }
//     
//     for (size_t i = 0; i < questionSet.nonapplicable_rows.size(); ++i) {
// 		  int item = questionSet.nonapplicable_rows.at(i);
//       
//       double a = questionSet.discrimination[item];
//       std::vector<double> b = questionSet.difficulty[item];
//       double c = questionSet.guessing[item];
//       
//       double exp_part = exp(a + b * estimateTheta(prior));
//       double P_1 = b * (1 - c) * (exp_part / pow((1 + exp_part), 2.0));
//       double P_2 = pow(b, 2.0) * exp_part * (1 - exp_part) * ((1 - c) / pow((1 + exp_part), 3.0));
//   
//       std::vector<double> prob_correct = probability(estimateTheta(prior), item);
//       B += (P_1 * P_2) / (prob_correct * (1 - prob_correct);
//     }
//   
//   double L_theta = dLL(estimateTheta(prior), false, prior);
//   double I = fisherTestInfo(prior);
//   
//   return L_theta + (B / (2 * I));

double WLEEstimator::estimateSE(Prior prior) {
  return 2.0;
}


EstimationType WLEEstimator::getEstimationType() const {
	return EstimationType::WLE;
}

WLEEstimator::WLEEstimator(Integrator &integrator, QuestionSet &questionSet) : Estimator(integrator, questionSet) { }

