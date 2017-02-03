#include "MLEEstimator.h"


double MLEEstimator::dLL_root(){

  integrableFunction dLL_fctn = [&](double theta) {
    double l_theta = 0.0;
	  for (auto question : questionSet.applicable_rows) {
		  const int answer_k = questionSet.answers[question];

		  auto probs = probability(theta, (size_t) question);

		  double P_star1 = probs[answer_k];
		  double Q_star1 = 1.0 - P_star1;
		  double P_star2 = probs[answer_k - 1];
		  double Q_star2 = 1 - P_star2;
		  double P = P_star1 - P_star2;
		  double w2 = P_star2 * Q_star2;
		  double w1 = P_star1 * Q_star1;

		  l_theta += (-1*questionSet.discrimination[question] * ((w1 - w2) / P));
		  }
	  return l_theta;
	  };

  return brentMethod(dLL_fctn);
}

double MLEEstimator::estimateTheta(Prior prior) {
  int iter = 0;
  int max_iter = 200;
  
	double theta_hat_old = 0.0;
	double theta_hat_new = 1.0;

	const double tolerance = 0.0000001;

	double difference = std::abs(theta_hat_new - theta_hat_old);
	
	while (difference > tolerance && iter < max_iter) {
	  iter++;
		theta_hat_new = theta_hat_old - dLL(theta_hat_old, false, prior) / d2LL(theta_hat_old, false, prior);
		difference = std::abs(theta_hat_new - theta_hat_old);
		theta_hat_old = theta_hat_new;
		if(isnan(theta_hat_old)){
		  theta_hat_new = dLL_root();
		  break;
		}
	}
	
	return theta_hat_new;
}

double MLEEstimator::estimateSE(Prior prior) {
  double var = 1.0 / fisherTestInfo(prior);
  return pow(var, 0.5);
}
  

EstimationType MLEEstimator::getEstimationType() const {
	return EstimationType::MLE;
}

MLEEstimator::MLEEstimator(Integrator &integrator, QuestionSet &questionSet) : Estimator(integrator, questionSet) { }

