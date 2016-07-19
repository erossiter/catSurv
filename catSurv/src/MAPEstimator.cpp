#include "MAPEstimator.h"

double MAPEstimator::estimateTheta(Prior prior) {
  int iter = 0;
  int max_iter = 100;
  
	double theta_hat_old = 0.0;
	double theta_hat_new = 1.0;

	const double tolerance = 0.0000001;

	double difference = std::abs(theta_hat_new - theta_hat_old);
	
	while (difference > tolerance && iter < max_iter) {
	  iter++;
		theta_hat_new = theta_hat_old - dLL(theta_hat_old, true, prior) / d2LL(theta_hat_old, true, prior);
		difference = std::abs(theta_hat_new - theta_hat_old);
		theta_hat_old = theta_hat_new;
	}
	
	return theta_hat_new;
}

double MAPEstimator::estimateSE(Prior prior) {
  //double var = 1.0 / (fisherTestInfo(prior) - (1 / pow(prior.parameters[1], 2)));
  double theta = estimateTheta(prior);
  double var = 1.0 / (fisherTestInfo(theta) - (1 / pow(prior.parameters[1], 2)));
  return pow(var, 0.5);
}

EstimationType MAPEstimator::getEstimationType() const {
	return EstimationType::MAP;
}

MAPEstimator::MAPEstimator(Integrator &integrator, QuestionSet &questionSet) : Estimator(integrator, questionSet) { }