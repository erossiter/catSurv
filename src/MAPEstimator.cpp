#include <Rcpp.h>
using namespace Rcpp;
#include "MAPEstimator.h"

double MAPEstimator::estimateTheta(Prior prior) {
    
    
	double theta_hat_old = 0.0;
	double theta_hat_new = 1.0;
	double difference = std::abs(theta_hat_new - theta_hat_old);
	
	// check to see that the difference is not increasing!
	std::vector<double> check_diffs;
	for(int i = 1; i < 4; i++) {
	    theta_hat_new = theta_hat_old - d1LL(theta_hat_old, true, prior) / d2LL(theta_hat_old, true, prior);
	    difference = std::abs(theta_hat_new - theta_hat_old);
	    check_diffs.push_back(difference);
	    theta_hat_old = theta_hat_new;
	}
	if(check_diffs[1] > check_diffs[0]){ //} || check_diffs[2] > check_diffs[1]){ //difference shows we aren't "climbing" the likelihood
	    theta_hat_old = 2.0; //estimate me!!;
	    theta_hat_new = 1.0; //estimate me + 1???
	    difference = std::abs(theta_hat_new - theta_hat_old); 
	}else{ //start over
	    theta_hat_old = 0.0;
	    theta_hat_new = 1.0;
	    difference = std::abs(theta_hat_new - theta_hat_old); 
	}
	
	// now complete Newton-Raphson
	int iter = 0;
	int max_iter = 200;
	const double tolerance = 0.0000001;
	while (difference > tolerance && iter < max_iter) {
	    iter++;
		theta_hat_new = theta_hat_old - d1LL(theta_hat_old, true, prior) / d2LL(theta_hat_old, true, prior);
		difference = std::abs(theta_hat_new - theta_hat_old);
		std::cout<<difference<<std::endl;
		theta_hat_old = theta_hat_new;
	}
	
	return theta_hat_new;
}

double MAPEstimator::estimateTheta(Prior prior, size_t question, int answer)
{
	int iter = 0;
  	int max_iter = 200;
  
	double theta_hat_old = 0.0;
	double theta_hat_new = 1.0;

	const double tolerance = 0.0000001;

	double difference = std::abs(theta_hat_new - theta_hat_old);
	
	while (difference > tolerance && iter < max_iter) {
	  	iter++;
		theta_hat_new = theta_hat_old - d1LL(theta_hat_old, true, prior,question,answer) / d2LL(theta_hat_old, true, prior,question,answer);
		difference = std::abs(theta_hat_new - theta_hat_old);
		theta_hat_old = theta_hat_new;
	}
	
	return theta_hat_new;
}

double MAPEstimator::estimateSE(Prior prior) {
  double var = 1.0 / (fisherTestInfo(prior) + (1 / std::pow(prior.param1(), 2)));
  return std::pow(var, 0.5);
}

double MAPEstimator::estimateSE(Prior prior, size_t question, int answer)
{
	double var = 1.0 / (fisherTestInfo(prior,question,answer) + (1 / std::pow(prior.param1(), 2)));
  	return std::pow(var, 0.5);
}

EstimationType MAPEstimator::getEstimationType() const {
	return EstimationType::MAP;
}

MAPEstimator::MAPEstimator(Integrator &integrator, QuestionSet &questionSet) : Estimator(integrator, questionSet) { }