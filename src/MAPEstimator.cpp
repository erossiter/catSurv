#include <Rcpp.h>
using namespace Rcpp;
#include "MAPEstimator.h"

double MAPEstimator::estimateTheta(Prior prior) {
    double theta_hat_old = 0.0;
    double theta_hat_new = 1.0;
    double difference = std::abs(theta_hat_new - theta_hat_old);
    
	// check to see that the difference is not increasing,
	// meaning that we are off track right away and need a better start value
	// std::vector<double> check_diffs;
	// for(int i = 1; i < 3; i++) {
	//     theta_hat_new = theta_hat_old - d1LL(theta_hat_old, true, prior) / d2LL(theta_hat_old, true, prior);
	//     difference = std::abs(theta_hat_new - theta_hat_old);
	//     check_diffs.push_back(difference);
	//     theta_hat_old = theta_hat_new;
	// }
	// std::cout<<"first difference: " << check_diffs[0] <<std::endl;
	// std::cout<<"second difference: " << check_diffs[1] << std::endl;
	// if(check_diffs[1] > check_diffs[0]){ 
	    // Difference shows we aren't "climbing" the likelihood
	    // Pick smallest first deriv because we're likely picking up on a slope
	    std::vector<double> check_d1LL;
	    std::vector<double> try_theta = {-3.5, -3.0, -2.5, -2.0, -1.5, -1.0, -0.5, 0.0,
                                          0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5};
	    for(int i = 0; i < try_theta.size(); i++){
	        double d1ll_val = d1LL(try_theta.at(i), true, prior);
	        check_d1LL.push_back(std::abs(d1ll_val));
	        std::cout << try_theta.at(i) << "  " << std::abs(d1ll_val) <<std::endl;
	    }
	    auto this_value = std::min_element(std::begin(check_d1LL), std::end(check_d1LL));
	    auto this_index = std::distance(std::begin(check_d1LL), this_value);
	    std::cout<<"chose: " << this_index << "..." << try_theta.at(this_index) << std::endl;
	    theta_hat_old = try_theta.at(this_index);
	    theta_hat_new = theta_hat_old - 1.0;
	    difference = std::abs(theta_hat_new - theta_hat_old);
	// }else{
	//     //then original start value should be okay -- reset it
	//     theta_hat_old = 0.0;
	//     theta_hat_new = 1.0;
	//     difference = std::abs(theta_hat_new - theta_hat_old);
	// }
	
	// now complete Newton-Raphson
	int iter = 0;
	int max_iter = 200;
	const double tolerance = 0.0000001;
	while (difference > tolerance && iter < max_iter) {
	    iter++;
		theta_hat_new = theta_hat_old - d1LL(theta_hat_old, true, prior) / d2LL(theta_hat_old, true, prior);
		difference = std::abs(theta_hat_new - theta_hat_old);
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