#include <Rcpp.h>
using namespace Rcpp;
#include "MAPEstimator.h"

double MAPEstimator::newton_raphson(Prior prior, double theta_hat_old, double theta_hat_new, bool second_try){

    int iter = 0;
    int max_iter = 200;
    
    const double tolerance = 0.0000001;
    
    double difference = std::abs(theta_hat_new - theta_hat_old);
    
    while (difference > tolerance && iter < max_iter) {
        iter++;
        theta_hat_new = theta_hat_old - d1LL(theta_hat_old, true, prior) / d2LL(theta_hat_old, true, prior);
        difference = std::abs(theta_hat_new - theta_hat_old);
        theta_hat_old = theta_hat_new;
    }
    
    // throw an error if first time around we reach max number of iterations
    // it will be caught and we will try again with a better start value
    if(not second_try && iter == max_iter){
        throw std::domain_error("Newton Raphson algorithm reached maximum number of iterations before theta estimate converged.  Trying a different start value.");
    }
    
    // write a warning if the second time around we reach max number of iterations
    if(second_try && iter == max_iter){
        Rcpp::Rcout << "Warning: Newton Raphson algorithm reached maximum number of iterations before theta estimate converged." << std::endl;
    }
    
    return theta_hat_new;
}


double MAPEstimator::newton_raphson(Prior prior, size_t question, int answer, double theta_hat_old, double theta_hat_new, bool second_try){
    int iter = 0;
    int max_iter = 200;
    
    const double tolerance = 0.0000001;
    
    double difference = std::abs(theta_hat_new - theta_hat_old);
    
    while (difference > tolerance && iter < max_iter) {
        iter++;
        theta_hat_new = theta_hat_old - d1LL(theta_hat_old, true, prior, question, answer) / d2LL(theta_hat_old, true, prior, question, answer);
        difference = std::abs(theta_hat_new - theta_hat_old);
        theta_hat_old = theta_hat_new;
    }
    
    if(not second_try && iter == max_iter){
        throw std::domain_error("Newton Raphson algorithm reached maximum number of iterations before theta estimate converged.  Trying a different start value.");
    }
    
    if(second_try && iter == max_iter){
        Rcpp::Rcout << "Warning: Newton Raphson algorithm reached maximum number of iterations before theta estimate converged." << std::endl;
    }
    
    return theta_hat_new;
}



double MAPEstimator::estimateTheta(Prior prior) {
  
  if(questionSet.applicable_rows.empty()){
    return prior.param0();
  }

    double theta_hat_old = 0.0;
    double theta_hat_new = 1.0;
    
    try {
        theta_hat_new = newton_raphson(prior, theta_hat_old, theta_hat_new, false);
    } catch(std::domain_error &) {

        std::vector<double> check_d1LL;
        std::vector<double> try_theta = {-3.5, -3.25, -3.0, -2.75, -2.5, -2.25, -2.0,
                                         -1.75, -1.5, -1.25, -1.0, -0.75, -0.5, -0.25,
                                         0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75,
                                         2.0, 2.25, 2.5, 2.75, 3.0, 3.25, 3.5};
        for(size_t i = 0; i < try_theta.size(); i++){
            double d1ll_val = d1LL(try_theta.at(i), true, prior);
            check_d1LL.push_back(std::abs(d1ll_val));
        }
        auto this_value = std::min_element(std::begin(check_d1LL), std::end(check_d1LL));
        auto this_index = std::distance(std::begin(check_d1LL), this_value);
        theta_hat_old = try_theta.at(this_index);
        theta_hat_new = theta_hat_old - 1.0;
        
        // If there's still an error, it will be thrown.
        theta_hat_new = newton_raphson(prior, theta_hat_old, theta_hat_new, true);
    }
    
    return theta_hat_new;
}

double MAPEstimator::estimateTheta(Prior prior, size_t question, int answer)
{
  
  if(questionSet.applicable_rows.empty()){
    return prior.param0();
  }
    double theta_hat_old = 0.0;
    double theta_hat_new = 1.0;
    try {
        theta_hat_new = newton_raphson(prior, question, answer, theta_hat_old, theta_hat_new, false);
    } catch(std::domain_error &) {
        std::vector<double> check_d1LL;
        std::vector<double> try_theta = {-3.5, -3.25, -3.0, -2.75, -2.5, -2.25, -2.0,
                                         -1.75, -1.5, -1.25, -1.0, -0.75, -0.5, -0.25,
                                         0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75,
                                         2.0, 2.25, 2.5, 2.75, 3.0, 3.25, 3.5};
        for(size_t i = 0; i < try_theta.size(); i++){
            double d1ll_val = d1LL(try_theta.at(i), true, prior);
            check_d1LL.push_back(std::abs(d1ll_val));
        }
        auto this_value = std::min_element(std::begin(check_d1LL), std::end(check_d1LL));
        auto this_index = std::distance(std::begin(check_d1LL), this_value);
        theta_hat_old = try_theta.at(this_index);
        theta_hat_new = theta_hat_old - 1.0;
        
        // If there's still an error, it will be thrown.
        theta_hat_new = newton_raphson(prior, question, answer, theta_hat_old, theta_hat_new, true);
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