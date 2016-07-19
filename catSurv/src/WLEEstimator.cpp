#include "WLEEstimator.h"


double WLEEstimator::binary_estimateTheta(Prior prior){
  
  integrableFunction W = [&](double theta) {
    double B = 0.0;
    double I = 0.0;
    for (auto item : questionSet.applicable_rows) {
      double a = questionSet.difficulty.at(item)[0];
      double b = questionSet.discrimination.at(item);
      double c = questionSet.guessing.at(item);

      double exp_part = exp(a + b * theta);
      double dP = b * (1 - c) * (exp_part / pow((1 + exp_part), 2.0));
      double d2P = pow(b, 2.0) * exp_part * (1 - exp_part) * ((1 - c) / pow((1 + exp_part), 3.0));

      double P = probability(theta, item)[0];
      B += (dP * d2P) / (P * (1.0 - P));
      I += fisherInf(theta, item);
    }
    double L_theta = dLL(theta, false, prior);
    return L_theta + (B / (2 * I));
  };
  
  return brentMethod(W);
}

double WLEEstimator::poly_estimateTheta(Prior prior){
  
  integrableFunction W = [&](double theta) {
    double B = 0.0;
    double I = 0.0;

    for (auto item : questionSet.applicable_rows) {
      I += fisherInf(theta, item);
      
      double beta = questionSet.discrimination.at(item);
      std::vector<double> P_star = paddedProbability(theta, item);
      std::vector<double> P;
      std::vector<double> dP;
      std::vector<double> d2P;

      for (size_t k = 1; k < P_star.size(); ++k) {
        double P_star1 = P_star.at(k);
        double P_star2 = P_star.at(k-1);
        P.push_back(P_star1 - P_star2);
        
        double Q_star1 = 1 - P_star1;
        double Q_star2 = 1 - P_star2;

        double first_term = -1 * beta * P_star1 * Q_star1;
        double second_term = -1 * beta * P_star2 * Q_star2;

        dP.push_back(first_term - second_term);
      }
      
      for (size_t k = 1; k < dP.size(); ++k) {
        double P_prime1 = dP.at(k);
        double P_prime2 = dP.at(k-1);

        double first_term = -1 * beta * (P_prime1 - 2 * P_star[k] * P_prime1);
        double second_term = -1 * beta * (P_prime2 - 2 * P_star[k - 1] * P_prime2);

        d2P.push_back(first_term - second_term);
      }
      
      for (size_t k = 0; k < dP.size() - 1; ++k) {
        B += (dP.at(k) * d2P.at(k)) / P.at(k);
      }
    }
    
    double L_theta = dLL(theta, false, prior);
    return L_theta + (B / (2 * I));
  };

  return brentMethod(W);
}


double WLEEstimator::estimateTheta(Prior prior) {
  return questionSet.poly[0] ? poly_estimateTheta(prior) : binary_estimateTheta(prior);
}


double WLEEstimator::estimateSE(Prior prior) {
  double I_theta = fisherTestInfo(prior);
  double var = I_theta / pow(I_theta, 2.0);
  return pow(var, 0.5);
}


EstimationType WLEEstimator::getEstimationType() const {
	return EstimationType::WLE;
}

WLEEstimator::WLEEstimator(Integrator &integrator, QuestionSet &questionSet) : Estimator(integrator, questionSet) { }

