#include "WLEEstimator.h"


double WLEEstimator::binary_estimateTheta(Prior prior){
  
  integrableFunction W = [&](double theta) {
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
      std::vector<double> P_prime;
      std::vector<double> P_doub_prime;

      for (size_t k = 1; k < P_star.size(); ++k) {
        double P_star1 = P_star[k];
        double P_star2 = P_star[k-1];
        P.push_back(P_star1 - P_star2);

        double Q_star1 = 1 - P_star1;
        double Q_star2 = 1 - P_star2;

        double first_term = -1 * beta * P_star1 * Q_star1;
        double second_term = -1 * beta * P_star2 * Q_star2;

        double P_prime_k = first_term - second_term;

        P_prime.push_back(P_prime_k);
      }

      for (size_t k = 1; k < P_prime.size(); ++k) {
        double P_prime1 = P_prime[k];
        double P_prime2 = P_prime[k-1];

        double first_term = -1 * beta * (P_prime1 - 2 * P_star[k] * P_prime1);
        double second_term = -1 * beta * (P_prime2 - 2 * P_star[k - 1] * P_prime2);

        double P_doub_prime_k = first_term - second_term;
        P_doub_prime.push_back(P_doub_prime_k);
      }

      for (auto k : P_prime) {
        B += (P_prime[k] * P_doub_prime[k]) / P[k];
      }
      std::cout << B << std::endl;
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
  return 2.0;
}


EstimationType WLEEstimator::getEstimationType() const {
	return EstimationType::WLE;
}

WLEEstimator::WLEEstimator(Integrator &integrator, QuestionSet &questionSet) : Estimator(integrator, questionSet) { }

