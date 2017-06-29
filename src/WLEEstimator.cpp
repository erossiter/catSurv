#include "WLEEstimator.h"


double WLEEstimator::ltm_estimateTheta(Prior prior){
  
  integrableFunction W = [&](double theta) {
    double B = 0.0;
    double I = 0.0;
    for (auto item : questionSet.applicable_rows) {
      double a = (questionSet.difficulty.at(item)).at(0);
      double b = questionSet.discrimination.at(item);
      double c = questionSet.guessing.at(item);

      double exp_part = exp(a + b * theta);
      double dP = b * (1 - c) * (exp_part / std::pow((1.0 + exp_part), 2.0));
      double d2P = std::pow(b, 2.0) * exp_part * (1 - exp_part) * ((1 - c) / std::pow((1.0 + exp_part), 3.0));

      double P = prob_ltm(theta, item);
      B += (dP * d2P) / (P * (1.0 - P));
      I += fisherInf(theta, item);
    }
    double L_theta = d1LL(theta, false, prior);
    return L_theta + (B / (2 * I));
  };
  
  return brentMethod(W);
}

double WLEEstimator::ltm_estimateTheta(Prior prior, size_t question, int answer){
  
  integrableFunction W = [&](double theta) {
    double B = 0.0;
    double I = 0.0;
    for (auto item : questionSet.applicable_rows) {
      double a = (questionSet.difficulty.at(item)).at(0);
      double b = questionSet.discrimination.at(item);
      double c = questionSet.guessing.at(item);

      double exp_part = exp(a + b * theta);
      double dP = b * (1 - c) * (exp_part / std::pow((1.0 + exp_part), 2.0));
      double d2P = std::pow(b, 2.0) * exp_part * (1 - exp_part) * ((1 - c) / std::pow((1.0 + exp_part), 3.0));

      double P = prob_ltm(theta, item);
      B += (dP * d2P) / (P * (1.0 - P));
      I += fisherInf(theta, item, questionSet.answers.at(item));
    }

    double a = questionSet.difficulty.at(question).at(0);
    double b = questionSet.discrimination.at(question);
    double c = questionSet.guessing.at(question);

    double exp_part = exp(a + b * theta);
    double dP = b * (1 - c) * (exp_part / std::pow((1.0 + exp_part), 2.0));
    double d2P = std::pow(b, 2.0) * exp_part * (1 - exp_part) * ((1 - c) / std::pow((1.0 + exp_part), 3.0));

    double P = prob_ltm(theta, question);
    B += (dP * d2P) / (P * (1.0 - P));
    I += fisherInf(theta, question, answer);

    double L_theta = d1LL(theta, false, prior, question, answer);
    return L_theta + (B / (2 * I));
  };
  
  return brentMethod(W);
}

double WLEEstimator::gpcm_estimateTheta(Prior prior){
  
  integrableFunction W = [&](double theta) {
    double B = 0.0;
    double I = 0.0;

    std::vector<double> p;
    std::vector<double> p_prime;
    std::vector<double> p_primeprime;
  
    for (auto item : questionSet.applicable_rows) {
      I += fisherInf(theta, item);
      
      prob_derivs_gpcm(theta, item, p, p_prime, p_primeprime);
      
      for (size_t k = 0; k < p.size(); ++k) {
        B += (p_prime.at(k) * p_primeprime.at(k)) / p.at(k);
      }
    }
    double L_theta = d1LL(theta, false, prior);
    return L_theta + (B / (2 * I));
  };
  
  return brentMethod(W);
}

double WLEEstimator::gpcm_estimateTheta(Prior prior, size_t question, int answer){
  
  integrableFunction W = [&](double theta) {
    double B = 0.0;
    double I = 0.0;
  
    std::vector<double> p;
    std::vector<double> p_prime;
    std::vector<double> p_primeprime;

    for (auto item : questionSet.applicable_rows) {
      I += fisherInf(theta, item, questionSet.answers.at(item));
      
      prob_derivs_gpcm(theta, item, p, p_prime, p_primeprime);
      
      for (size_t k = 0; k < p.size(); ++k) {
        B += (p_prime.at(k) * p_primeprime.at(k)) / p.at(k);
      }
    }

    I += fisherInf(theta, question, answer);
    
    prob_derivs_gpcm(theta, question, p, p_prime, p_primeprime);
    
    for (size_t k = 0; k < p.size(); ++k) {
      B += (p_prime.at(k) * p_primeprime.at(k)) / p.at(k);
    }


    double L_theta = d1LL(theta, false, prior, question, answer);
    return L_theta + (B / (2 * I));
  };
  
  return brentMethod(W);
}

double WLEEstimator::grm_estimateTheta(Prior prior){
  
  integrableFunction W = [&](double theta) {
    double B = 0.0;
    double I = 0.0;

    for (auto item : questionSet.applicable_rows) {
      I += fisherInf(theta, item);
      
      double beta = questionSet.discrimination.at(item);
      std::vector<double> P_stars = prob_grm(theta, item);
      
      std::vector<double> P;
      for (size_t i = 1; i < P_stars.size(); ++i) {
        double P1 = P_stars.at(i);
        double P2 = P_stars.at(i-1);
        P.push_back(P1 - P2);
      }
    
      std::vector<double> P_star_prime;
      std::vector<double> P_star_2prime;
      std::vector<double> P_prime;
      std::vector<double> P_2prime;

      for (size_t k = 0; k < P_stars.size(); ++k) {
        double P_star = P_stars.at(k);
        double Q_star = 1 - P_star;
        double P_star_p = -1 * beta * P_star * Q_star;
        P_star_prime.push_back(P_star_p);
        
        double P_star_2p = -1 * beta * (P_star_p - (2 * P_star * P_star_p));
        P_star_2prime.push_back(P_star_2p);
      }
      
      for (size_t j = 1; j < P_star_prime.size(); ++j) {
        double P_prime1 = P_star_prime.at(j);
        double P_prime2 = P_star_prime.at(j-1);
        P_prime.push_back(P_prime1 - P_prime2);
      
        double P_2prime1 = P_star_2prime.at(j);
        double P_2prime2 = P_star_2prime.at(j-1);
        P_2prime.push_back(P_2prime1 - P_2prime2);
      }
    
      for (size_t k = 0; k < P.size(); ++k) {
        B += (P_prime.at(k) * P_2prime.at(k)) / P.at(k);
      }
    }
    
    double L_theta = d1LL(theta, false, prior);

    return L_theta + (B / (2 * I));
  };
  
  return brentMethod(W);
}

double WLEEstimator::grm_estimateTheta(Prior prior, size_t question, int answer){
  
  integrableFunction W = [&](double theta) {
    double B = 0.0;
    double I = 0.0;

    std::vector<double> P_star_prime;
    std::vector<double> P_star_2prime;

    for (auto item : questionSet.applicable_rows) {
      I += fisherInf(theta, item, questionSet.answers.at(item));
      
      double beta = questionSet.discrimination.at(item);
      std::vector<double> P_stars = prob_grm(theta, item);

      P_star_prime.clear();
      P_star_prime.reserve(P_stars.size());

      P_star_2prime.clear();
      P_star_2prime.reserve(P_stars.size());

      for (auto P_star : P_stars) {
        double Q_star = 1 - P_star;
        double P_star_p = -1 * beta * P_star * Q_star;
        P_star_prime.push_back(P_star_p);
        
        double P_star_2p = -1 * beta * (P_star_p - (2 * P_star * P_star_p));
        P_star_2prime.push_back(P_star_2p);
      }
      
      for (size_t j = 1; j < P_stars.size(); ++j) {
        double P_prime = (P_star_prime.at(j) - P_star_prime.at(j-1));      
        double P_2prime = (P_star_2prime.at(j) - P_star_2prime.at(j-1));
        double P = P_stars.at(j) - P_stars.at(j-1);
        B += (P_prime* P_2prime) / P;
      }

    }

    I += fisherInf(theta, question, answer);
    
    double beta = questionSet.discrimination.at(question);
    std::vector<double> P_stars = prob_grm(theta, question);

    P_star_prime.clear();
    P_star_prime.reserve(P_stars.size());

    P_star_2prime.clear();
    P_star_2prime.reserve(P_stars.size());

    for (auto P_star : P_stars) {
      double Q_star = 1 - P_star;
      double P_star_p = -1 * beta * P_star * Q_star;
      P_star_prime.push_back(P_star_p);
      
      double P_star_2p = -1 * beta * (P_star_p - (2 * P_star * P_star_p));
      P_star_2prime.push_back(P_star_2p);
    }
    
    for (size_t j = 1; j < P_stars.size(); ++j) {
      double P_prime = (P_star_prime.at(j) - P_star_prime.at(j-1));      
      double P_2prime = (P_star_2prime.at(j) - P_star_2prime.at(j-1));
      double P = P_stars.at(j) - P_stars.at(j-1);
      B += (P_prime* P_2prime) / P;
    }
    
    double L_theta = d1LL(theta, false, prior, question, answer);

    return L_theta + (B / (2 * I));
  };
  
  return brentMethod(W);
}

double WLEEstimator::estimateTheta(Prior prior) {
  double theta = 0.0;

  if ((questionSet.model == "ltm") | (questionSet.model == "tpm")) {
	  theta = ltm_estimateTheta(prior);
	}
	if (questionSet.model == "grm") {
	  theta = grm_estimateTheta(prior);
	}
	if (questionSet.model == "gpcm"){
		theta = gpcm_estimateTheta(prior);
	}
	
	return theta;
}

double WLEEstimator::estimateTheta(Prior prior, size_t question, int answer)
{
  double theta = 0.0;

  if ((questionSet.model == "ltm") | (questionSet.model == "tpm")) {
    theta = ltm_estimateTheta(prior, question, answer);
  }
  if (questionSet.model == "grm") {
    theta = grm_estimateTheta(prior, question, answer);
  }
  if (questionSet.model == "gpcm"){
    theta = gpcm_estimateTheta(prior, question, answer);
  }
  
  return theta;
}


double WLEEstimator::estimateSE(Prior prior) {
  double I_theta = fisherTestInfo(prior);
  double var = 1 / I_theta;
  return std::pow(var, 0.5);
}

double WLEEstimator::estimateSE(Prior prior, size_t question, int answer)
{
  double I_theta = fisherTestInfo(prior, question, answer);
  return std::pow(1 / I_theta, 0.5);
}


EstimationType WLEEstimator::getEstimationType() const {
	return EstimationType::WLE;
}

WLEEstimator::WLEEstimator(Integrator &integrator, QuestionSet &questionSet) : Estimator(integrator, questionSet) { }

