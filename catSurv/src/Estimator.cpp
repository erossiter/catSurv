#include "EAPEstimator.h"
#include "GSLFunctionWrapper.h"
#include <limits>
#include <gsl/gsl_roots.h>
#include <gsl/gsl_errno.h>

double Estimator::likelihood(double theta) {
	return questionSet.poly[0] ? polytomous_likelihood(theta) : binary_likelihood(theta);
}

std::vector<double> Estimator::probability(double theta, size_t question) {
  double eps = pow(2, -52);
  eps = pow(eps, 1.0/3.0);
  
	auto calculate = [&](double difficulty) {
		double guess = questionSet.guessing.at(question);
		double exp_prob_bi = exp(difficulty + (questionSet.discrimination.at(question) * theta));
		double exp_prob_poly = exp(difficulty - (questionSet.discrimination.at(question) * theta));
		double result = questionSet.poly[0] ? exp_prob_poly / (1 + exp_prob_poly) : guess + (1 - guess) * exp_prob_bi / (1 + exp_prob_bi);
		
		if(std::isinf(exp_prob_bi)){
		  result = 1.0 - eps;
		}
		if(std::isinf(exp_prob_poly)){
		 result = 1.0 - eps;
		}
		if(result > (1.0 - eps)){
		  result = 1.0 - eps;
		}
		if(result < eps){
		  result = eps;
		}
		return result;
	};

	std::vector<double> probabilities;
	for (auto term : questionSet.difficulty.at(question)) {
		probabilities.push_back(calculate(term));
	}

	return probabilities;
}

double Estimator::polytomous_likelihood(double theta) {
	double L = 0.0;

	for (auto question : questionSet.applicable_rows) {
		size_t unsigned_question = (size_t) question;
		auto question_cdf = paddedProbability(theta, unsigned_question);

		// TODO: Determine what should happen when a negative answer is given
		// TODO: that is, when a user doesn't respond, the value will be negative
		// TODO: Which will result in an out-of-bounds array access

		// TODO: Absolute value and reserve array, if needed.
		int index = questionSet.answers.at(unsigned_question);
		L += log(question_cdf.at((size_t) index) - question_cdf.at(((size_t) index) - 1)) ;
	}
	return exp(L);
}

double Estimator::binary_likelihood(double theta) {
	double L = 0.0;
	for (auto question : questionSet.applicable_rows) {
		size_t index = (size_t) question;
		double prob = probability(theta, index)[0];
		int this_answer = questionSet.answers.at(index);
		L += (this_answer * log(prob)) + ((1 - this_answer) * log(1 - prob));
	}
	return exp(L);
}

Estimator::Estimator(Integrator &integration, QuestionSet &question) : integrator(integration), questionSet(question) { }


double Estimator::polytomous_posterior_variance(int item, Prior &prior) {
  auto question_cdf = paddedProbability(estimateTheta(prior), (size_t) item);
  
  questionSet.applicable_rows.push_back(item);
  
	std::vector<double> variances;
	for (size_t i = 0; i <= questionSet.difficulty[item].size(); ++i) {
		questionSet.answers[item] = (int) i + 1.0;
		variances.push_back(pow(estimateSE(prior), 2));
	}

	double sum = 0;
	for (size_t i = 1; i < question_cdf.size(); ++i) {
		sum += variances[i-1] * (question_cdf[i] - question_cdf[i - 1]);
	}
	
	questionSet.applicable_rows.pop_back();
	return sum;
}

double Estimator::binary_posterior_variance(int item, Prior &prior) {
  const double probability_incorrect = probability(estimateTheta(prior), (size_t) item)[0];
  
  questionSet.applicable_rows.push_back(item);
  
	questionSet.answers[item] = 1;
	double variance_correct = pow(estimateSE(prior), 2);

	questionSet.answers[item] = 0;
	double variance_incorrect = pow(estimateSE(prior), 2);
	
	questionSet.applicable_rows.pop_back();

	return (probability_incorrect * variance_correct) + ((1.0 - probability_incorrect) * variance_incorrect);
}

double Estimator::expectedPV(int item, Prior &prior) {
  double result = questionSet.poly[0] ? polytomous_posterior_variance(item, prior) : binary_posterior_variance(item, prior);
	questionSet.answers[item] = NA_INTEGER;
	return result;
}

double Estimator::partial_second_derivative(double theta, size_t question) {
	size_t answer_k = (size_t) questionSet.answers.at(question);

	auto probabilities = paddedProbability(theta, question);

	double P_star1 = probabilities.at(answer_k);
	double P_star2 = probabilities.at(answer_k - 1);
	double P = P_star1 - P_star2;

	double Q_star1 = 1 - P_star1;
	double Q_star2 = 1 - P_star2;

	double w2 = P_star2 * Q_star2;
	double w1 = P_star1 * Q_star1;
	double w = w1 - w2;

	double first_term = (-w2 * (Q_star2 - P_star2) + w1 * (Q_star1 - P_star1)) / P;
	double second_term = pow(w, 2) / pow(P, 2);

	return first_term - second_term;
}

double Estimator::obsInf(double theta, int item) {
  
	if (questionSet.applicable_rows.empty()) {
		throw std::domain_error("ObsInf should not be called if no items have been answered.");
	}

	size_t index = (size_t) item;
	double discrimination = questionSet.discrimination.at(index);

	if (questionSet.poly[0]) {
	  return -pow(discrimination, 2) * partial_second_derivative(theta, index);
	}

	double guess = questionSet.guessing.at(index);
	double P = probability(theta, index)[0];
	double Q = 1 - P;
	double temp = pow((P - guess) / (1 - guess), 2);
	return pow(discrimination, 2) * temp * (Q / P);
}

double Estimator::fisherInf(double theta, int item) {

	if (!questionSet.poly[0]) {
		return obsInf(theta, item);
	}

	double output = 0.0;
	auto probabilities = paddedProbability(theta, (size_t) item);

	double discrimination_squared = pow(questionSet.discrimination[item], 2);
	for (size_t i = 1; i <= questionSet.difficulty[item].size() + 1; ++i) {
		double P_star1 = probabilities[i];
		double P_star2 = probabilities[i - 1];
		double w1 = P_star1 * (1.0 - P_star1);
		double w2 = P_star2 * (1.0 - P_star2);
		
		output += discrimination_squared * (pow(w1 - w2, 2) / (P_star1 - P_star2));
	}
	return output;
}

/**
 * Computes the probability for a given theta and question number,
 * then pads it with a 1.0 at the beginning, and a 0.0 at the end.
 */
std::vector<double> Estimator::paddedProbability(double theta, size_t question) {
	std::vector<double> probabilities = probability(theta, question);
	std::vector<double> padded{0.0};
	padded.insert(padded.end(), probabilities.begin(), probabilities.end());
	padded.push_back(1.0);
	return padded;
}

double Estimator::expectedObsInf(int item, Prior &prior) {

	if (questionSet.poly[0]){
	  std::vector<double> question_cdf = paddedProbability(estimateTheta(prior), (size_t) item);
	  questionSet.applicable_rows.push_back(item);
	  
		double sum = 0.0;
		std::vector<double> obsInfs;
		for (size_t i = 0; i <= questionSet.difficulty[item].size(); ++i){
			questionSet.answers[item] = (int) i + 1;
			obsInfs.push_back(obsInf(estimateTheta(prior), item));
		}

		questionSet.answers[item] = NA_INTEGER;
		questionSet.applicable_rows.pop_back();

	for (size_t i = 1; i < question_cdf.size(); ++i) {
			sum += obsInfs[i-1] * (question_cdf[i] - question_cdf[i - 1]);
		}
		return sum;
	}

	double prob_one = probability(estimateTheta(prior), (size_t) item)[0];
	questionSet.applicable_rows.push_back(item);
	
	questionSet.answers[item] = 0;
	double obsInfZero = obsInf(estimateTheta(prior), item);
	questionSet.answers[item] = 1;
	double obsInfOne = obsInf(estimateTheta(prior), item);
	
	questionSet.applicable_rows.pop_back();
	questionSet.answers[item] = NA_INTEGER;

	return (prob_one * obsInfOne) + ((1 - prob_one) * obsInfZero);
}


double Estimator::findRoot(){
  
  integrableFunction dLL = [&](double theta) {
    double l_theta = 0.0;
	  for (auto question : questionSet.applicable_rows) {
		  const int answer_k = questionSet.answers[question];

		  auto probabilities = probability(theta, (size_t) question);
		  std::vector<double> probs{0.0};
		  probs.insert(probs.end(), probabilities.begin(), probabilities.end());
		  probs.push_back(1.0);

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
  
  return brentMethod(dLL);
}

double Estimator::brentMethod(integrableFunction const &function) {
  int status;
  int iter = 0;
  int max_iter = 100;
  
  const gsl_root_fsolver_type *T;
  gsl_root_fsolver *s;
  
  double r = 0;
  double x_lo = -5.0;
  double x_hi = 5.0;
  
	gsl_function *F = GSLFunctionWrapper(function).asGSLFunction();
	
	T = gsl_root_fsolver_brent;
  s = gsl_root_fsolver_alloc (T);
  
  // This function initializes, or reinitializes, an existing solver s
  // to use the function f and the initial search interval [x_lower, x_upper].
  gsl_root_fsolver_set (s, F, x_lo, x_hi);

  do {
      iter++;
      status = gsl_root_fsolver_iterate (s);
      r = gsl_root_fsolver_root (s);
      x_lo = gsl_root_fsolver_x_lower (s);
      x_hi = gsl_root_fsolver_x_upper (s);
      
      // This function tests for the convergence of the interval [x_lower, x_upper]
      // with absolute error epsabs and relative error epsrel. 
      // The test returns GSL_SUCCESS if the following condition is achieved,
      // |a - b| < epsabs + epsrel min(|a|,|b|) 
      double epsabs = 0;
      double epsrel = 0.0000001;
      
      status = gsl_root_test_interval (x_lo, x_hi, epsabs, epsrel);
    } 
  while (status == GSL_CONTINUE && iter < max_iter);

  gsl_root_fsolver_free (s);
  
  return r;
}
  
  
double Estimator::fisherTestInfo(Prior prior) {
  double theta = estimateTheta(prior);
  double sum = 0.0;
  for (auto item : questionSet.applicable_rows) {
    sum += fisherInf(theta, item);
  }
  return sum;
}
  
/**
 * pwi(), lwi(), and all kl functions define the integration that needs to be
 * performed for each question that their respective selectItem()
 * function will call in a loop
 */
double Estimator::pwi(int item, Prior prior) {

	integrableFunction pwi_j = [&](double theta) {
		return likelihood(theta) * prior.prior(theta) * fisherInf(theta, item);
	};

	return integrate_selectItem(pwi_j);
}

double Estimator::lwi(int item) {

	integrableFunction lwi_j = [&](double theta) {
		return likelihood(theta) * fisherInf(theta, item);
	};

	return integrate_selectItem(lwi_j);
}

double Estimator::fii(int item, Prior prior) {
  
	integrableFunction fii_j = [&](double theta_not) {
		return fisherInf(theta_not, item);
	};
	  
  double delta = questionSet.z[0] * pow(fisherTestInfo(prior), 0.5);
  
  const double lower = estimateTheta(prior) - delta;
  const double upper = estimateTheta(prior) + delta;

	return integrate_selectItem_bounds(fii_j, lower, upper);
}


double Estimator::kl(double theta_not, int item, Prior prior){
  
  if(questionSet.poly[0]){
    auto cdf_theta_not = paddedProbability(theta_not, (size_t) item);
	  auto cdf_theta_hat = paddedProbability(estimateTheta(prior), (size_t) item);
	  
	  double sum = 0.0;
	  for (size_t i = 1; i < cdf_theta_hat.size(); ++i) {
	    double prob_theta_not = cdf_theta_not[i] - cdf_theta_not[i - 1];
	    double prob_theta_hat = cdf_theta_hat[i] - cdf_theta_hat[i - 1];
	    sum += prob_theta_not * (log(prob_theta_not) - log(prob_theta_hat));
	  }
	  return sum; 
	}
  
  const double prob_theta_not = probability(theta_not, (size_t) item)[0];
  const double prob_theta_hat = probability(estimateTheta(prior), (size_t) item)[0];

  double first_term = prob_theta_not * (log(prob_theta_not) - log(prob_theta_hat));
  double second_term = (1 - prob_theta_not) * (log(1 - prob_theta_not) - log(1 - prob_theta_hat));

  return first_term + second_term;
}


double Estimator::expectedKL(int item, Prior prior) {
	
	integrableFunction kl_fctn = [&](double theta_not) {
	  return kl(theta_not, item, prior);
  };
  
  double delta = questionSet.z[0] * pow(fisherTestInfo(prior), 0.5);
  
  const double lower = estimateTheta(prior) - delta;
  const double upper = estimateTheta(prior) + delta;

  return integrate_selectItem_bounds(kl_fctn, lower, upper);
}


double Estimator::likelihoodKL(int item, Prior prior) {
	
	integrableFunction kl_fctn = [&](double theta_not) {
	  return likelihood(theta_not) * kl(theta_not, item, prior);
  };

  return integrate_selectItem(kl_fctn);
}


double Estimator::posteriorKL(int item, Prior prior) {
	
	integrableFunction kl_fctn = [&](double theta_not) {
	  return prior.prior(theta_not) * likelihood(theta_not) * kl(theta_not, item, prior);
  };

  return integrate_selectItem(kl_fctn);
}


double Estimator::integrate_selectItem(const integrableFunction &function){
  gsl_function *f = GSLFunctionWrapper(function).asGSLFunction();
	return integrator.integrate(f, integrationSubintervals);
}

double Estimator::integrate_selectItem_bounds(const integrableFunction &function, const double lower, const double upper){
  gsl_function *f = GSLFunctionWrapper(function).asGSLFunction();
  return integrator.integrate(f, integrationSubintervals, lower, upper);
}
