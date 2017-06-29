#include "EAPEstimator.h"
#include "GSLFunctionWrapper.h"
#include <limits>
#include <numeric>
#include <algorithm>
#include <gsl/gsl_math.h>
#include <gsl/gsl_roots.h>
#include <gsl/gsl_errno.h>

  
double Estimator::prob_ltm(double theta, size_t question) {
	double eps = std::pow(std::pow(2.0, -52.0), 1.0/3.0);

	double difficulty = questionSet.difficulty.at(question).at(0);
	double exp_prob_bi = exp(difficulty + (questionSet.discrimination.at(question) * theta));

	if(std::isinf(exp_prob_bi))
	{
		return 1.0 - eps;
	}

	double guess = questionSet.guessing.at(question);
    double result = guess + (1 - guess) * (exp_prob_bi / (1 + exp_prob_bi));
  
  	if(result > (1.0 - eps))
  	{
	  result = 1.0 - eps;
	}
	else if(result < eps)
	{
	  result = eps;
	}

	return result;
}

struct GrmProb
{
	GrmProb(double theta, double discrimination)
	: theta_desc(theta*discrimination)
	{}

	double operator()(double difficulty) const
	{
		double exp_prob = exp(difficulty - theta_desc);

		if(std::isinf(exp_prob))
		{
		 	return 1.0 - eps;
		}

		double result = exp_prob / (1 + exp_prob);
		
		if(result > (1.0 - eps))
		{
		  	result = 1.0 - eps;
		}
		else if(result < eps)
		{
		  	result = eps;
		}
		return result;
	}

private:
	double eps = std::pow(std::pow(2.0, -52.0), 1.0/3.0);
	double theta_desc;
};

std::vector<double> Estimator::prob_grm(double theta, size_t question) {
	GrmProb calculate{theta, questionSet.discrimination.at(question)};

	std::vector<double> probabilities;
	probabilities.reserve(questionSet.difficulty.size()+2);
	probabilities.push_back(0.0);

	for (auto term : questionSet.difficulty.at(question)) {
		probabilities.push_back(calculate(term));
	}

	probabilities.push_back(1.0);

	// checking for repeated elements
  	auto it = std::adjacent_find(probabilities.begin(), probabilities.end());
  	if(it != probabilities.end()){
    	throw std::domain_error("Theta value too extreme for numerical routines.");
  	}

	return probabilities;
}

std::pair<double,double> Estimator::prob_grm_pair(double theta, size_t question, size_t at)
{
	// Returns prob at at-1 and at
	GrmProb calculate{theta, questionSet.discrimination.at(question)};
	std::pair<double, double> probs;

	auto const& difficulties = questionSet.difficulty.at(question);

	if(at == 1)
	{
		probs.first = 0.0;
	}
	else
	{
		probs.first = calculate(difficulties[at-2]);
	}

	if(at == difficulties.size()+1)
	{
		probs.second = 1.0;
	}
	else
	{
		probs.second = calculate(difficulties[at-1]);
	}

	// checking for repeated elements
  	if(probs.first == probs.second)
  	{
    	throw std::domain_error("Theta value too extreme for numerical routines.");
  	}

	return probs;
}

std::vector<double> Estimator::prob_gpcm(double theta, size_t question) {
  	// double xmax = pow(2, 52);
  	// double eps = pow(2, -52);
  	// eps = pow(eps, 1.0/3.0);
    
  	double discrimination = questionSet.discrimination.at(question);
  	auto const & categoryparams = questionSet.difficulty.at(question);
 
  	std::vector<double> probabilities;
  	probabilities.reserve(categoryparams.size()+1); 	

  	double sum = discrimination * theta;
  	double denominator = exp(sum);
  	probabilities.push_back(denominator);

	for (auto cat : categoryparams) {
	  	sum += discrimination * (theta - cat);
	  	double num = exp(sum);
	  	denominator += num;
		probabilities.push_back(num);
	}
	
	if(denominator == 0.0 or std::isinf(denominator)){
    	throw std::domain_error("Theta value too extreme for numerical routines.");
  	}

  	// normalize
  	for (auto& prob:probabilities)
  	{
  		prob /= denominator;
	}
  
	return probabilities;
}

double Estimator::prob_gpcm_at(double theta, size_t question, size_t at)
{
	double discrimination = questionSet.discrimination.at(question);
  	auto const & categoryparams = questionSet.difficulty.at(question);

  	double sum = discrimination * theta;
  	double denominator = exp(sum);

  	double result = -1;
  	if(at == 0)
  	{
  		result = denominator;

  		for (auto cat : categoryparams)
  		{
		  	sum += discrimination * (theta - cat);
		  	denominator += exp(sum);
		}
	}
	else
	{
		at -= 1;
		for (size_t i = 0; i != at; ++i)
  		{
  			sum += discrimination * (theta - categoryparams[i]);
		  	denominator += exp(sum);
  		}

  		sum += discrimination * (theta - categoryparams[at]);
  		result = exp(sum);
		denominator += result;

		for (size_t i = at+1; i < categoryparams.size(); ++i)
  		{
  			sum += discrimination * (theta - categoryparams[i]);
		  	denominator += exp(sum);
  		}
	}
	
	if(denominator == 0.0 or std::isinf(denominator)){
    	throw std::domain_error("Theta value too extreme for numerical routines.");
  	}

  	// normalize 
	return result/denominator;
}

double Estimator::gpcm_partial_d1LL(double theta, size_t question, int answer) {
	size_t index = ((size_t)answer) - 1;

	double discrimination = questionSet.discrimination.at(question);
  	auto const & categoryparams = questionSet.difficulty.at(question);
 
	double f = -1;
	double f_prime = -1;
	double sum = discrimination * (theta - 0.0);
  	double g = exp(sum);
  	double x = discrimination;
  	double g_prime = g*x;

  	if(index == 0)
  	{
  		f = g;
  		f_prime = g_prime;

  		for (auto cat : categoryparams)
  		{
		  	sum += discrimination * (theta - cat);
	  		double num = exp(sum);
	  		x += discrimination;
	  		g += num;
	  		g_prime += num*x;
		}
	}
	else
	{
		index -= 1;
		for (size_t i = 0; i != index; ++i)
  		{
  			sum += discrimination * (theta - categoryparams[i]);
  			double num = exp(sum);
	  		x += discrimination;
	  		g += num;
	  		g_prime += num*x;
  		}

  		sum += discrimination * (theta - categoryparams[index]);
  		f = exp(sum);
  		x += discrimination;
	  	f_prime = f*x;
	  	g += f;
	  	g_prime += f_prime;

		for (size_t i = index+1; i < categoryparams.size(); ++i)
  		{
  			sum += discrimination * (theta - categoryparams[i]);
		  	double num = exp(sum);
	  		x += discrimination;
	  		g += num;
	  		g_prime += num*x;
  		}
	}

	if(g == 0.0 or std::isinf(g)){
    	throw std::domain_error("Theta value too extreme for numerical routines.");
  	}

  	return (g*f_prime - f*g_prime)/(g*f);
}

double Estimator::gpcm_partial_d2LL(double theta, size_t question, int answer) {
	size_t index = ((size_t)answer) - 1;

	double discrimination = questionSet.discrimination.at(question);
  	auto const & categoryparams = questionSet.difficulty.at(question);
 
	double f = -1;
	double f_prime = -1;
	double f_primeprime = -1;
	double sum = discrimination * (theta - 0.0);
  	double g = exp(sum);
  	double x = discrimination;
  	double g_prime = g*x;
  	double g_primeprime = g_prime*x;


  	if(index == 0)
  	{
  		f = g;
  		f_prime = g_prime;
  		f_primeprime = g_primeprime;

  		for (auto cat : categoryparams)
  		{
		  	sum += discrimination * (theta - cat);
	  		double num = exp(sum);
	  		x += discrimination;
	  		double num_x = num*x;
	  		g += num;
	  		g_prime += num_x;
	  		g_primeprime += num_x*x;
		}
	}
	else
	{
		index -= 1;
		for (size_t i = 0; i != index; ++i)
  		{
  			sum += discrimination * (theta - categoryparams[i]);
  			double num = exp(sum);
	  		x += discrimination;
	  		double num_x = num*x;
	  		g += num;
	  		g_prime += num_x;
	  		g_primeprime += num_x*x;
  		}

  		sum += discrimination * (theta - categoryparams[index]);
  		f = exp(sum);
  		x += discrimination;
	  	f_prime = f*x;
	  	f_primeprime = f_prime*x;
	  	g += f;
	  	g_prime += f_prime;
	  	g_primeprime += f_primeprime;

		for (size_t i = index+1; i < categoryparams.size(); ++i)
  		{
  			sum += discrimination * (theta - categoryparams[i]);
		  	double num = exp(sum);
	  		x += discrimination;
	  		double num_x = num*x;
	  		g += num;
	  		g_prime += num_x;
	  		g_primeprime += num_x*x;
  		}
	}

	if(g == 0.0 or std::isinf(g)){
    	throw std::domain_error("Theta value too extreme for numerical routines.");
  	}

	double b = g*g;
	double b2 = b*b;
	double b_prime = 2.0 * g * g_prime;
    		
	double a = g * f_prime - f * g_prime;
	f_prime = a / b; // p_prime

	double a_prime = f_primeprime * g - g_primeprime * f;
	f_primeprime = (b * a_prime - a * b_prime) / b2; // p_primeprime

	f /= g; // p
	
  	return - ((f_prime*f_prime/f - f_primeprime) / f);
}

std::vector<double> Estimator::prob_derivs_gpcm_first(double theta, size_t question)
{
	double discrimination = questionSet.discrimination.at(question);
  	auto const & categoryparams = questionSet.difficulty.at(question);
 
  	std::vector<double> f;
  	std::vector<double> f_prime;

  	f.reserve(categoryparams.size()+1);
  	f_prime.reserve(categoryparams.size()+1); 

  	double sum = discrimination * theta;
  	double num = exp(sum);
  	double x = discrimination;
  	double g = num;
  	double g_prime = num*x;
  	f.push_back(num);
  	f_prime.push_back(g_prime);
  	
	for (auto cat : categoryparams) {
	  	sum += discrimination * (theta - cat);
	  	double num = exp(sum);
	  	x += discrimination;
	  	double num_x = num*x;
	  	g += num;
	  	g_prime += num_x;
		f.push_back(num);
		f_prime.push_back(num_x);
	}
	
	double b = g*g;
	for (size_t i = 0; i < f.size(); ++i)
  	{    
    	f_prime[i] = (g * f_prime.at(i) - f.at(i) * g_prime) / b;
	}
	return f_prime;
}


void Estimator::prob_derivs_gpcm(double theta, size_t question, std::vector<double>& probs, std::vector<double>& first, std::vector<double>& second){
  	double discrimination = questionSet.discrimination.at(question);
  	auto const & categoryparams = questionSet.difficulty.at(question);
 
  	probs.clear();
  	probs.reserve(categoryparams.size()+1);
  	first.clear();
  	first.reserve(categoryparams.size()+1); 
  	second.clear();
  	second.reserve(categoryparams.size()+1); 

  	double sum = discrimination * theta;
  	double x = discrimination;
  	double g = exp(sum);
  	double g_prime = g*x;
  	double g_primeprime = g_prime*x;

  	probs.push_back(g);
  	first.push_back(g_prime);
	second.push_back(g_primeprime);
  	
	for (auto cat : categoryparams) {
	  	sum += discrimination * (theta - cat);
	  	double num = exp(sum);
	  	x += discrimination;
	  	double num_x = num*x;
	  	double num_xx = num_x*x;

	  	g += num;
	  	g_prime += num_x;
	  	g_primeprime += num_xx;

		probs.push_back(num);
		first.push_back(num_x);
		second.push_back(num_xx);
	}
	
	double b = g*g;
	double b2 = b*b;
	double b_prime = 2.0 * g * g_prime;

  	for (size_t i = 0; i < probs.size(); ++i)
  	{     		
  		double a = g * first.at(i) - probs.at(i) * g_prime;
    	first[i] = a / b;

    	double a_prime = second.at(i) * g - g_primeprime * probs.at(i);
    	second[i] = (b * a_prime - a * b_prime) / b2;

    	probs[i] /= g;
	}
}

std::vector<double> Estimator::probability(double theta, size_t question) {
  if (question > questionSet.answers.size() ) {
    throw std::domain_error("Must use a question number applicable to Cat object.");
  }
  
  	std::vector<double> probabilities;

	if(questionSet.model == "grm") {
	  	probabilities = prob_grm(theta, question);
	}
	else if (questionSet.model == "gpcm"){
		probabilities = prob_gpcm(theta, question);
	}
	else if (questionSet.model == "ltm" || questionSet.model == "tpm")
	{
		probabilities.reserve(1);
		probabilities.push_back(prob_ltm(theta, question));
	}
	
	return probabilities;
}



double Estimator::likelihood_grm(double theta) {
	double L = 0.0;

	for (auto question : questionSet.applicable_rows) {
		size_t unanswered_question = (size_t) question;
	  	int answer = questionSet.answers.at(unanswered_question);
		auto probs = prob_grm_pair(theta, question, answer);
		L += log(probs.second- probs.first) ;
	}
	return exp(L);
}

double Estimator::likelihood_gpcm(double theta) {
	double L = 0.0;

	for (auto question : questionSet.applicable_rows) {
		size_t unanswered_question = (size_t) question;
	  	size_t answer = questionSet.answers.at(unanswered_question);
    	// index probabilities correctly using the answer
    	L += log(prob_gpcm_at(theta, unanswered_question, answer-1));
	}
	return exp(L);
}

double Estimator::likelihood_ltm(double theta) {
	double L = 0.0;
	for (auto question : questionSet.applicable_rows) {
		size_t index = (size_t) question;
		double prob = prob_ltm(theta, index);
		int this_answer = questionSet.answers.at(index);
		L += (this_answer * log(prob)) + ((1 - this_answer) * log(1 - prob));
	}
	return exp(L);
}

double Estimator::likelihood(double theta) {
  double likelihood = 0.0;

  if ((questionSet.model == "ltm") | (questionSet.model == "tpm")) {
	  likelihood = likelihood_ltm(theta);
	}
	if (questionSet.model == "grm") {
	  likelihood = likelihood_grm(theta);
	}
	if (questionSet.model == "gpcm"){
		likelihood = likelihood_gpcm(theta);
	}
	
	return likelihood;
}


double Estimator::likelihood_grm(double theta, size_t question, int answer) {
	double L = 0.0;

	for (auto q : questionSet.applicable_rows) {
	  	size_t a = (size_t)questionSet.answers.at((size_t) q);
    	auto probs = prob_grm_pair(theta, q, a);
		L += log(probs.second - probs.first) ;
	}

    auto probs = prob_grm_pair(theta, question, answer);
	L += log(probs.second - probs.first) ;

	return exp(L);
}

double Estimator::likelihood_gpcm(double theta,size_t question, int answer) {
	double L = 0.0;

	for (auto q : questionSet.applicable_rows) {
		size_t unanswered_question = (size_t) q;
	  	auto a = (size_t)questionSet.answers.at(unanswered_question);
    	L += log(prob_gpcm_at(theta, unanswered_question, a-1));
	}

    L += log(prob_gpcm_at(theta, question, ((size_t)answer)-1));

	return exp(L);
}

double Estimator::likelihood_ltm(double theta,size_t question, int answer) {
	double L = 0.0;
	for (auto q : questionSet.applicable_rows) {
		size_t index = (size_t) q;
		double prob = prob_ltm(theta, index);
		int this_answer = questionSet.answers.at(index);
		L += (this_answer * log(prob)) + ((1 - this_answer) * log(1 - prob));
	}

	double prob = prob_ltm(theta, question);
	L += (answer * log(prob)) + ((1 - answer) * log(1 - prob));

	return exp(L);
}

double Estimator::likelihood(double theta, size_t question, int answer){
	 double likelihood = 0.0;

  if ((questionSet.model == "ltm") | (questionSet.model == "tpm")) {
	  likelihood = likelihood_ltm(theta,question,answer);
	}
	if (questionSet.model == "grm") {
	  likelihood = likelihood_grm(theta,question,answer);
	}
	if (questionSet.model == "gpcm"){
		likelihood = likelihood_gpcm(theta,question,answer);
	}
	
	return likelihood;
}

double Estimator::grm_partial_d2LL(double theta, size_t question) {
	size_t answer_k = (size_t) questionSet.answers.at(question);

	double P_star1;
	double P_star2;

	std::tie(P_star2, P_star1) = prob_grm_pair(theta, question, answer_k);

	double P = P_star1 - P_star2;

	double Q_star1 = 1 - P_star1;
	double Q_star2 = 1 - P_star2;

	double w2 = P_star2 * Q_star2;
	double w1 = P_star1 * Q_star1;
	double w = w1 - w2;

	double first_term = (-w2 * (Q_star2 - P_star2) + w1 * (Q_star1 - P_star1)) / P;
	double second_term = std::pow(w, 2.0) / std::pow(P, 2.0);
	return first_term - second_term;
}

double Estimator::grm_partial_d2LL(double theta, size_t question, int answer) {
	double P_star1;
	double P_star2;

	std::tie(P_star2, P_star1) = prob_grm_pair(theta, question, answer);

	double P = P_star1 - P_star2;

	double Q_star1 = 1 - P_star1;
	double Q_star2 = 1 - P_star2;

	double w2 = P_star2 * Q_star2;
	double w1 = P_star1 * Q_star1;
	double w = w1 - w2;

	double first_term = (-w2 * (Q_star2 - P_star2) + w1 * (Q_star1 - P_star1)) / P;
	double second_term = std::pow(w, 2.0) / std::pow(P, 2.0);
	return first_term - second_term;
}

double Estimator::gpcm_partial_d2LL(double theta, size_t question) {
	return gpcm_partial_d2LL(theta,question,questionSet.answers.at(question));
}

double Estimator::gpcm_partial_d1LL(double theta, size_t question) {

  	return gpcm_partial_d1LL(theta, question, questionSet.answers.at(question));
}


double Estimator::gpcm_d2LL(double theta) {
  double d2l = 0.0;

	for (auto question : questionSet.applicable_rows) {
    	d2l += gpcm_partial_d2LL(theta, question);
	}
	
	return d2l;
}

double Estimator::gpcm_d2LL(double theta, size_t question, int answer) {
  double d2l = 0.0;

	for (auto q : questionSet.applicable_rows) {
		int a = questionSet.answers.at(q);
    	d2l += gpcm_partial_d2LL(theta, q, a);
	}
	
	d2l += gpcm_partial_d2LL(theta, question, answer);
	
	return d2l;
}

double Estimator::grm_d2LL(double theta) {
	double lambda_theta = 0.0;
	for (auto question : questionSet.applicable_rows) {
		const double question_discrimination = std::pow(questionSet.discrimination.at(question), 2.0);
		const double second_derivative = grm_partial_d2LL(theta, (size_t) question);

		lambda_theta += question_discrimination * second_derivative;
	}
	return lambda_theta;
}

double Estimator::grm_d2LL(double theta, size_t question, int answer) {
	double lambda_theta = 0.0;
	for (auto q : questionSet.applicable_rows) {
		double question_discrimination = std::pow(questionSet.discrimination.at(q), 2.0);
		int a = questionSet.answers.at(q);
		double second_derivative = grm_partial_d2LL(theta, (size_t) q, a);
		lambda_theta += question_discrimination * second_derivative;
	}

	double question_discrimination = std::pow(questionSet.discrimination.at(question), 2.0);
	double second_derivative = grm_partial_d2LL(theta, (size_t) question, answer);
	lambda_theta += question_discrimination * second_derivative;

	return lambda_theta;
}

double Estimator::ltm_d2LL(double theta) {
	double lambda_theta = 0.0;
	for (auto question : questionSet.applicable_rows) {
		const double P = prob_ltm(theta, (size_t) question);
		const double guess = questionSet.guessing.at(question);
		const double Q = 1.0 - P;
		const double lambda_temp = (P - guess) / (1.0 - guess);
		const double discrimination = questionSet.discrimination.at(question);

		lambda_theta += std::pow(discrimination, 2.0) * std::pow(lambda_temp, 2.0) * (Q / P);
	}
	return -lambda_theta;
}

double Estimator::ltm_d2LL(double theta, size_t question, int answer) {
	double lambda_theta = 0.0;
	for (auto q : questionSet.applicable_rows) {
		double P = prob_ltm(theta, (size_t) q);
		double guess = questionSet.guessing.at(q);
		double Q = 1.0 - P;
		double lambda_temp = (P - guess) / (1.0 - guess);
		double discrimination = questionSet.discrimination.at(q);

		lambda_theta += std::pow(discrimination*lambda_temp, 2.0) * (Q / P);
	}

	double P = prob_ltm(theta, question);
	double guess = questionSet.guessing.at(question);
	double Q = 1.0 - P;
	double lambda_temp = (P - guess) / (1.0 - guess);
	double discrimination = questionSet.discrimination.at(question);

	lambda_theta += std::pow(discrimination*lambda_temp, 2.0) * (Q / P);

	return -lambda_theta;
}

double Estimator::gpcm_d1LL(double theta) {
  double d1l = 0.0;

	for (auto question : questionSet.applicable_rows) {
    d1l += gpcm_partial_d1LL(theta, question);
	 }
	
	return d1l;
}

double Estimator::gpcm_d1LL(double theta, size_t question, int answer) {
  	double d1l = 0.0;

	for (auto q : questionSet.applicable_rows) {
		int a = questionSet.answers.at(q);
    	d1l += gpcm_partial_d1LL(theta, q, a);
	}

	d1l += gpcm_partial_d1LL(theta, question, answer);
	
	return d1l;
}

double Estimator::grm_d1LL(double theta) {
	double l_theta = 0.0;
	for (auto question : questionSet.applicable_rows) {
		int answer_k = questionSet.answers.at(question);

		double P_star2, P_star1;
		std::tie(P_star2, P_star1) = prob_grm_pair(theta, question, answer_k);

		double Q_star1 = 1.0 - P_star1;
		double Q_star2 = 1 - P_star2;
		double P = P_star1 - P_star2;
		double w2 = P_star2 * Q_star2;
		double w1 = P_star1 * Q_star1;

		l_theta += (-1*questionSet.discrimination.at(question) * ((w1 - w2) / P));
	}
	return l_theta;
}

double Estimator::grm_d1LL(double theta, size_t question, int answer) {
	double l_theta = 0.0;
	for (auto q : questionSet.applicable_rows) {
		int answer_k = questionSet.answers.at(q);
		double P_star2, P_star1;
		std::tie(P_star2, P_star1) = prob_grm_pair(theta, q, answer_k);
		double P = P_star1 - P_star2;
		double w = P_star1 * (1.0 - P_star1) - P_star2 * (1 - P_star2);

		l_theta += (-1*questionSet.discrimination.at(q) * (w/ P));
	}

	double P_star2, P_star1;
	std::tie(P_star2, P_star1) = prob_grm_pair(theta, question, answer);
	double P = P_star1 - P_star2;
	double w = P_star1 * (1.0 - P_star1) - P_star2 * (1 - P_star2);

	l_theta += (-1*questionSet.discrimination.at(question) * (w / P));

	return l_theta;
}

double Estimator::ltm_d1LL(double theta) {
	double l_theta = 0;
	for (auto question : questionSet.applicable_rows) {
		const double P = prob_ltm(theta, question);
		const double guess = questionSet.guessing.at(question);
		const double answer = questionSet.answers.at(question);
		const double discrimination = questionSet.discrimination.at(question);
		l_theta += discrimination * ((P - guess) / (P * (1 - guess))) * (answer - P);
	}
	return l_theta;
}

double Estimator::ltm_d1LL(double theta, size_t question, int answer) {
	double l_theta = 0;
	for (auto q : questionSet.applicable_rows) {
		double P = prob_ltm(theta, q);
		double guess = questionSet.guessing.at(q);
		double a = questionSet.answers.at(q);
		double discrimination = questionSet.discrimination.at(q);
		l_theta += discrimination * ((P - guess) / (P * (1 - guess))) * (a - P);
	}

	double P = prob_ltm(theta, question);
	double guess = questionSet.guessing.at(question);
	double discrimination = questionSet.discrimination.at(question);
	l_theta += discrimination * ((P - guess) / (P * (1 - guess))) * (answer - P);

	return l_theta;
}


double Estimator::d1LL(double theta, bool use_prior, Prior &prior) {
	const double prior_shift = (theta - prior.param0()) / std::pow(prior.param1(), 2.0);
	if (questionSet.applicable_rows.empty()) {
		return prior_shift;
	}
	double l_theta = 0.0;
	
	if ((questionSet.model == "ltm") | (questionSet.model == "tpm")) {
	  l_theta = ltm_d1LL(theta);
	}
	if (questionSet.model == "grm") {
	  l_theta = grm_d1LL(theta);
	}
	if (questionSet.model == "gpcm"){
		l_theta = gpcm_d1LL(theta);
	}
	
	return use_prior ? l_theta - prior_shift : l_theta;
}

double Estimator::d1LL(double theta, bool use_prior, Prior &prior, size_t question, int answer) {
	double l_theta = 0.0;
	
	if ((questionSet.model == "ltm") | (questionSet.model == "tpm")) {
	  l_theta = ltm_d1LL(theta, question, answer);
	}
	if (questionSet.model == "grm") {
	  l_theta = grm_d1LL(theta, question, answer);
	}
	if (questionSet.model == "gpcm"){
		l_theta = gpcm_d1LL(theta, question, answer);
	}

	if (use_prior)
	{
		l_theta -= ((theta - prior.param0()) / std::pow(prior.param1(), 2.0));
	}
	return l_theta;
}

double Estimator::d2LL(double theta, bool use_prior, Prior &prior) {
	const double prior_shift = 1.0 / std::pow(prior.param1(), 2.0);
	if (questionSet.applicable_rows.empty()) {
		return -prior_shift;
	}
	double lambda_theta = 0.0;
	
	if ((questionSet.model == "ltm") | (questionSet.model == "tpm")) {
	  lambda_theta = ltm_d2LL(theta);
	}
	if (questionSet.model == "grm") {
	  lambda_theta = grm_d2LL(theta);
	}
	if (questionSet.model == "gpcm"){
		lambda_theta = gpcm_d2LL(theta);
	}
	return use_prior ? lambda_theta - prior_shift : lambda_theta;
}

double Estimator::d2LL(double theta, bool use_prior, Prior &prior, size_t question, int answer) {
	double lambda_theta = 0.0;
	
	if ((questionSet.model == "ltm") | (questionSet.model == "tpm")) {
	  lambda_theta = ltm_d2LL(theta, question, answer);
	}
	if (questionSet.model == "grm") {
	  lambda_theta = grm_d2LL(theta, question, answer);
	}
	if (questionSet.model == "gpcm"){
		lambda_theta = gpcm_d2LL(theta, question, answer);
	}

	if(use_prior)
	{
		lambda_theta -= (1.0 / std::pow(prior.param1(), 2.0));
	}
	return lambda_theta;
}



Estimator::Estimator(Integrator &integration, QuestionSet &question) : integrator(integration), questionSet(question) { }

double Estimator::polytomous_posterior_variance(int item, Prior &prior) {
	double theta_old = estimateTheta(prior);
  
  	questionSet.applicable_rows.push_back(item);
 
	std::vector<double> variances;
	for (size_t i = 0; i <= questionSet.difficulty.at(item).size(); ++i) {
		questionSet.answers.at(item) = (int) i + 1;
		variances.push_back(std::pow(estimateSE(prior), 2.0));
	}

	double sum = 0;
	if (questionSet.model == "grm") {
		auto probabilities = prob_grm(theta_old, (size_t) item);
	  	for (size_t i = 1; i < probabilities.size(); ++i) {
	    	sum += variances.at(i-1) * (probabilities.at(i) - probabilities.at(i-1));
	    }
	}
	if (questionSet.model == "gpcm"){
		auto probabilities = prob_gpcm(theta_old, (size_t) item);
	  	for (size_t i = 0; i < probabilities.size(); ++i) {
	    	sum += variances.at(i) * probabilities.at(i);
	    }
	}
	
	questionSet.applicable_rows.pop_back();
	return sum;
}

double Estimator::binary_posterior_variance(int item, Prior &prior) {
  const double prob_incorrect = prob_ltm(estimateTheta(prior), (size_t) item);
  
  questionSet.applicable_rows.push_back(item);
  
	questionSet.answers.at(item) = 1;
	double variance_correct = std::pow(estimateSE(prior), 2.0);

	questionSet.answers.at(item) = 0;
	double variance_incorrect = std::pow(estimateSE(prior), 2.0);
	
	questionSet.applicable_rows.pop_back();

	return (prob_incorrect * variance_correct) + ((1.0 - prob_incorrect) * variance_incorrect);
}

double Estimator::expectedPV(int item, Prior &prior) {
	double result = 0.0;
  
	if ((questionSet.model == "ltm") | (questionSet.model == "tpm")) {
	  result = binary_posterior_variance(item, prior);
	}
	if (questionSet.model == "grm") {
	  result = polytomous_posterior_variance(item, prior);
	}
	if (questionSet.model == "gpcm"){
		result = polytomous_posterior_variance(item, prior);
	}
	
	questionSet.answers.at(item) = NA_INTEGER;
	return result;
}

double Estimator::expectedPV_ltm_tpm(int item, Prior &prior)
{
	//binary_posterior_variance
	double prob_incorrect = prob_ltm(estimateTheta(prior), (size_t) item);
    
	double variance_correct = std::pow(estimateSE(prior,item,1), 2.0);
	double variance_incorrect = std::pow(estimateSE(prior,item,0), 2.0);
	
	return (prob_incorrect * variance_correct) + ((1.0 - prob_incorrect) * variance_incorrect);
}

double Estimator::expectedPV_grm(int item, Prior &prior)
{
	//polytomous_posterior_variance
	   
	double sum = 0;
	auto probabilities = prob_grm(estimateTheta(prior), (size_t) item);
  	for (size_t i = 1; i < probabilities.size(); ++i) {
  		double var = std::pow(estimateSE(prior,item,(int)i), 2.0);
    	sum += var * (probabilities.at(i) - probabilities.at(i-1));
    }
	
	return sum;
}

double Estimator::expectedPV_gpcm(int item, Prior &prior)
{
	//polytomous_posterior_variance
	double sum = 0;
	auto probabilities = prob_gpcm(estimateTheta(prior), (size_t) item);
  	for (size_t i = 0; i < probabilities.size(); ++i) {
  		double var = std::pow(estimateSE(prior,item,(int) i + 1), 2.0);
    	sum += var * probabilities.at(i);
    }
	
	return sum;
}

double Estimator::obsInf(double theta, int item) {
	if(questionSet.model == "grm"){
	  return obsInf_grm(theta, item);
	}	
	else if(questionSet.model == "gpcm"){
	  return obsInf_gpcm(theta, item);
	}
	else
	{
		return obsInf_ltm(theta, item);
	}
}

double Estimator::obsInf(double theta, int item, int answer) {
	if(questionSet.model == "grm"){
	  return obsInf_grm(theta, item, answer);
	}	
	else if(questionSet.model == "gpcm"){
	  return obsInf_gpcm(theta, item, answer);
	}
	else
	{
		return obsInf_ltm(theta, item, answer);
	}
}

double Estimator::obsInf_grm(double theta, int item)
{	double discrimination = questionSet.discrimination.at(item);
	return -discrimination*discrimination * grm_partial_d2LL(theta, item);
}
double Estimator::obsInf_grm(double theta, int item, int answer)
{
	double discrimination = questionSet.discrimination.at(item);
	return -discrimination*discrimination* grm_partial_d2LL(theta, item, answer);
}

double Estimator::obsInf_gpcm(double theta, int item)
{
	return -gpcm_partial_d2LL(theta, item);
}

double Estimator::obsInf_gpcm(double theta, int item, int answer)
{
	return -gpcm_partial_d2LL(theta, item, answer);
}

double Estimator::obsInf_ltm(double theta, int item)
{
	double discrimination = questionSet.discrimination.at(item);
	double guess = questionSet.guessing.at(item);
	double P = prob_ltm(theta, item);
	double Q = 1 - P;
	double temp = std::pow((P - guess) / (1.0 - guess), 2.0);
	return discrimination*discrimination * temp * (Q / P);
}
double Estimator::obsInf_ltm(double theta, int item, int answer)
{
	return obsInf_ltm(theta, item);
}


double Estimator::fisherInf(double theta, int item) {

	if ((questionSet.model == "ltm") || (questionSet.model == "tpm")) {
		return obsInf_ltm(theta, item);
	}

	double output = 0.0;

	if (questionSet.model == "grm") {
		double discrimination_squared = std::pow(questionSet.discrimination.at(item), 2.0);
		auto probabilities = prob_grm(theta, (size_t) item);
	  	for (size_t i = 1; i <= questionSet.difficulty.at(item).size() + 1; ++i) {
		  double P_star1 = probabilities.at(i);
		  double P_star2 = probabilities.at(i-1);
		  double w1 = P_star1 * (1.0 - P_star1);
		  double w2 = P_star2 * (1.0 - P_star2);
		  output += discrimination_squared * (std::pow(w1 - w2, 2.0) / (P_star1 - P_star2));
		}
	}
	else if (questionSet.model == "gpcm"){
		std::vector<double> probs;
	  	std::vector<double> prob_firstderiv;
		std::vector<double> prob_secondderiv;
		prob_derivs_gpcm(theta, item, probs, prob_firstderiv, prob_secondderiv);

	  	for (size_t i = 0; i < probs.size(); ++i) {
		  double p = probs.at(i);
		  double p_prime = prob_firstderiv.at(i);
		  double p_primeprime = prob_secondderiv.at(i);
		  output += (std::pow(p_prime, 2.0) / p) - p_primeprime;
	  }
	}
	return output;
}

double Estimator::fisherInf(double theta, int item, int answer) {

	if ((questionSet.model == "ltm") | (questionSet.model == "tpm")) {
		return obsInf_ltm(theta, item, answer);
	}

	double output = 0.0;

	if (questionSet.model == "grm") {
		double discrimination_squared = std::pow(questionSet.discrimination.at(item), 2.0);
		auto probabilities = prob_grm(theta, (size_t) item);
	  	for (size_t i = 1; i <= questionSet.difficulty.at(item).size() + 1; ++i) {
		  double P_star1 = probabilities.at(i);
		  double P_star2 = probabilities.at(i-1);
		  double w1 = P_star1 * (1.0 - P_star1);
		  double w2 = P_star2 * (1.0 - P_star2);
		  output += discrimination_squared * (std::pow(w1 - w2, 2.0) / (P_star1 - P_star2));
		}
	}	
	else if (questionSet.model == "gpcm"){
		std::vector<double> probs;
	  	std::vector<double> prob_firstderiv;
		std::vector<double> prob_secondderiv;
		prob_derivs_gpcm(theta, item, probs, prob_firstderiv, prob_secondderiv);

	  	for (size_t i = 0; i < probs.size(); ++i) {
		  double p = probs.at(i);
		  double p_prime = prob_firstderiv.at(i);
		  double p_primeprime = prob_secondderiv.at(i);
		  output += (std::pow(p_prime, 2.0) / p) - p_primeprime;
	  }
	}
	return output;
}

double Estimator::expectedObsInf(int item, Prior &prior) {

	
	if (questionSet.model == "grm"){
		auto probabilities = prob_grm(estimateTheta(prior), (size_t) item);
	  	
	  	questionSet.applicable_rows.push_back(item);

		double sum = 0.0;
	  	for (size_t i = 1; i < probabilities.size(); ++i) {
	  		questionSet.answers.at(item) = (int) i;
			double obsinfo = obsInf_grm(estimateTheta(prior), item);
	    	sum += obsinfo * (probabilities.at(i) - probabilities.at(i-1));
     	}

		questionSet.answers.at(item) = NA_INTEGER;
		questionSet.applicable_rows.pop_back();
		
		return sum;
	}
	else if (questionSet.model == "gpcm"){
		auto probabilities = prob_gpcm(estimateTheta(prior), (size_t) item);

	  	questionSet.applicable_rows.push_back(item);
	  
		double sum = 0.0;
	  	for (size_t i = 0; i < probabilities.size(); ++i) {
	  		questionSet.answers.at(item) = (int) i + 1;
			double obsinfo = obsInf_gpcm(estimateTheta(prior), item);
      		sum += obsinfo * probabilities.at(i);
    	}

		questionSet.answers.at(item) = NA_INTEGER;
		questionSet.applicable_rows.pop_back();
				
		return sum;
	}

	double prob_one = prob_ltm(estimateTheta(prior), (size_t) item);
	questionSet.applicable_rows.push_back(item);
	
	questionSet.answers.at(item) = 0;
	double obsInfZero = obsInf_ltm(estimateTheta(prior), item);
	questionSet.answers.at(item) = 1;
	double obsInfOne = obsInf_ltm(estimateTheta(prior), item);
	
	questionSet.applicable_rows.pop_back();
	questionSet.answers.at(item) = NA_INTEGER;

	return (prob_one * obsInfOne) + ((1 - prob_one) * obsInfZero);
}

double Estimator::expectedObsInf_grm(int item, Prior &prior)
{
	std::vector<double> probabilities = prob_grm(estimateTheta(prior), (size_t) item);
	double sum = 0.0;

	for(size_t i = 1; i < probabilities.size(); ++i){
	    sum += obsInf_grm(estimateTheta(prior,item,(int)i), item, (int)i) * (probabilities.at(i) - probabilities.at(i-1));
    }

	return sum;
}

double Estimator::expectedObsInf_gpcm(int item, Prior &prior)
{
	std::vector<double> probabilities = prob_gpcm(estimateTheta(prior), (size_t) item);
	double sum = 0.0;
	
	for (size_t i = 0; i < probabilities.size(); ++i) {
	      sum += obsInf_gpcm(estimateTheta(prior,item,(int) i + 1), item, (int) i + 1) * probabilities.at(i);
	}

	return sum;
}

double Estimator::expectedObsInf_rest(int item, Prior &prior)
{
	double prob_one = prob_ltm(estimateTheta(prior), (size_t) item);
	double obsInfZero = obsInf_ltm(estimateTheta(prior, item, 0), item, 0);
	double obsInfOne = obsInf_ltm(estimateTheta(prior, item, 1), item, 1);
	return (prob_one * obsInfOne) + ((1 - prob_one) * obsInfZero);
}

double Estimator::brentMethod(integrableFunction function){//const &function) {
  int status;
  int iter = 0;
  int max_iter = 100;
  
  const gsl_root_fsolver_type *T;
  gsl_root_fsolver *s;
  
  double r = 0;
  double x_lo = -5.0;
  double x_hi = 5.0;
  
  auto gslfunc = GSLFunctionWrapper(function);
  gsl_function *F = gslfunc.asGSLFunction();
  
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

double Estimator::fisherTestInfo(Prior prior, size_t question, int answer)
{
	double theta = estimateTheta(prior,question,answer);
	double sum = 0.0;
	for (auto item : questionSet.applicable_rows)
	{
		int a = questionSet.answers.at(item);
		sum += fisherInf(theta, item, a);
	}
	sum += fisherInf(theta, question, answer);
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

	return integrate_selectItem(pwi_j, questionSet.lowerBound, questionSet.upperBound);
}

double Estimator::lwi(int item) {

	integrableFunction lwi_j = [&](double theta) {
		return likelihood(theta) * fisherInf(theta, item);
	};

	return integrate_selectItem(lwi_j, questionSet.lowerBound, questionSet.upperBound);
}

double Estimator::fii(int item, Prior prior) {
  
	integrableFunction fii_j = [&](double theta_not) {
		return fisherInf(theta_not, item);
	};
	  
  double delta = questionSet.z.at(0) * std::pow(fisherTestInfo(prior), 0.5);
  
  const double lower = estimateTheta(prior) - delta;
  const double upper = estimateTheta(prior) + delta;

	return integrate_selectItem(fii_j, lower, upper);
}

double Estimator::kl(double theta_not, int item, double theta){
  	double sum = 0.0;
  
  	if(questionSet.model == "grm"){
    	auto cdf_theta_not = prob_grm(theta_not, (size_t) item);
	  	auto cdf_theta_hat = prob_grm(theta, (size_t) item);
	  
	  	for (size_t i = 1; i < cdf_theta_hat.size(); ++i) {
	    	double prob_theta_not = cdf_theta_not.at(i) - cdf_theta_not.at(i-1);
	    	double prob_theta_hat = cdf_theta_hat.at(i) - cdf_theta_hat.at(i-1);
	    	sum += prob_theta_not * (log(prob_theta_not) - log(prob_theta_hat));
	  	}
	}
  
  	if(questionSet.model == "gpcm"){
    	auto prob_theta_not = prob_gpcm(theta_not, (size_t) item);
	  	auto prob_theta_hat = prob_gpcm(theta, (size_t) item);
	  
	  	for (size_t i = 0; i < prob_theta_not.size(); ++i) {
	  	  	sum += prob_theta_not.at(i) * (log(prob_theta_not.at(i)) - log(prob_theta_hat.at(i)));
	  	}
  	}
  
  	if((questionSet.model == "ltm") || (questionSet.model == "tpm")){
    	const double prob_theta_not = prob_ltm(theta_not, (size_t) item);
    	const double prob_theta_hat = prob_ltm(theta, (size_t) item);

    	double first_term = prob_theta_not * (log(prob_theta_not) - log(prob_theta_hat));
    	double second_term = (1 - prob_theta_not) * (log(1 - prob_theta_not) - log(1 - prob_theta_hat));

    	sum = first_term + second_term;
  	}
  
  	return sum;
}

double Estimator::expectedKL(int item, Prior prior) {
	double theta = estimateTheta(prior);
	integrableFunction kl_fctn = [&](double theta_not) {
	  return kl(theta_not, item, theta);
  };
  
  double delta = questionSet.z.at(0) * std::pow(fisherTestInfo(prior), 0.5);
  
  const double lower = estimateTheta(prior) - delta;
  const double upper = estimateTheta(prior) + delta;

  return integrate_selectItem(kl_fctn, lower, upper);
}

double Estimator::likelihoodKL(int item, Prior prior) {
	double theta = estimateTheta(prior);
	integrableFunction kl_fctn = [&](double theta_not) {
	  return likelihood(theta_not) * kl(theta_not, item, theta);
  };

  return integrate_selectItem(kl_fctn, questionSet.lowerBound, questionSet.upperBound);
}

double Estimator::posteriorKL(int item, Prior prior) {
	double theta = estimateTheta(prior);
	integrableFunction kl_fctn = [&](double theta_not) {
	  return prior.prior(theta_not) * likelihood(theta_not) * kl(theta_not, item, theta);
  };

  return integrate_selectItem(kl_fctn, questionSet.lowerBound, questionSet.upperBound);
}

double Estimator::integrate_selectItem(const integrableFunction &function, const double lower, const double upper){
  auto gslfunc = GSLFunctionWrapper(function);
  gsl_function *f = gslfunc.asGSLFunction();
  return integrator.integrate(f, integrationSubintervals, lower, upper);
}




