#include "Rcpp.h"
#include "Cat.h"

using namespace Rcpp;


Cat::Cat(S4 cat_df) {

	std::vector<double> prior_values;
	prior = (Prior) {.name = Rcpp::as<std::string>(cat_df.slot("priorName")),
			.parameters = Rcpp::as<std::vector<double> >(cat_df.slot("priorParams"))};

	for (auto i : X) {
		prior.values.push_back(prior.prior(i, prior.name, prior.parameters);
	}



	// Precalculate the rows that have been answered.s
	std::vector<int> applicable_rows;
	std::vector<int> nonapplicable_rows;
	std::vector<int> answers = Rcpp::as<std::vector<int> >(cat_df.slot("answers"));

	for (unsigned int i = 0; i < answers.size(); i++) {
		if (answers[i] != NA_INTEGER) {
			applicable_rows.push_back(i);
		} else {
			nonapplicable_rows.push_back(i + 1);
		}
	}

	std::vector<std::vector<double> > difficulty;

	// Unpack the difficulty list
	List cat_difficulty = cat_df.slot("difficulty");
	for (auto item : cat_difficulty) {
		difficulty.push_back(Rcpp::as<std::vector<double> >(item));
	}

	//TODO: Convert to initializer list
	this->guessing = Rcpp::as<std::vector<double> >(cat_df.slot("guessing"));
	this->discrimination = Rcpp::as<std::vector<double> >(cat_df.slot("discrimination"));
	this->prior_values = prior_values;
	this->prior_params = priorParams;
	this->answers = answers;
	this->D = Rcpp::as<std::vector<double> >(cat_df.slot("D"))[0];
	this->X = Rcpp::as<std::vector<double> >(cat_df.slot("X"));
	this->theta_est = Rcpp::as<std::vector<double> >(cat_df.slot("Theta.est"));
	this->difficulty = difficulty;
	this->applicable_rows = applicable_rows;
	this->nonapplicable_rows = nonapplicable_rows;
	this->coverage = Rcpp::as<std::vector<double> >(cat_df.slot("coverage"))[0];
	this->points = Rcpp::as<std::vector<int> >(cat_df.slot("points"))[0];
	this->poly = Rcpp::as<std::vector<bool> >(cat_df.slot("poly"))[0];
}

double Cat::likelihood(double theta, std::vector<int> items) {
	double L = 1.0;
	if (poly) {
		for (auto question : items) {
			double prob = probability(theta, question)[0];
			int this_answer = answers[question];
			L *= pow(prob, this_answer) * pow(1 - prob, 1 - this_answer);
		}
		return L;
	}
	for (auto question : items) {

		auto probabilities = probability(theta, question);

		std::vector<double> question_cdf;

		question_cdf.push_back(1.0);
		question_cdf.insert(question_cdf.end(), probabilities.begin(), probabilities.end());
		question_cdf.push_back(0.0);

		std::vector<double> question_pdf;
		for (unsigned int j = 0; j < question_cdf.size() - 1; ++j) {
			question_pdf.push_back(question_cdf[j] - question_cdf[j + 1]);
		}

		L *= question_pdf[answers[question] - 1];
	}
	return L;
	//	} else {
	//		double L = 1.0;
	//		for(unsigned int i = 0; i < items.size(); ++i){
	//			int question = items[i];
	//			double prob = probability(cat, theta, question);
	//			int this_answer = cat.answers[question];
	//			double l_temp = pow(prob, this_answer) * pow(1-prob, 1-this_answer);
	//			L *= l_temp;
	//		}
	//		return L;
	//	}
}

std::vector<double> Cat::probability(double theta, int question_number) {
	std::vector<double> probabilities;
	auto question = get_question(question_number);

	double initial_probability = exp(D * questionSet.discrimination[question_number]);
	double guessing = questionSet.guessing[question_number];

	auto calculate = [&](auto difficulty) {
		double exp_prob = initial_probability * exp(theta - difficulty);
		double base_probability = (exp_prob) / (1 + exp_prob);
		return poly ? guessing + (1 - guessing) * base_probability : base_probability;
	};

	for (auto term : question.difficulty) {
		probabilities.push_back(calculate(term));
	}

	return probabilities;
}

Cat::Cat(QuestionSet questionSet, Integrator integrator, Estimator estimator, Prior prior)
		: estimator(estimator), questionSet(questionSet), integrator(integrator), prior(prior) {
	this->poly = questionSet.difficulty[0].size() > 1;
}

