#include "Rcpp.h"
#include "Cat.h"

using namespace Rcpp;

Cat::Cat(std::vector<double> guess, std::vector<double> disc, std::vector<double> pri_v, std::string pri_n,
         std::vector<double> pri_p, std::vector<int> ans, double d, std::vector<double> x,
         std::vector<double> t_est, std::vector<std::vector<double> > difficulty,
         std::vector<int> app_rows, std::vector<int> nonapp_rows, bool p, std::string im, std::string em,
         std::string sm, double cov, int pts)
		: guessing(guess), discrimination(disc), prior_values(pri_v), prior_params(pri_p),
		  answers(ans), D(d), X(x), theta_est(t_est), difficulty(difficulty),
		  applicable_rows(app_rows), nonapplicable_rows(nonapp_rows), poly(p), coverage(cov), points(pts) {
	if (im == "trapezoid") {
		integration_method = TRAPEZOID;
	}

	if (em == "EAP") {
		estimation_method = EAP;
	}
	else if (em == "MAP") {
		estimation_method = MAP;
	}

	if (sm == "EPV") {
		selection_method = EPV;
	}
	else if (sm == "MFI") {
		selection_method = MFI;
	}
	else if (sm == "LWI") {
		selection_method = LWI;
	}
	else if (sm == "PWI") {
		selection_method = PWI;
	}
	else if (sm == "MEI") {
		selection_method = MEI;
	}
	else {
		selection_method = invalid;
	}

	if (pri_n == "normal") {
		prior_name = NORMAL;
	}
}

Cat::Cat(S4 cat_df) {
	std::string priorName = Rcpp::as<std::string>(cat_df.slot("priorName"));
	std::vector<double> priorParams = Rcpp::as<std::vector<double> >(cat_df.slot("priorParams"));
	std::vector<double> prior_values;

	//TODO: Re-enable
	//for(unsigned int i = 0; i < X.size(); ++i){
	//prior_values.push_back(prior(X[i], priorName, priorParams));
	//}

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
	this->poly = Rcpp::as<std::vector<bool> >(cat_df.slot("poly"))[0];;

	//TODO: Convert all these strings to their enums
	//this->prior_name
	//this->integration_method
	//this->estimation
	//this->selection
}

std::string Cat::priorEnumToString() {
	return prior_name == NORMAL ? "normal" : "student_t";
}

question_data Cat::get_question(int question) {
	return (question_data) {.difficulty = difficulty[question],
							.D = D,
							.discrimination = discrimination[question],
							.guessing = guessing[question]};
}

