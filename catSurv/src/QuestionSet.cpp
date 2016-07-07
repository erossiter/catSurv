#include "QuestionSet.h"

QuestionSet::QuestionSet(Rcpp::S4 &cat_df) {
	answers = Rcpp::as<std::vector<int> >(cat_df.slot("answers"));
	guessing = Rcpp::as<std::vector<double> >(cat_df.slot("guessing"));
	discrimination = Rcpp::as<std::vector<double> >(cat_df.slot("discrimination"));
	coverage = Rcpp::as<std::vector<double> >(cat_df.slot("coverage"));
	
	for (size_t i = 0; i < answers.size(); i++) {
		if (answers[i] == NA_INTEGER) {
			nonapplicable_rows.push_back((int) i);
		} else {
			applicable_rows.push_back((int) i);
		}
	}
	
	for (auto item : (Rcpp::List) cat_df.slot("difficulty")) {
		difficulty.push_back(Rcpp::as<std::vector<double> >(item));
	}

	poly = Rcpp::as<std::vector<bool> >(cat_df.slot("poly"));
	
	
	// Added all this in to check if its appropriate to use MLE
	std::vector<double> minAnswer_posDiscrim;
	std::vector<double> maxAnswer_posDiscrim;
	std::vector<double> minAnswer_negDiscrim;
	std::vector<double> maxAnswer_negDiscrim;
	std::vector<double> ans_not_extreme;
	
	int max_response;
	int min_response;
	poly[0] ? max_response = difficulty[1].size() + 1.0 : max_response = 1.0;
	poly[0] ? min_response = 1.0 : min_response = 0.0;

	for (auto i : applicable_rows) {
	  if (discrimination[i] < 0.0 and answers[i] == min_response) minAnswer_negDiscrim.push_back((int) i);
	  else if (discrimination[i] < 0.0 and answers[i] == max_response) maxAnswer_negDiscrim.push_back((int) i);
	  else if (discrimination[i] > 0.0 and answers[i] == min_response) minAnswer_posDiscrim.push_back((int) i);
	  else if (discrimination[i] > 0.0 and answers[i] == max_response) maxAnswer_posDiscrim.push_back((int) i);
	  else ans_not_extreme.push_back((int) i);
	}

	if (minAnswer_posDiscrim.size() != 0 and maxAnswer_negDiscrim.size() != 0 and minAnswer_negDiscrim.size() == 0 and maxAnswer_posDiscrim.size() == 0 and ans_not_extreme.size() == 0){
	  all_extreme = true;
	} else if (minAnswer_posDiscrim.size() == 0 and maxAnswer_negDiscrim.size() == 0 and minAnswer_negDiscrim.size() != 0 and maxAnswer_posDiscrim.size() != 0 and ans_not_extreme.size() == 0){
	  all_extreme = true;
	} else {
	  all_extreme = false;
	}
}
