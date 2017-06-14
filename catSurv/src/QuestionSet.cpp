#include "QuestionSet.h"

QuestionSet::QuestionSet(Rcpp::S4 &cat_df) {
	answers = Rcpp::as<std::vector<int> >(cat_df.slot("answers"));
	guessing = Rcpp::as<std::vector<double> >(cat_df.slot("guessing"));
	discrimination = Rcpp::as<std::vector<double> >(cat_df.slot("discrimination"));
	z = Rcpp::as<std::vector<double> >(cat_df.slot("z"));
	
	lowerBound = Rcpp::as<double >(cat_df.slot("lowerBound"));
	upperBound = Rcpp::as<double >(cat_df.slot("upperBound"));
	
	Rcpp::NumericVector discrim_names = cat_df.slot("discrimination");
  Rcpp::CharacterVector names = discrim_names.names();
  question_names = Rcpp::as<std::vector<std::string> >(names);

	for (size_t i = 0; i < answers.size(); i++) {
		if (answers.at(i) == NA_INTEGER) {
			nonapplicable_rows.push_back((int) i);
		} else if (answers.at(i) != -1) {
			applicable_rows.push_back((int) i);
		} else {
		  skipped.push_back((int) i);
		} 
	}
	
	for (auto item : (Rcpp::List) cat_df.slot("difficulty")) {
		difficulty.push_back(Rcpp::as<std::vector<double> >(item));
	}

	model = Rcpp::as<std::string >(cat_df.slot("model"));
	
	// Added all this is to check if its appropriate to use MLE
	std::vector<double> minAnswer_posDiscrim;
	std::vector<double> maxAnswer_posDiscrim;
	std::vector<double> minAnswer_negDiscrim;
	std::vector<double> maxAnswer_negDiscrim;
	std::vector<double> ans_not_extreme;
	

	int max_response = ((model == "ltm") | (model == "tpm")) ? 1.0 : difficulty[1].size() + 1.0;
	int min_response = ((model == "ltm") | (model == "tpm")) ? 0.0 : 1.0;

	for (auto i : applicable_rows) {
	  if (discrimination.at(i) < 0.0 and answers.at(i) == min_response) minAnswer_negDiscrim.push_back((int) i);
	  else if (discrimination.at(i) < 0.0 and answers.at(i) == max_response) maxAnswer_negDiscrim.push_back((int) i);
	  else if (discrimination.at(i) > 0.0 and answers.at(i) == min_response) minAnswer_posDiscrim.push_back((int) i);
	  else if (discrimination.at(i) > 0.0 and answers.at(i) == max_response) maxAnswer_posDiscrim.push_back((int) i);
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
