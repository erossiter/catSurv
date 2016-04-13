#include <Rcpp.h>
#include "Cat.h"
#include <boost/variant.hpp>

using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

//' Probabilities of the responses to a question given theta 
//' 
//' This function calculates the probabilities of a specific set of responses to a specific question for a specific value of \eqn{\theta}.
//' 
//' @param cat_df An object of \code{Cat} class
//' @param t A double indicating the potential value for \eqn{\theta_{j}}{\theta_j}
//' @param q An integer indicating the index of the question
//' @param ret_prob (For polytonomous implementation only) A double-vector where the calculations carried out by this function will be stored.
//' 
//' @return A vector consisting of the probability of a correct response for each respondent on item \eqn{i}.
//' 
//' @details The probability of a correct response for respondent \eqn{j} on item \eqn{i} is ....
//' where \eqn{\theta_{j}}{\theta_j} is respondent \eqn{j}'s position on the latent scale of interest, \eqn{a_{i}}{a_i} is item \eqn{i}'s discrimination parameter,
//'  \eqn{b_{i}}{b_i} is item i's difficulty parameter, and \eqn{c_{i}}{c_i} is item \eqn{i}'s guessing parameter.
//'  
//'  Note: this function is overloaded, due to different output types of binary vs polytomous implementations (outputs single value for binary implementation,
//'  vector of values for polytomous implementation)
//'  
//'  Note: the function for polytomous implementation does not return values, but rather alters the object ret_prob in memory
//'  
//' @export
// [[Rcpp::export]]
List probability(S4 cat_df, NumericVector t, IntegerVector q) {
	Cat cat = Cat(cat_df);
	double theta = t[0];
	int question = q[0];

	DataFrame question_probs = DataFrame::create(Named("probabilities") = cat.probability(theta, question));
	return List::create(Named("all.probabilities") = question_probs);
}

//' Likelihood of offering specific response
//' 
//' This function returns the value of likelihood of a person with ability parameter \eqn{\theta} having offered the specific response profile stored in answers conditional on the item-level parameters. 
//' 
//' @param cat_df An object of \code{Cat} class
//' @param t A numeric for possible value of theta (position on the latent scale of interest)
//' 
//' @return A value of the likelihood of each respondent having offered the provided response profile
//' 
//' @details Letting \eqn{q_{i}(\theta_{j})=1-p_{i}(\theta_{j})}{q_i(\theta_j)=1-p_i(\theta_j)}, the likelihood function associated with the responses profile \eqn{y_{j}}{y_j} is..
//' \eqn{L(\theta_{j}|y_{j})=\prod^{j}_{i=1}p_{i}(\theta_{j})^{y_{ij}}q_{i}(\theta_{j})^{(1-y_{ij}}}{L(\theta_j|y_j)=\prod^{j}_{i=1}p_i(\theta_j)^{y_{ij}}q_i(\theta_j)^{(1-y_{ij}}}, where \eqn{y_{j}}{y_j} is evaluated based only on the questions the respondent has actually had the opportunity to answer
//'  
//' @export
// [[Rcpp::export]]
double likelihood(S4 cat_df, NumericVector t) {
	Cat cat = Cat(cat_df);
	return cat.likelihood(t[0], cat.questionSet.applicable_rows);
}

//' Estimate of the ability parameter
//' 
//' This function takes a Cat object and returns the expected value of the ability parameter conditioned on the observed answers and the item calibrations.
//' 
//' @param cat An object of \code{Cat} class
//' 
//' @return A vector consisting of the expected value of the ability parameter
//' 
//' @details The expected value of \eqn{\theta_{j}}{\theta_j} is:
//' \eqn{\frac{\int_{-\infty}^{\infty} \theta_{j} \eqn{L}(\theta_j) \pi(\theta_j) d\theta_j}{\int_{-\infty}^\infty \eqn{L}(\theta_j) \pi(\theta_j) d\theta_j}}{\frac{\int_{-\infty}^\infty \theta_j \eqn{L}(\theta_j) \pi(\theta_j) d\theta_j}{\int_{-\infty}^\infty \eqn{L}(\theta_j) \pi(\theta_j) d\theta_j}},
//' where \eqn{L(\theta_{j})}{L(\theta_j)} is the likelihood function and \eqn{\pi(\theta_{j})}{\pi(\theta_j)} is the prior distribution for \eqn{\theta_{j}}{\theta_j}
//'   
//'  
//'  Note: For the moment, this function implements only the expected a posteriori (EAP) approach
//'  
//'  Note: This method will only be available using the normal prior distribution
//'  
//' @export
// [[Rcpp::export]]
double estimateTheta(S4 cat_df) {
	return Cat(cat_df).estimateTheta();
}

//' Estimate of standard error for the posterior estimate
//' 
//' This function estimates the standard error for the posterior estimate for the position \eqn{\theta_{j}}{\theta_j} of a person on the latent scale
//' 
//' @param cat An object of \code{Cat} class
//' 
//' @return The posterior standard error for \eqn{\theta_{j}}{\theta_j}
//' 
//' @details The posterior variance for \eqn{\theta_j} is:
//' \eqn{\hat{\theta}_{j} = E[(\theta_{j}-\hat{\theta_{j}})^{2}]=\frac{\int(\theta_{j}-\hat{\theta_{j}})^{2}\pi(\theta_{j})L(\theta_{j})d\theta_{j}}\int\pi(\theta_{j})L(\theta_{j})d\theta_{j}}}{\hat{\theta}_j = E[(\theta_j-\hat{\theta_j})^2]=\frac{\int(\theta_j-\hat{\theta_j})^2\pi(\theta_j)L(\theta_j)d\theta_j}\int\pi(\theta_j)L(\theta_j)d\theta_j}},
//' where \eqn{\hat{\theta}_{j}}{\hat{\theta}_j} is the chosen point estimate for the respondent on the latent scale. The standard error is then \eqn{\sqrt{\text{Var}(\hat{\theta}_{j})}\sqrt{\text{Var}(\hat{\theta}_j)}}
//'   
//'  
//' @export
// [[Rcpp::export]]
double estimateSE(S4 cat_df) {
	return Cat(cat_df).estimateSE();
}


//' The possible prior distribution functions
//' 
//' This function returns the prior value for each respondent's position on the latent scale of interest
//' 
//' @param x A numeric value where we want to evaluate the prior name
//' 
//' @return A vector consisting of prior value, \eqn{\pi(x)}, given the value \eqn{x}
//' 
//' @details \eqn{x} needs to be either NORMAL or STUDENT_T params, which are parameters controlling the shape of the prior
//'  
//' @export
// [[Rcpp::export]]
double prior(NumericVector x, CharacterVector c, NumericVector p) {
	std::string name = Rcpp::as<std::string>(c);
	std::vector<double> params = Rcpp::as<std::vector<double> >(p);
	return Prior(name, params).prior(x[0]);
}



//
//// [[Rcpp::export]]
//List nextItem(S4 cat_df) {
//	Cat cat = Cat(cat_df);
//	switch (cat.selection_method) {
//		case Cat::EPV:
//			return nextItemEPVcpp(cat);
//		case Cat::MFI:
//			return nextItemMFI(cat);
//		case Cat::LWI:
//			return nextItemMLWI(cat);
//		case Cat::PWI:
//			return nextItemMPWI(cat);
//		case Cat::MEI:
//			return nextItemMEI(cat);
//		default:
//			throw -1;
//	}
//}
//
//// [[Rcpp::export]]
//List lookAheadEPVcpp(S4 cat_df, NumericVector item) {
//	int look_ahead_item = as<int>(item) - 1;
//	/*NumericVector X = cat_df.slot("X");
//	CharacterVector priorName = cat_df.slot("priorName");
//	NumericVector priorParams = cat_df.slot("priorParams");
//	std::vector<double> prior_values = as<std::vector<double> >(prior(X, priorName, priorParams));
//	*/
//	std::vector<double> X = as<std::vector<double> >(cat_df.slot("X"));
//	std::string priorName = as<std::string>(cat_df.slot("priorName"));
//	std::vector<double> priorParams = as<std::vector<double> >(cat_df.slot("priorParams"));
//	std::vector<double> prior_values;
//
//	for (unsigned int i = 0; i < X.size(); ++i) {
//		prior_values.push_back(prior(X[i], priorName, priorParams));
//	}
//
//	// Precalculate the rows that have been answered.
//	std::vector<int> applicable_rows;
//	std::vector<int> nonapplicable_rows;
//	std::vector<int> answers = as<std::vector<int> >(cat_df.slot("answers"));
//	for (int i = 0; i < answers.size(); i++) {
//		if (i == look_ahead_item) {
//			applicable_rows.push_back(i);
//		} else if (answers[i] != NA_INTEGER) {
//			applicable_rows.push_back(i);
//		} else {
//			nonapplicable_rows.push_back(i + 1);
//		}
//	}
//
//	bool poly = as<std::vector<bool> >(cat_df.slot("poly"))[0];
//	std::vector<std::vector<double> > poly_difficulty; // if poly, construct obj with vector<vector<double>> for difficulty
//	std::vector<double> nonpoly_difficulty;
//	if (poly) {
//		// Unpack the difficulty list
//		List cat_difficulty = cat_df.slot("difficulty");
//		for (List::iterator itr = cat_difficulty.begin(); itr != cat_difficulty.end(); ++itr) {
//			poly_difficulty.push_back(
//					as<std::vector<double> >(*itr)); // if poly, set poly_difficulty to vector<vector<double>
//		}
//	}
//	else {
//		// if non-poly, set non_poly difficulty to vector<double>
//		nonpoly_difficulty = as<std::vector<double> >(cat_df.slot("difficulty"));
//	}
//
//
//	//Construct C++ Cat object
//	Cat cat(as<std::vector<double> >(cat_df.slot("guessing")), as<std::vector<double> >(cat_df.slot("discrimination")),
//	        prior_values, priorName, priorParams, as<std::vector<int> >(cat_df.slot("answers")),
//	        as<std::vector<double> >(cat_df.slot("D"))[0], as<std::vector<double> >(cat_df.slot("X")),
//	        as<std::vector<double> >(cat_df.slot("Theta.est")), poly_difficulty, nonpoly_difficulty, applicable_rows,
//	        nonapplicable_rows, poly, as<std::string>(cat_df.slot("integration")),
//	        as<std::string>(cat_df.slot("estimation")),
//	        as<std::string>(cat_df.slot("selection")), as<std::vector<double> >(cat_df.slot("coverage"))[0],
//	        as<std::vector<int> >(cat_df.slot("points"))[0]);
//
//	if (look_ahead_item >= cat.answers.size()) {
//		stop("Item out of bounds");
//	} else if (!(cat.answers[look_ahead_item] == NA_INTEGER)) {
//		stop("Item already answered");
//	}
//	std::vector<DataFrame> all_epvs;
//	std::vector<double> min_items;
//	for (unsigned int answer = 1, size = cat.poly_difficulty[look_ahead_item].size() + 2; answer < size; ++answer) {
//		cat.answers[look_ahead_item] = answer;
//		std::vector<double> epvs;
//		int min_item = -1;
//		double min_epv = std::numeric_limits<double>::max();
//		for (unsigned int i = 0, size = nonapplicable_rows.size(); i < size; ++i) {
//			epvs.push_back(expectedPV(cat, nonapplicable_rows[i] - 1));
//			if (epvs[i] < min_epv) {
//				min_item = nonapplicable_rows[i];
//				min_epv = epvs[i];
//			}
//		}
//		all_epvs.push_back(DataFrame::create(Named("questions") = nonapplicable_rows, Named("epvs") = epvs));
//		min_items.push_back(min_item);
//	}
//	return List::create(Named("all.epvs") = all_epvs, Named("next.items") = min_items);
//}

//
//List nextItemMFI(Cat &cat) {
//	std::vector<double> mfis;
//	double max_mfi = 0.0;
//	int max_item = -1;
//
//	double theta = estimateTheta(cat);
//	for (unsigned int i = 0; i < cat.nonapplicable_rows.size(); ++i) {
//		mfis.push_back(fisherInf(cat, cat.nonapplicable_rows[i] - 1, theta));
//		if (mfis[i] > max_mfi) {
//			max_item = cat.nonapplicable_rows[i];
//			max_mfi = mfis[i];
//		}
//	}
//	DataFrame all_estimates = DataFrame::create(Named("questions") = cat.nonapplicable_rows, Named("MFI") = mfis);
//	NumericVector next_item = wrap(max_item);
//	return List::create(Named("all.estimates") = all_estimates, Named("next.item") = next_item);
//}
//
//List nextItemMLWI(Cat &cat) {
//	std::vector<double> LWI;
//	std::vector<double> Like_X;
//	double max_LWI = 0.0;
//	int max_item = -1;
//
//	for (unsigned int i = 0; i < cat.X.size(); ++i) {
//		Like_X.push_back(likelihood(cat, cat.X[i], cat.applicable_rows));
//	}
//	for (unsigned int i = 0; i < cat.nonapplicable_rows.size(); ++i) {
//		int item = cat.nonapplicable_rows[i];
//		std::vector<double> Like_FI;
//
//		for (unsigned int j = 0; j < cat.X.size(); ++j) {
//			Like_FI.push_back(Like_X[j] * fisherInf(cat, item - 1, cat.X[j]));
//		}
//		double this_LWI = trapezoidal_integration(cat.X, Like_FI);
//		if (this_LWI > max_LWI) {
//			max_LWI = this_LWI;
//			max_item = item;
//		}
//		LWI.push_back(this_LWI);
//	}
//
//	DataFrame all_estimates = DataFrame::create(Named("questions") = cat.nonapplicable_rows, Named("LWI") = LWI);
//	NumericVector next_item = wrap(max_item);
//	return List::create(Named("all.estimates") = all_estimates, Named("next.item") = next_item);
//}
//
//List nextItemMPWI(Cat &cat) {
//	std::vector<double> PWI, Like_X, Prior_X;
//	double max_PWI = 0.0;
//	int max_item = -1;
//
//	Rcpp::Rcout << "before" << std::endl;
//	for (unsigned int i = 0; i < cat.X.size(); ++i) {
//		Like_X.push_back(likelihood(cat, cat.X[i], cat.applicable_rows));
//		Prior_X.push_back(prior(cat.X[i], cat.priorEnumToString(), cat.prior_params));
//	}
//	Rcpp::Rcout << "after" << std::endl;
//
//	for (unsigned int i = 0; i < cat.nonapplicable_rows.size(); ++i) {
//		int item = cat.nonapplicable_rows[i];
//		std::vector<double> Prior_Like_FI;
//
//		for (unsigned int j = 0; j < cat.X.size(); ++j) {
//			Prior_Like_FI.push_back(Prior_X[j] * Like_X[j] * fisherInf(cat, item - 1, cat.X[j]));
//		}
//
//		double this_PWI = trapezoidal_integration(cat.X, Prior_Like_FI);
//		if (this_PWI > max_PWI) {
//			max_PWI = this_PWI;
//			max_item = item;
//		}
//		PWI.push_back(this_PWI);
//	}
//
//	DataFrame all_estimates = DataFrame::create(Named("questions") = cat.nonapplicable_rows, Named("PWI") = PWI);
//	NumericVector next_item = wrap(max_item);
//	return List::create(Named("all.estimates") = all_estimates, Named("next.item") = next_item);
//}
//
//List nextItemMEI(Cat &cat) {
//	std::vector<double> EI;
//	double max_EI = 0.0;
//	int max_item = -1;
//	for (unsigned int i = 0; i < cat.nonapplicable_rows.size(); ++i) {
//		int item = cat.nonapplicable_rows[i];
//		double this_EI = expectedObsInf(cat, item - 1);
//		if (this_EI > max_EI) {
//			max_EI = this_EI;
//			max_item = item;
//		}
//		EI.push_back(this_EI);
//	}
//
//	DataFrame all_estimates = DataFrame::create(Named("questions") = cat.nonapplicable_rows, Named("EI") = EI);
//	NumericVector next_item = wrap(max_item);
//	return List::create(Named("all.estimates") = all_estimates, Named("next.item") = next_item);
//
//}
//
//double expectedObsInf(Cat & cat, int item){
//	double sum = 0.0;
//	cat.applicable_rows.push_back(item);
//	if(cat.poly){
//		std::vector<double> obsInfs;
//		for(unsigned int i = 0; i <= cat.poly_difficulty[item].size(); ++i){
//			cat.answers[item] = i + 1;
//			obsInfs.push_back(obsInf(cat, item, estimateTheta(cat)));
//		}
//		cat.answers[item] = NA_INTEGER;
//		cat.applicable_rows.pop_back();
//
//		std::vector<double> question_cdf;
//		question_cdf.push_back(1.0);
//		probability(cat, estimateTheta(cat), item, question_cdf);
//		question_cdf.push_back(0.0);
//		for(unsigned int i = 0; i < question_cdf.size() -1; ++i){
//			sum += obsInfs[i] * (question_cdf[i] - question_cdf[i+1]);
//		}
//	}
//	else{
//		cat.answers[item] = 0;
//		double obsInfZero = obsInf(cat, item, estimateTheta(cat));
//		cat.answers[item] = 1;
//		double obsInfOne = obsInf(cat, item, estimateTheta(cat));
//		cat.applicable_rows.pop_back();
//		cat.answers[item] = NA_INTEGER;
//		double prob_one = probability(cat, estimateTheta(cat), item);
//		sum = ((1.0 - prob_one) * obsInfZero) + (prob_one + obsInfOne);
//	}
//	return sum;
//}

//double obsInf(Cat & cat, int item, double theta){
//	if(cat.applicable_rows.size() == 0){
//		Rcpp::Rcout << "ObsInf should not be called if no items have been answered." << std::endl;
//		throw -1;
//	}
//	double output = 0.0;
//	if(cat.poly){
//		int index_k = cat.answers[item];
//		std::vector<double> probs;
//		probs.push_back(1.0);
//		probability(cat, theta, item, probs);
//		probs.push_back(0.0);
//		double P_star1 = probs[index_k];
//		double Q_star1 = 1.0 - P_star1;
//		double P_star2 = probs[index_k-1];
//		double Q_star2 = 1.0 - P_star2;
//		double P = P_star2 - P_star1;
//		double w2 = P_star2 * Q_star2;
//		double w1 = P_star1 * Q_star1;
//		output = -(cat.discrimination[item] * cat.discrimination[item]) * (((-w1 * (Q_star1 - P_star1)
//		                                                                     + w2 * (Q_star2 - P_star2)) / P) - (((w2 - w1) * (w2 - w1)) / (P*P)));
//	}
//	else{
//		double P = probability(cat, theta, item);
//		double Q = 1.0 - P;
//		double temp = ((P - cat.guessing[item]) / (1.0 - cat.guessing[item]));
//		temp *= temp;
//		output = (cat.discrimination[item] * cat.discrimination[item]) * temp * (Q / P);
//	}
//	return output;
//}
//
//double fisherInf(Cat & cat, int item, double theta){
//	double output = 0.0;
//	if(cat.poly){
//		std::vector<double> probs;
//		probs.push_back(1.0);
//		probability(cat, theta, item, probs);
//		probs.push_back(0.0);
//		for(unsigned int i = 1; i <= cat.poly_difficulty[item].size(); ++i){
//			double P_star1 = probs[i];
//			double Q_star1 = 1.0 - P_star1;
//			double P_star2 = probs[i-1];
//			double Q_star2 = 1.0 - P_star2;
//			double w2 = P_star2 * Q_star2;
//			double w1 = P_star1 * Q_star1;
//			output += (cat.discrimination[item] * cat.discrimination[item]) *(((w2 - w1) * (w2 - w1))
//			                                                                  / (P_star2- P_star1));
//		}
//	}
//	else{
//		double P = probability(cat, theta, item);
//		double Q = 1.0 - P;
//		double temp = (P - cat.guessing[item]) / (1.0 - cat.guessing[item]);
//		temp *= temp;
//		output = (cat.discrimination[item] * cat.discrimination[item]) * temp * (Q / P);
//	}
//	return output;
//}
//
//double dLL(Cat & cat, double theta, bool use_prior){
//	if(cat.applicable_rows.size() == 0){
//		return ((theta - cat.prior_params[0]) / (cat.prior_params[1] * cat.prior_params[1]));
//	}
//
//	double L_theta = 0.0;
//	if(cat.poly){
//		for(unsigned int i = 0; i < cat.applicable_rows.size(); ++i){
//			int item = cat.applicable_rows[i];
//			int answer_k = cat.answers[item];
//			std::vector<double> probs;
//			probs.push_back(1.0);
//			probability(cat, theta, item, probs);
//			probs.push_back(0.0);
//			double P_star1 = probs[answer_k];
//			double Q_star1 = 1.0 - P_star1;
//			double P_star2 = probs[answer_k-1];
//			double Q_star2 = 1 - P_star2;
//			double P = P_star2 - P_star1;
//			double w2 = P_star2 * Q_star2;
//			double w1 = P_star1 * Q_star1;
//			L_theta = L_theta + (cat.discrimination[item] * ((w2 - w1) / P));
//		}
//	}
//	else{
//		for(unsigned int i = 0; i < cat.applicable_rows.size(); ++i){
//			int item = cat.applicable_rows[i];
//			double P = probability(cat, theta, item);
//			L_theta = L_theta + cat.discrimination[item] * ((P - cat.guessing[item]) / (P * (1.0 - cat.guessing[item])))
//			                    * (cat.answers[item] - P);
//		}
//	}
//	if(use_prior){
//		L_theta -= ((theta - cat.prior_params[0]) / (cat.prior_params[1] * cat.prior_params[1]));
//	}
//	return L_theta;
//}
//
//double d2LL(Cat & cat, double theta, bool use_prior){
//	if(cat.applicable_rows.size() == 0){
//		return -1.0 / (cat.prior_params[1] * cat.prior_params[1]);
//	}
//
//	double Lambda_theta = 0.0;
//	if(cat.poly){
//		for(unsigned int i = 0; i < cat.applicable_rows.size(); ++i){
//			int item = cat.applicable_rows[i];
//			int answer_k = cat.answers[item];
//			std::vector<double> probs;
//			probs.push_back(1.0);
//			probability(cat, theta, item, probs);
//			probs.push_back(0.0);
//			double P_star1 = probs[answer_k];
//			double Q_star1 = 1.0 - P_star1;
//			double P_star2 = probs[answer_k-1];
//			double Q_star2 = 1 - P_star2;
//			double P = P_star2 - P_star1;
//			double w2 = P_star2 * Q_star2;
//			double w1 = P_star1 * Q_star1;
//			Lambda_theta = Lambda_theta + (cat.discrimination[item] * cat.discrimination[item]) * (((-w1 * (Q_star1 - P_star1)
//			                                                                                         + w2 * (Q_star2 - P_star2)) / P) - (((w2 - w1) * (w2 - w1)) / (P*P)));
//		}
//	}
//	else{
//		for(unsigned int i = 0; i < cat.applicable_rows.size(); ++i){
//			int item = cat.applicable_rows[i];
//			double P = probability(cat, theta, item);
//			double Q = 1.0 - P;
//			double Lambda_temp = (P - cat.guessing[item]) / (1.0 - cat.guessing[item]);
//			Lambda_temp *= Lambda_temp;
//			Lambda_theta = Lambda_theta - (cat.discrimination[item] * cat.discrimination[item]) * Lambda_temp * (Q / P);
//		}
//	}
//	if(use_prior){
//		Lambda_theta -= (1.0 / (cat.prior_params[1] * cat.prior_params[1]));
//	}
//	return Lambda_theta;
//}
// TODO
//// [[Rcpp::export]]
//double obsInf(S4 cat_df, int item, double theta){
//	//Cat cat = Cat(cat_df);
//	//return obsInf(cat, item, theta);
//	return 0;
//}
//
//// TODO
//// [[Rcpp::export]]
//double fisherInf(S4 cat_df, int item, double theta){
////	Cat cat = Cat(cat_df);
////	return fisherInf(cat, item, theta);
//	return 0;
//}

// TODO
// [[Rcpp:::export]]
//double dLL(S4 cat_df, NumericVector t, LogicalVector use_p){
//	Cat cat = Cat(cat_df);
//	double theta = as<std::vector<double> >(t)[0];
//	bool use_prior = as<std::vector<bool> >(use_p)[0];
//	return dLL(cat, theta, use_p);
//}
//
//// TODO
//// [[Rcpp::export]]
//double d2LL(S4 cat_df, NumericVector t, LogicalVector use_p){
////	Cat cat = Cat(cat_df);
////	double theta = t[0];
////	bool use_prior = (bool) use_p[0];
////	return d2LL(cat, theta, use_p);
//	return 0;
//}


// TODO
//// [[Rcpp:export]]
//double integrate(NumericVector x, NumericVector fx){
//	std::vector<double> x_ = as<std::vector<double> >(x);
//	std::vector<double> fx_ = as<std::vector<double> >(fx);
//	return trapezoidal_integration(x_, fx_);
//}

//// [[Rcpp:export]]
//double dnorm(NumericVector x, NumericVector mu, NumericVector sigma){
//	return dnorm(as<std::vector<double> >(x)[0], as<std::vector<double> >(mu)[0],
//	             as<std::vector<double> >(sigma)[0]);
//}
//
//// [[Rcpp:export]]
//double dchi(NumericVector x, NumericVector k){
//	return dchi(as<std::vector<double> >(x)[0], as<std::vector<double> >(k)[0]);
//}
//
//// [[Rcpp::export]]
//double dt(NumericVector x, NumericVector mu, IntegerVector df){
//	return dt(as<std::vector<double> >(x)[0], as<std::vector<double> >(mu)[0],
//	          as<std::vector<int> >(df)[0]);
//}