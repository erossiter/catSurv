#include <Rcpp.h>
#include <RcppGSL.h>
#include <boost/math/special_functions/hermite.hpp>
#include <boost/math/special_functions/factorials.hpp>
#include <gsl/gsl_integration.h>
#include <boost/math/distributions/normal.hpp>
#include <boost/math/distributions/cauchy.hpp>
#include <boost/math/distributions/students_t.hpp>
#include <boost/variant.hpp>

using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(BH)]]

struct Cat {
	std::vector<double> guessing;
	std::vector<double> discrimination;
	std::vector<double> prior_values;
	std::vector<double> prior_params;
	std::vector<int> answers;
	double D;
	// X is the abcissa values for whatever mode of integation
	// in trapezoidal, this doesn't matter that much
	// in hermite-gauss, it should be the roots of the hermite polynomial
	std::vector<double> X;
	std::vector<double> theta_est;
	std::vector<std::vector<double> > poly_difficulty;
	std::vector<double> nonpoly_difficulty;
	std::vector<int> applicable_rows;
	std::vector<int> nonapplicable_rows;
	bool poly;
	enum IntegrationType {
		TRAPEZOID, HERMITE, QAG
	};
	enum EstimationType { 
		EAP, MAP 
	};
	enum SelectionType{
		EPV, MFI, LWI, PWI, MEI, invalid
	};
	enum priorName {
		NORMAL, STUDENT_T
	};
	IntegrationType integration_method;
	EstimationType estimation_method;
	SelectionType selection_method;
	priorName prior_name;
	double coverage;
	int points;

	Cat(std::vector<double> guess, std::vector<double> disc, std::vector<double> pri_v, std::string pri_n,
		std::vector<double> pri_p, std::vector<int> ans, double d, std::vector<double> x, 
		std::vector<double> t_est, std::vector<std::vector<double> > poly_diff, std::vector<double> nonpoly_diff,
		std::vector<int> app_rows, std::vector<int> nonapp_rows, bool p, std::string im, std::string em, 
		std::string sm, double cov, int pts) :
		guessing(guess), discrimination(disc), prior_values(pri_v), prior_params(pri_p),
		answers(ans), D(d), X(x), theta_est(t_est), poly_difficulty(poly_diff), nonpoly_difficulty(nonpoly_diff),
		applicable_rows(app_rows), nonapplicable_rows(nonapp_rows), poly(p), coverage(cov), points(pts)
		{
			if(im == "trapezoid"){
				integration_method = TRAPEZOID;
			}

			if(em == "EAP"){
				estimation_method = EAP;
			}
			else if(em == "MAP"){
				estimation_method = MAP;
			}

			if(sm == "EPV"){
				selection_method = EPV;
			}
			else if(sm == "MFI"){
				selection_method = MFI;
			}
			else if(sm == "LWI"){
				selection_method = LWI;
			}
			else if(sm == "PWI"){
				selection_method = PWI;
			}
			else if(sm == "MEI"){
				selection_method = MEI;
			}
			else{
				selection_method = invalid;
			}

			if(pri_n == "normal"){
				prior_name = NORMAL;
			}
		}

	std::string priorEnumToString(){
		return (prior_name == NORMAL) ? "normal" : "student_t";
	}
};

double trapezoidal_integration(std::vector<double>& x, std::vector<double>& fx) {
	double val = 0;
	for (unsigned int i = 0; i < x.size() - 1; ++i) {
		val += (x[i + 1] - x[i]) * (fx[i + 1] + fx[i]) / 2.0;
	}
	return val;
}

double dnorm(double x, double mu, double sigma){
	return 1.0 / (sigma * std::sqrt(2 * PI)) * exp(-(x - mu) * (x - mu) / (2 * sigma * sigma));
}

int gamma(int n){
	return (n == 1 || n == 0) ? 1 : gamma(n - 2) * (n - 1);
}

double dchi(double x, double k){
	int gamma_fn = gamma(int(k) / 2);
	return (pow(x, (k / 2) - 1) * exp(-x / 2)) / (pow(2, k / 2) * gamma_fn);
}

double dt(double x, double mu, int df){
	double V = dchi(x, df);
	double Z = dnorm(x, 0, 1);
	return (Z + mu) / std::sqrt(V / df);
}

double prior(double x, std::string name, std::vector<double> params){
	if(name == "normal"){
		return dnorm(x, params[0], params[1]);
	}
	else{
		return dt(x, params[0], int(params[1]));
	}
}

void probability(Cat& cat, double theta, int question, std::vector<double>& ret_prob) {
	unsigned int diff_size = cat.poly_difficulty[question].size();
	double D = cat.D;
	double discrimination = cat.discrimination[question];
	double guessing = cat.guessing[question];
	for (unsigned int i = 0; i < diff_size; ++i) {
		double exp_prob = exp(D * discrimination * (theta - cat.poly_difficulty[question][i]));
		ret_prob.push_back(guessing + (1 - guessing) * (exp_prob) / (1 + exp_prob));
	}
}

/* Overloaded since non-poly case needs to just return one double value, 
 * rather than a vector of doubles.
 */
double probability(Cat & cat, double theta, int question){
	double D = cat.D;
	double discrimination = cat.discrimination[question];
	double difficulty = cat.nonpoly_difficulty[question];
	double guessing = cat.guessing[question];
	double exp_prob = exp(D*discrimination * (theta - difficulty));
	return guessing + (1 - guessing) * (exp_prob / (1 + exp_prob));
}

double likelihood(Cat & cat, double theta, std::vector<int> items) {
	if (cat.poly) {
		double L = 1.0;
		for (unsigned int i = 0; i < items.size(); ++i) {
			int question = items[i];
			std::vector<double> question_cdf;
			question_cdf.push_back(1.0);
			probability(cat, theta, question, question_cdf);
			question_cdf.push_back(0.0);

			std::vector<double> question_pdf;
			for (unsigned int j = 0; j < question_cdf.size() - 1; ++j) {
				question_pdf.push_back(question_cdf[j] - question_cdf[j + 1]);
			}
			L *= question_pdf[cat.answers[question] - 1];
		}
		return L;
	} else { 
		double L = 1.0;
		for(unsigned int i = 0; i < items.size(); ++i){
			int question = items[i];
			double prob = probability(cat, theta, question);
			int this_answer = cat.answers[question]; 
			double l_temp = pow(prob, this_answer) * pow(1-prob, 1-this_answer);
			L *= l_temp;
		}
		return L;
	}
}

double obsInf(Cat & cat, int item, double theta){
	if(cat.applicable_rows.size() == 0){
		Rcpp::Rcout << "ObsInf should not be called if no items have been answered." << std::endl;
		throw -1;
	}
	double output = 0.0;
	if(cat.poly){
		int index_k = cat.answers[item];
		std::vector<double> probs;
		probs.push_back(1.0);
		probability(cat, theta, item, probs);
		probs.push_back(0.0);
		double P_star1 = probs[index_k];
		double Q_star1 = 1.0 - P_star1;
		double P_star2 = probs[index_k-1];
		double Q_star2 = 1.0 - P_star2;
		double P = P_star2 - P_star1;
		double w2 = P_star2 * Q_star2;
		double w1 = P_star1 * Q_star1;
		output = -(cat.discrimination[item] * cat.discrimination[item]) * (((-w1 * (Q_star1 - P_star1) 
			+ w2 * (Q_star2 - P_star2)) / P) - (((w2 - w1) * (w2 - w1)) / (P*P)));
	}
	else{
		double P = probability(cat, theta, item);
		double Q = 1.0 - P;
		double temp = ((P - cat.guessing[item]) / (1.0 - cat.guessing[item]));
		temp *= temp;
		output = (cat.discrimination[item] * cat.discrimination[item]) * temp * (Q / P);
	}
	return output;
}

double fisherInf(Cat & cat, int item, double theta){
	double output = 0.0;
	if(cat.poly){
		std::vector<double> probs;
		probs.push_back(1.0);
		probability(cat, theta, item, probs);
		probs.push_back(0.0);
		for(unsigned int i = 1; i <= cat.poly_difficulty[item].size(); ++i){
			double P_star1 = probs[i];
			double Q_star1 = 1.0 - P_star1;
			double P_star2 = probs[i-1];
			double Q_star2 = 1.0 - P_star2;
			double w2 = P_star2 * Q_star2;
			double w1 = P_star1 * Q_star1;
			output += (cat.discrimination[item] * cat.discrimination[item]) *(((w2 - w1) * (w2 - w1)) 
				/ (P_star2- P_star1));		
		}
	}
	else{
		double P = probability(cat, theta, item);
		double Q = 1.0 - P;
		double temp = (P - cat.guessing[item]) / (1.0 - cat.guessing[item]);
		temp *= temp;
		output = (cat.discrimination[item] * cat.discrimination[item]) * temp * (Q / P);
	}
	return output;
}

double dLL(Cat & cat, double theta, bool use_prior){
	if(cat.applicable_rows.size() == 0){
		return ((theta - cat.prior_params[0]) / (cat.prior_params[1] * cat.prior_params[1]));
	}

	double L_theta = 0.0;
	if(cat.poly){
		for(unsigned int i = 0; i < cat.applicable_rows.size(); ++i){
			int item = cat.applicable_rows[i];
			int answer_k = cat.answers[item];
			std::vector<double> probs;
			probs.push_back(1.0);
			probability(cat, theta, item, probs);
			probs.push_back(0.0); 
			double P_star1 = probs[answer_k];
			double Q_star1 = 1.0 - P_star1;
			double P_star2 = probs[answer_k-1];
			double Q_star2 = 1 - P_star2;
			double P = P_star2 - P_star1;
			double w2 = P_star2 * Q_star2;
			double w1 = P_star1 * Q_star1;
			L_theta = L_theta + (cat.discrimination[item] * ((w2 - w1) / P));
		}
	}
	else{
		for(unsigned int i = 0; i < cat.applicable_rows.size(); ++i){
			int item = cat.applicable_rows[i];
			double P = probability(cat, theta, item);
			L_theta = L_theta + cat.discrimination[item] * ((P - cat.guessing[item]) / (P * (1.0 - cat.guessing[item]))) 
				* (cat.answers[item] - P);
		}
	}
	if(use_prior){
		L_theta -= ((theta - cat.prior_params[0]) / (cat.prior_params[1] * cat.prior_params[1]));
	}
	return L_theta;
}

double d2LL(Cat & cat, double theta, bool use_prior){
	if(cat.applicable_rows.size() == 0){
		return -1.0 / (cat.prior_params[1] * cat.prior_params[1]);
	}

	double Lambda_theta = 0.0;
	if(cat.poly){
		for(unsigned int i = 0; i < cat.applicable_rows.size(); ++i){
			int item = cat.applicable_rows[i];
			int answer_k = cat.answers[item];
			std::vector<double> probs;
			probs.push_back(1.0);
			probability(cat, theta, item, probs);
			probs.push_back(0.0);
			double P_star1 = probs[answer_k];
			double Q_star1 = 1.0 - P_star1;
			double P_star2 = probs[answer_k-1];
			double Q_star2 = 1 - P_star2;
			double P = P_star2 - P_star1;
			double w2 = P_star2 * Q_star2;
			double w1 = P_star1 * Q_star1;
			Lambda_theta = Lambda_theta + (cat.discrimination[item] * cat.discrimination[item]) * (((-w1 * (Q_star1 - P_star1) 
				+ w2 * (Q_star2 - P_star2)) / P) - (((w2 - w1) * (w2 - w1)) / (P*P)));
		}
	}
	else{
		for(unsigned int i = 0; i < cat.applicable_rows.size(); ++i){
			int item = cat.applicable_rows[i];
			double P = probability(cat, theta, item);
			double Q = 1.0 - P;
			double Lambda_temp = (P - cat.guessing[item]) / (1.0 - cat.guessing[item]);
			Lambda_temp *= Lambda_temp;
			Lambda_theta = Lambda_theta - (cat.discrimination[item] * cat.discrimination[item]) * Lambda_temp * (Q / P);
		}
	}
	if(use_prior){
		Lambda_theta -= (1.0 / (cat.prior_params[1] * cat.prior_params[1])); 
	}
	return Lambda_theta;
}

double estimateTheta(Cat & cat) {
	double results = 0.0;
	if(cat.estimation_method == Cat::EAP){
		if (cat.integration_method == Cat::TRAPEZOID) {
			std::vector<double> fx;
			std::vector<double> fx_x;
			for (unsigned int i = 0; i < cat.X.size(); ++i) {
				fx.push_back(likelihood(cat, cat.X[i], cat.applicable_rows) * cat.prior_values[i]);
				fx_x.push_back(cat.X[i] * fx[i]);
			}
			results = trapezoidal_integration(cat.X, fx_x) / trapezoidal_integration(cat.X, fx);
		}
		else{
			//other intergrations methods not yet implemented
			throw -1; 
		}
	}
	else if(cat.estimation_method == Cat::MAP){
		double theta_hat_old = 0.0, theta_hat_new = 1.0;
		double tolerance = 0.0000001; 
		double difference = std::abs(theta_hat_new - theta_hat_old);
		while(difference > tolerance){
			theta_hat_new = theta_hat_old - (dLL(cat, theta_hat_old, true) / d2LL(cat, theta_hat_old, true));
			difference = std::abs(theta_hat_new - theta_hat_old);
			theta_hat_old = theta_hat_new;
		}
		results = theta_hat_new;
	}
	else{
		// other estimation methods not yet implemented
		throw -2; 
	}
	
	return results;
}

double estimateSE(Cat & cat) {
	double results = 0.0;
	double theta_hat = estimateTheta(cat);
	if (cat.integration_method == Cat::TRAPEZOID) {
		std::vector<double> fx;
		std::vector<double> fx_theta;
		for (unsigned int i = 0; i < cat.X.size(); ++i) {
			fx.push_back(likelihood(cat, cat.X[i], cat.applicable_rows) * cat.prior_values[i]);
			fx_theta.push_back((cat.X[i] - theta_hat) * (cat.X[i] - theta_hat) * fx[i]);
		}
		results = sqrt(trapezoidal_integration(cat.X, fx_theta) / trapezoidal_integration(cat.X, fx));
	}
	else{
		//other integration methods not yet implemented
		throw -1;
	}
	return results;
}

double expectedPV(Cat cat, int item) {
	double sum = 0.0;
	cat.applicable_rows.push_back(item); // add item to set of answered items
	if (cat.poly) {
		std::vector<double> variances;
		for (unsigned int i = 0, size = cat.poly_difficulty[item].size() + 1; i < size; ++i) {
			cat.answers[item] = i + 1;
			variances.push_back(estimateSE(cat));
			variances[i] *= variances[i];
		}
		cat.answers[item] = NA_INTEGER;
		cat.applicable_rows.pop_back();
		std::vector<double> question_cdf;
		question_cdf.push_back(1.0);
		probability(cat, estimateTheta(cat), item, question_cdf);
		question_cdf.push_back(0.0);
		for (unsigned int i = 0, size = question_cdf.size() - 1; i < size; ++i) {
			sum += variances[i] * (question_cdf[i] - question_cdf[i + 1]);
		}
	} else {
		cat.answers[item] = 0;
		double variance_zero = estimateSE(cat);
		variance_zero *= variance_zero; 
		cat.answers[item] = 1;
		double variance_one = estimateSE(cat);
		variance_one *= variance_one;
		cat.applicable_rows.pop_back();
		cat.answers[item] = NA_INTEGER; // remove answer
		double prob_zero = probability(cat, estimateTheta(cat), item);
		double prob_one = 1.0 - prob_zero;
		sum = (prob_zero * variance_zero) + (prob_one * variance_one);
	}
	return sum;
}

Cat constructCppCat(S4 cat_df){
	std::vector<double> X = as<std::vector<double> >(cat_df.slot("X"));
	std::string priorName = as<std::string>(cat_df.slot("priorName"));
	std::vector<double> priorParams = as<std::vector<double> >(cat_df.slot("priorParams"));
	std::vector<double> prior_values;

	for(unsigned int i = 0; i < X.size(); ++i){
		prior_values.push_back(prior(X[i], priorName, priorParams));
	}

	// Precalculate the rows that have been answered.
	std::vector<int> applicable_rows;
	std::vector<int> nonapplicable_rows;
	std::vector<int> answers = as<std::vector<int> >(cat_df.slot("answers"));
	for (unsigned int i = 0; i < answers.size(); i++) {
		if (answers[i] != NA_INTEGER) {
			applicable_rows.push_back(i);
		} else {
			nonapplicable_rows.push_back(i + 1);
		}
	}

	bool poly = as<std::vector<bool> >(cat_df.slot("poly"))[0];
	std::vector<std::vector<double> > poly_difficulty; // if poly, construct obj with vector<vector<double>> for difficulty
	std::vector<double> nonpoly_difficulty;
	if(poly){
		// Unpack the difficulty list
		List cat_difficulty = cat_df.slot("difficulty");
		for (List::iterator itr = cat_difficulty.begin(); itr != cat_difficulty.end(); ++itr) {
			poly_difficulty.push_back(as<std::vector<double> >(*itr)); // if poly, set poly_difficulty to vector<vector<double>
		}
	}
	else{
		// if non-poly, set non_poly difficulty to vector<double>
		nonpoly_difficulty = as<std::vector<double> >(cat_df.slot("difficulty"));
	}


	//Construct C++ Cat object
	Cat cat(as<std::vector<double> >(cat_df.slot("guessing")), as<std::vector<double> >(cat_df.slot("discrimination")),
		prior_values, priorName, priorParams, as<std::vector<int> >(cat_df.slot("answers")), 
		as<std::vector<double> >(cat_df.slot("D"))[0], as<std::vector<double> >(cat_df.slot("X")),
		as<std::vector<double> >(cat_df.slot("Theta.est")), poly_difficulty, nonpoly_difficulty, applicable_rows, 
		nonapplicable_rows, poly, as<std::string>(cat_df.slot("integration")), as<std::string>(cat_df.slot("estimation")),
		as<std::string>(cat_df.slot("selection")), as<std::vector<double> >(cat_df.slot("coverage"))[0], 
		as<std::vector<int> >(cat_df.slot("points"))[0] );

	return cat;
}

List nextItemEPVcpp(Cat & cat) {
	// For every unanswered item, calculate the epv of that item
	std::vector<double> epvs;
	int min_item = -1;
	double min_epv = std::numeric_limits<double>::max();

	for (unsigned int i = 0, size = cat.nonapplicable_rows.size(); i < size; ++i) {
		epvs.push_back(expectedPV(cat, cat.nonapplicable_rows[i] - 1));
		if (epvs[i] < min_epv) {
			min_item = cat.nonapplicable_rows[i];
			min_epv = epvs[i];
		}
	}
	DataFrame all_estimates = DataFrame::create(Named("questions")=cat.nonapplicable_rows, Named("EPV")=epvs);
	NumericVector next_item = wrap(min_item);
	return List::create(Named("all.estimates")=all_estimates, Named("next.item")=next_item);
}

List nextItemMFI(Cat & cat){
	std::vector<double> mfis;
	double max_mfi = 0.0;
	int max_item = -1;

	double theta = estimateTheta(cat);
	for(unsigned int i = 0; i < cat.nonapplicable_rows.size(); ++i){
		mfis.push_back(fisherInf(cat, cat.nonapplicable_rows[i] - 1, theta));
		if(mfis[i] > max_mfi){
			max_item = cat.nonapplicable_rows[i];
			max_mfi = mfis[i];
		}
	}
	DataFrame all_estimates = DataFrame::create(Named("questions")=cat.nonapplicable_rows, Named("MFI")=mfis);
	NumericVector next_item = wrap(max_item);
	return List::create(Named("all.estimates")=all_estimates, Named("next.item")=next_item);
}

List nextItemMLWI(Cat & cat){
	std::vector<double> LWI;
	std::vector<double> Like_X;
	double max_LWI = 0.0;
	int max_item = -1;

	for(unsigned int i = 0; i < cat.X.size(); ++i){
		Like_X.push_back(likelihood(cat, cat.X[i], cat.applicable_rows));
	}
	for(unsigned int i = 0; i < cat.nonapplicable_rows.size(); ++i){
		int item = cat.nonapplicable_rows[i];
		std::vector<double> Like_FI;

		for(unsigned int j = 0; j < cat.X.size(); ++j){
			Like_FI.push_back(Like_X[j] * fisherInf(cat, item - 1, cat.X[j]));
		}
		double this_LWI = trapezoidal_integration(cat.X, Like_FI);
		if(this_LWI > max_LWI){
			max_LWI = this_LWI;
			max_item = item;
		}
		LWI.push_back(this_LWI);
	}

	DataFrame all_estimates = DataFrame::create(Named("questions")=cat.nonapplicable_rows, Named("LWI")=LWI);
	NumericVector next_item = wrap(max_item);
	return List::create(Named("all.estimates")=all_estimates, Named("next.item")=next_item);
}

List nextItemMPWI(Cat & cat){
	std::vector<double> PWI, Like_X, Prior_X;
	double max_PWI = 0.0;
	int max_item = -1;

	Rcpp::Rcout << "before" << std::endl;
	for(unsigned int i = 0; i < cat.X.size(); ++i){
		Like_X.push_back(likelihood(cat, cat.X[i], cat.applicable_rows));
		Prior_X.push_back(prior(cat.X[i], cat.priorEnumToString(), cat.prior_params));
	}
	Rcpp::Rcout << "after" << std::endl;

	for(unsigned int i = 0; i < cat.nonapplicable_rows.size(); ++i){
		int item = cat.nonapplicable_rows[i];
		std::vector<double> Prior_Like_FI;

		for(unsigned int j = 0; j < cat.X.size(); ++j){
			Prior_Like_FI.push_back(Prior_X[j] * Like_X[j] * fisherInf(cat, item - 1, cat.X[j]));
		}

		double this_PWI = trapezoidal_integration(cat.X, Prior_Like_FI);
		if(this_PWI > max_PWI){
			max_PWI = this_PWI;
			max_item = item;
		}
		PWI.push_back(this_PWI);
	}

	DataFrame all_estimates = DataFrame::create(Named("questions")=cat.nonapplicable_rows, Named("PWI")=PWI);
	NumericVector next_item = wrap(max_item);
	return List::create(Named("all.estimates")=all_estimates, Named("next.item")=next_item);
}

double expectedObsInf(Cat & cat, int item){
	double sum = 0.0;
	cat.applicable_rows.push_back(item);
	if(cat.poly){
		std::vector<double> obsInfs;
		for(unsigned int i = 0; i <= cat.poly_difficulty[item].size(); ++i){
			cat.answers[item] = i + 1;
			obsInfs.push_back(obsInf(cat, item, estimateTheta(cat)));
		}
		cat.answers[item] = NA_INTEGER;
		cat.applicable_rows.pop_back();

		std::vector<double> question_cdf;
		question_cdf.push_back(1.0);
		probability(cat, estimateTheta(cat), item, question_cdf);
		question_cdf.push_back(0.0);
		for(unsigned int i = 0; i < question_cdf.size() -1; ++i){
			sum += obsInfs[i] * (question_cdf[i] - question_cdf[i+1]);
		}
	}
	else{
		cat.answers[item] = 0;
		double obsInfZero = obsInf(cat, item, estimateTheta(cat));
		cat.answers[item] = 1;
		double obsInfOne = obsInf(cat, item, estimateTheta(cat));
		cat.applicable_rows.pop_back();
		cat.answers[item] = NA_INTEGER;
		double prob_one = probability(cat, estimateTheta(cat), item);
		sum = ((1.0 - prob_one) * obsInfZero) + (prob_one + obsInfOne);
	}
	return sum;
}

List nextItemMEI(Cat & cat){
	std::vector<double> EI;
	double max_EI = 0.0;
	int max_item = -1;
	for(unsigned int i = 0; i < cat.nonapplicable_rows.size(); ++i){
		int item = cat.nonapplicable_rows[i];
		double this_EI = expectedObsInf(cat, item - 1);
		if(this_EI > max_EI){
			max_EI = this_EI;
			max_item = item;		
		}
		EI.push_back(this_EI);
	}

	DataFrame all_estimates = DataFrame::create(Named("questions")=cat.nonapplicable_rows, Named("EI")=EI);
	NumericVector next_item = wrap(max_item);
	return List::create(Named("all.estimates")=all_estimates, Named("next.item")=next_item);

}

// [[Rcpp::export]]
List nextItem(S4 cat_df){
	Cat cat = constructCppCat(cat_df);
	switch(cat.selection_method){
		case Cat::EPV:
			return nextItemEPVcpp(cat);
		case Cat::MFI:
			return nextItemMFI(cat);
		case Cat::LWI:
			return nextItemMLWI(cat);
		case Cat::PWI:
			return nextItemMPWI(cat);
		case Cat::MEI:
			return nextItemMEI(cat);
		default:
			throw -1;
	}
}

// [[Rcpp::export]]
List lookAheadEPVcpp(S4 cat_df, NumericVector item) {
	int look_ahead_item = as<int>(item) - 1;
	/*NumericVector X = cat_df.slot("X");
	CharacterVector priorName = cat_df.slot("priorName");
	NumericVector priorParams = cat_df.slot("priorParams");
	std::vector<double> prior_values = as<std::vector<double> >(prior(X, priorName, priorParams));
	*/
	std::vector<double> X = as<std::vector<double> >(cat_df.slot("X"));
	std::string priorName = as<std::string>(cat_df.slot("priorName"));
	std::vector<double> priorParams =  as<std::vector<double> >(cat_df.slot("priorParams"));
	std::vector<double> prior_values;

	for(unsigned int i = 0; i < X.size(); ++i){
		prior_values.push_back(prior(X[i], priorName, priorParams));
	}

	// Precalculate the rows that have been answered.
	std::vector<int> applicable_rows;
	std::vector<int> nonapplicable_rows;
	std::vector<int> answers = as<std::vector<int> >(cat_df.slot("answers"));
	for (int i = 0; i < answers.size(); i++) {
		if (i == look_ahead_item) {
			applicable_rows.push_back(i);
		} else if (answers[i] != NA_INTEGER) {
			applicable_rows.push_back(i);
		} else {
			nonapplicable_rows.push_back(i + 1);
		}
	}
	
	bool poly = as<std::vector<bool> >(cat_df.slot("poly"))[0];
	std::vector<std::vector<double> > poly_difficulty; // if poly, construct obj with vector<vector<double>> for difficulty
	std::vector<double> nonpoly_difficulty;
	if(poly){
		// Unpack the difficulty list
		List cat_difficulty = cat_df.slot("difficulty");
		for (List::iterator itr = cat_difficulty.begin(); itr != cat_difficulty.end(); ++itr) {
			poly_difficulty.push_back(as<std::vector<double> >(*itr)); // if poly, set poly_difficulty to vector<vector<double>
		}
	}
	else{
		// if non-poly, set non_poly difficulty to vector<double>
		nonpoly_difficulty = as<std::vector<double> >(cat_df.slot("difficulty"));
	}


	//Construct C++ Cat object
	Cat cat(as<std::vector<double> >(cat_df.slot("guessing")), as<std::vector<double> >(cat_df.slot("discrimination")),
		prior_values, priorName, priorParams, as<std::vector<int> >(cat_df.slot("answers")), 
		as<std::vector<double> >(cat_df.slot("D"))[0], as<std::vector<double> >(cat_df.slot("X")),
		as<std::vector<double> >(cat_df.slot("Theta.est")), poly_difficulty, nonpoly_difficulty, applicable_rows, 
		nonapplicable_rows, poly, as<std::string>(cat_df.slot("integration")), as<std::string>(cat_df.slot("estimation")),
		as<std::string>(cat_df.slot("selection")), as<std::vector<double> >(cat_df.slot("coverage"))[0], 
		as<std::vector<int> >(cat_df.slot("points"))[0]);

	if (look_ahead_item >= cat.answers.size()) {
		stop("Item out of bounds");
	} else if (!(cat.answers[look_ahead_item] == NA_INTEGER)) {
		stop("Item already answered");
	}
	std::vector<DataFrame> all_epvs;
	std::vector<double> min_items;
	for (unsigned int answer = 1, size = cat.poly_difficulty[look_ahead_item].size() + 2; answer < size; ++answer) {
		cat.answers[look_ahead_item] = answer;
		std::vector<double> epvs;
		int min_item = -1;
		double min_epv = std::numeric_limits<double>::max();
		for (unsigned int i = 0, size = nonapplicable_rows.size(); i < size; ++i) {
			epvs.push_back(expectedPV(cat, nonapplicable_rows[i] - 1));
			if (epvs[i] < min_epv) {
				min_item = nonapplicable_rows[i];
				min_epv = epvs[i];
			}
		}
		all_epvs.push_back(DataFrame::create(Named("questions")=nonapplicable_rows, Named("epvs")=epvs));
		min_items.push_back(min_item);
	}
	return List::create(Named("all.epvs")=all_epvs, Named("next.items")=min_items);
}

// [[Rcpp::export]]
List probability(S4 cat_df, NumericVector t, IntegerVector q){
	// convert R inputs
	Cat cat = constructCppCat(cat_df);
	double theta = as<std::vector<double> >(t)[0];
	int question = as<std::vector<int> >(q)[0];

	std::vector<double> probs;
	if(cat.poly){
		probability(cat, theta, question, probs);
	}
	else{
		probs.push_back(probability(cat, theta, question));
	}
	DataFrame question_probs = DataFrame::create(Named("probabilities")=probs);
	return List::create(Named("all.probabilities")=question_probs);
}

// [[Rcpp::export]]
double likelihood(S4 cat_df, NumericVector t){
	Cat cat = constructCppCat(cat_df);
	double theta = as<std::vector<double> >(t)[0];
	return likelihood(cat, theta, cat.applicable_rows);
}

// [[Rcpp::export]]
double obsInf(S4 cat_df, int item, double theta){
	Cat cat = constructCppCat(cat_df);
	return obsInf(cat, item, theta); 
}

// [[Rcpp::export]]
double fisherInf(S4 cat_df, int item, double theta){
	Cat cat = constructCppCat(cat_df);
	return fisherInf(cat, item, theta); 
}

// [[Rcpp::export]]
double dLL(S4 cat_df, NumericVector t, LogicalVector use_p){
	Cat cat = constructCppCat(cat_df);
	double theta = as<std::vector<double> >(t)[0];
	bool use_prior = as<std::vector<bool> >(use_p)[0];
	return dLL(cat, theta, use_p);
}

// [[Rcpp::export]]
double d2LL(S4 cat_df, NumericVector t, LogicalVector use_p){
	Cat cat = constructCppCat(cat_df);
	double theta = as<std::vector<double> >(t)[0];
	bool use_prior = as<std::vector<bool> >(use_p)[0];
	return d2LL(cat, theta, use_p);
}

// [[Rcpp::export]]
double estimateTheta(S4 cat_df){
	Cat cat = constructCppCat(cat_df);
	return estimateTheta(cat);
}

// [[Rcpp::export]]
double estimateSE(S4 cat_df){
	Cat cat = constructCppCat(cat_df);
	return estimateSE(cat);
}

// [[Rcpp::export]]
double prior(NumericVector x, CharacterVector c, NumericVector p){
	double x_ =  as<std::vector<double> >(x)[0];
	std::string name = as<std::string>(c);
	std::vector<double> params = as<std::vector<double> >(p);
	return prior(x_, name, params);
}

// [[Rcpp:export]]
double trapezoidal_integration(NumericVector x, NumericVector fx){
	std::vector<double> x_ = as<std::vector<double> >(x);
	std::vector<double> fx_ = as<std::vector<double> >(fx);
	return trapezoidal_integration(x_, fx_);
}

// [[Rcpp:export]]
double dnorm(NumericVector x, NumericVector mu, NumericVector sigma){
	return dnorm(as<std::vector<double> >(x)[0], as<std::vector<double> >(mu)[0],
		as<std::vector<double> >(sigma)[0]);
}

// [[Rcpp:export]]
int gamma(IntegerVector n){
	return gamma(as<std::vector<int> >(n)[0]);
}

// [[Rcpp:export]]
double dchi(NumericVector x, NumericVector k){
	return dchi(as<std::vector<double> >(x)[0], as<std::vector<double> >(k)[0]);
}

// [[Rcpp::export]]
double dt(NumericVector x, NumericVector mu, IntegerVector df){
	return dt(as<std::vector<double> >(x)[0], as<std::vector<double> >(mu)[0],
		as<std::vector<int> >(df)[0]);
}