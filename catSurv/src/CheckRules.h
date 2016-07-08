#pragma once
#include <vector>
#include <Rcpp.h>
#include "QuestionSet.h"
#include "Estimator.h"
#include "Prior.h"

class CheckRules {

public:
  double lengthThreshold;
	double seThreshold;
	double infoThreshold;
	double gainThreshold;
	double lengthOverride;
	double gainOverride;
	
	CheckRules(Rcpp::S4 &cat_df, QuestionSet &question, Estimator &estimation, Prior &priorModel);
	
	bool checkRules(Prior &prior);
	
	bool check_lengthThreshold();
	
	bool check_seThreshold(Prior &prior);
	
protected:
	QuestionSet &questionSet;
  Estimator &estimator;
  Prior &prior;
};

