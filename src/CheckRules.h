#pragma once
#include <vector>
#include <Rcpp.h>


struct CheckRules {

  double lengthThreshold;
	double seThreshold;
	double infoThreshold;
	double gainThreshold;
	double lengthOverride;
	double gainOverride;
	
	CheckRules(Rcpp::S4 &cat_df);
};

