#pragma once

#include <Rcpp.h>
#include "Selector.h"
#include "../Cat.h"

class EPVSelector : Selector {

	double expectedPV(QuestionSet questionSet, int item);

	virtual Rcpp::List virtual nextItem(QuestionSet questionSet);
};

