#pragma once
#include <Rcpp.h>
#include "../QuestionSet.h"

enum SelectionType {
	EPV, MFI, LWI, PWI, MEI, invalid
};

class Selector {
	SelectionType selection_method;
	virtual Rcpp::List virtual nextItem(QuestionSet questionSet) = 0;
};

