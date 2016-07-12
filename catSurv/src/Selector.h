#pragma once

#include "Selection.h"
#include "QuestionSet.h"
#include "Estimator.h"

enum class SelectionType {
	NONE, EPV, MFI, MEI, MPWI, MLWI, KL, LKL, PKL, RANDOM
};


class Selector {
public:
	virtual SelectionType getSelectionType() = 0;

	virtual Selection selectItem() = 0;
	
	Selection selectItem_strata(int strata_choice);

	Selector(QuestionSet &questions, Estimator &estimation, Prior &priorModel);

protected:
	QuestionSet &questionSet;
	Estimator &estimator;
	Prior &prior;
};

