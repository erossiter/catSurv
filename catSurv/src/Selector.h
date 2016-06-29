#pragma once

#include "Selection.h"
#include "QuestionSet.h"
#include "Estimator.h"

enum class SelectionType {
	NONE, EPV, MFI, MEI, MWPI
};


class Selector {
public:
	virtual SelectionType getSelectionType() = 0;

	virtual Selection selectItem() = 0;

	Selector(QuestionSet &questions, Estimator &estimation, Prior &priorModel);

protected:
	QuestionSet &questionSet;
	Estimator &estimator;
	Prior &prior;
};

