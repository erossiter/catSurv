#pragma once

#include "Selection.h"
#include "QuestionSet.h"
#include "Estimator.h"

enum class SelectionType {
	NONE, EPV, MFI, MEI
};


class Selector {
public:
	virtual SelectionType getSelectionType() = 0;

	virtual Selection nextItem() = 0;

	Selector(QuestionSet &questions, Estimator &estimation, Prior &priorModel);

protected:
	QuestionSet &questionSet;
	Estimator &estimator;
	Prior &prior;
};
