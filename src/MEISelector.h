#pragma once


#include "Selector.h"

class MEISelector : public Selector {

public:
	MEISelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel);

	virtual SelectionType getSelectionType();

	virtual Selection selectItem();
};

