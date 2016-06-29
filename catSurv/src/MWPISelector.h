#pragma once
#include "Selector.h"

class MWPISelector : public Selector {

public:
	MWPISelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel);

	virtual SelectionType getSelectionType() override;

	virtual Selection selectItem() override;
};



