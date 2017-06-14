#pragma once
#include "Selector.h"

class MLWISelector : public Selector {

public:
	MLWISelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel);

	virtual SelectionType getSelectionType() override;

	virtual Selection selectItem() override;
};



