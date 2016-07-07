#pragma once
#include "Selector.h"

class PKLSelector : public Selector {

public:
	PKLSelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel);

	virtual SelectionType getSelectionType() override;

	virtual Selection selectItem() override;
};



