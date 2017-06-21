#pragma once
#include "Selector.h"

class RANDOMSelector : public Selector {

public:
	RANDOMSelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel);

	virtual SelectionType getSelectionType() override;

	virtual Selection selectItem() override;
};



