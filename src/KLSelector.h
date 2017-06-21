#pragma once
#include "Selector.h"

class KLSelector : public Selector {

public:
	KLSelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel);

	virtual SelectionType getSelectionType() override;

	virtual Selection selectItem() override;
};



