#pragma once
#include "Selector.h"

class LKLSelector : public Selector {

public:
	LKLSelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel);
  
  ~LKLSelector() {};

	virtual SelectionType getSelectionType() override;

	virtual Selection selectItem() override;
};



