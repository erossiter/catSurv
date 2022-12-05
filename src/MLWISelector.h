#pragma once
#include "Selector.h"

class MLWISelector : public Selector {

public:
	MLWISelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel);
  
  ~MLWISelector() {};

	virtual SelectionType getSelectionType() override;

	virtual Selection selectItem() override;
};



