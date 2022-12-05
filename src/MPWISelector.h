#pragma once
#include "Selector.h"

class MPWISelector : public Selector {

public:
	MPWISelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel);
  
  ~MPWISelector() {};

	virtual SelectionType getSelectionType() override;

	virtual Selection selectItem() override;
};



