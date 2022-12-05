#pragma once
#include "Selector.h"

class PKLSelector : public Selector {

public:
	PKLSelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel);

  ~PKLSelector() {};
  
	virtual SelectionType getSelectionType() override;

	virtual Selection selectItem() override;
};



