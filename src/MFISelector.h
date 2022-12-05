#pragma once
#include "Selector.h"

class MFISelector : public Selector {

public:
	MFISelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel);
  
  ~MFISelector() {};
  
	virtual SelectionType getSelectionType();

	virtual Selection selectItem();
};

