#pragma once
#include "Selector.h"

class MFIISelector : public Selector {

public:
	MFIISelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel);
  
  ~MFIISelector() {};
  
	virtual SelectionType getSelectionType();

	virtual Selection selectItem();
};

