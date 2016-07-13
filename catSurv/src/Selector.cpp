#include "Selector.h"

/**
 * An abstract class that represents the various ways of selecting the next question.
 */
Selector::Selector(QuestionSet &questions, Estimator &estimation, Prior &priorModel)
		: questionSet(questions), estimator(estimation), prior(priorModel) {}

Selection Selector::selectItem_strata(int strata_choice) {
  std::vector<int> original_nonapplicable_rows = questionSet.nonapplicable_rows;
  questionSet.nonapplicable_rows.clear();

  for (auto item : original_nonapplicable_rows) {
    if (questionSet.strata[item] == strata_choice) {
      questionSet.nonapplicable_rows.push_back(item);
    }
  }
  Selection answer = selectItem();
  
  questionSet.nonapplicable_rows.clear();
  questionSet.nonapplicable_rows = original_nonapplicable_rows;
  
  return answer;
}