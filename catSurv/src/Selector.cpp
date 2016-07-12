#include "Selector.h"

/**
 * An abstract class that represents the various ways of selecting the next question.
 */
Selector::Selector(QuestionSet &questions, Estimator &estimation, Prior &priorModel)
		: questionSet(questions), estimator(estimation), prior(priorModel) {}

Selection Selector::selectItem_strata(int strata_choice) {
  std::vector<int> original_applicable_rows = questionSet.applicable_rows;
  questionSet.applicable_rows.clear();

  for (auto item : original_applicable_rows) {
    if (questionSet.strata[item] == strata_choice) {
      questionSet.applicable_rows.push_back(item);
    }
  }
  Selection answer = selectItem();
  
  questionSet.applicable_rows.clear();
  questionSet.applicable_rows = original_applicable_rows;
  
  return answer;
}