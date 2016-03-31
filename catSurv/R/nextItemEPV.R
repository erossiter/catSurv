#' Computerized Adaptive Testing Survey Minimum Expected Posterior Variance Function
#'
#' This function determines the next item by comparing expected posterior variance and choosing the smallest and presents it to the respondent.
#'
#' @param cat an object of \code{CATsurv} class.
#'
#' @return The next item to present to the respondent with the smallest expected posterior variance.
#'
#' @author Josh W. Cutler and Jacob M. Montgomery
#' @seealso \code{\link{likelihood}},\code{\link{prior}}, \code{\link{estimateTheta}}, \code{\link{estimateSE}}, \code{\link{expectedPV}}, \code{\link{nextItem}}, \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname nextItemEPV

#' @export


setGeneric("nextItemEPV", function(cat,...){standardGeneric("nextItemEPV")})


setMethod(f="nextItemEPV", signature=class.name, definition=function(cat, available_questions) {
  available_questions = data.frame(questions=which(is.na(cat@answers)), EPV=NA)

  ## A workaround to allow for refusals.
  cat@answers[which(cat@answers==-1)]<-NA
  for (i in 1:nrow(available_questions)) {
    available_questions[i,]$EPV = expectedPV(cat, available_questions[i,]$questions)
  }
  next.item = available_questions[available_questions$EPV == min(available_questions$EPV, na.rm=TRUE),1]
  to.return = list(all.estimates=available_questions, next.item=next.item)
  
  return(to.return)
})
