#' Computerized Adaptive Testing Survey Maximum Expected Information Criterion Function
#'
#' This function determines the next item by comparing maximum expected information and choosing the largest and presents it to the respondent.
#'
#' @param cat an object of \code{CATsurv} class.
#'
#' @return The next item to present to the respondent with the maximum expected information.
#'
#' @author Josh W. Cutler and Jacob M. Montgomery
#' @seealso \code{\link{nextItem}},\code{\link{nextItemEPV}}, \code{\link{nextItemKL}}, \code{\link{nextItemMFI}}, \code{\link{nextItemMPWI}}, \code{\link{nextItemMWFI}}
#' @rdname nextItemMEI

#' @export
setGeneric("nextItemMEI", function(cat,...){standardGeneric("nextItemMEI")})

#' @export
setMethod(f="nextItemMEI", signature=class.name, definition=function(cat, available_questions) {
  available_questions = data.frame(questions=which(is.na(cat@answers)),MEI=NA)
  
  for( i in 1:nrow(available_questions)){
  prob.correct <- three.pl(cat, cat@Theta.est, cat@difficulty[available_questions$questions[i]], cat@discrimination[available_questions$questions[i]], cat@guessing[available_questions$questions[i]])
  prob.incorrect <- 1-prob.correct
  
  old_val = cat@answers[available_questions$questions[i]]
  
  cat@answers[available_questions$questions[i]] = 1
  obsInfo.correct = observedInfo(cat, cat@Theta.est, available_questions[i,1])
  
  cat@answers[available_questions$questions[i]] = 0
    obsInfo.incorrect = observedInfo(cat, cat@Theta.est, available_questions[i,1])
  cat@answers[available_questions$questions[i]] = if (is.null(old_val) || is.na(old_val)) NA else old_val
  
    available_questions$MEI[i] <- (prob.correct*obsInfo.correct)+(prob.incorrect*obsInfo.incorrect)
  }
     
  next.item = available_questions[available_questions$MEI == max(available_questions$MEI, na.rm=TRUE), 1 ]
  to.return = list(all.estimates=available_questions, next.item=next.item)
  return(to.return)
}
)