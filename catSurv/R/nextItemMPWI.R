#' Computerized Adaptive Testing Survey Maximum Posterior Weighted Information Function
#'
#' This function determines the next item by comparing posterior weighted Fisher's information and choosing the largest and presents it to the respondent.
#'
#' @param cat an object of \code{CATsurv} class.
#'
#' @return The next item to present to the respondent with the largest posterior weighted Fisher's information.
#'
#' @author Josh W. Cutler and Jacob M. Montgomery
#' @seealso \code{\link{likelihood}},\code{\link{prior}}, \code{\link{estimateTheta}}, \code{\link{estimateSE}}, \code{\link{expectedPV}}, \code{\link{nextItem}}, \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname nextItemMPWI

#' @export
setGeneric("nextItemMPWI", function(cat){standardGeneric("nextItemMPWI")})

#' @export
setMethod(f="nextItemMPWI", signature=class.name, definition=function(cat) {
  available_questions = data.frame(questions=which(is.na(cat@answers)),MPWI=NA)
  
  posterior<- function(theta, cat, items){
    likelihood(cat, theta, items)*prior(cat, theta, cat@priorName, cat@priorParams)
  }
  
  posterior.theta <- sapply(cat@X,posterior,cat,items=which(!is.na(cat@answers)))
  
  for (i in 1:nrow(available_questions)) {
    FishInfo <- expectedInfo(cat,cat@X,available_questions[i,1])
    available_questions[i,]$MPWI <- integrate.xy(cat@X,FishInfo*posterior.theta)
  }
  
  next.item = available_questions[available_questions$MPWI == max(available_questions$MPWI, na.rm=TRUE), 1 ]
  to.return = list(all.estimates=available_questions, next.item=next.item)
  return(to.return)
}
)
