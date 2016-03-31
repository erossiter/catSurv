#' Computerized Adaptive Testing Survey Maximum Weighted Fisher's Information Function
#'
#' This function determines the next item by comparing weighted Fisher's information and choosing the largest and presents it to the respondent.
#'
#' @param cat an object of \code{CATsurv} class.
#'
#' @return The next item to present to the respondent with the largest weighted Fisher's information.
#'
#' @author Josh W. Cutler and Jacob M. Montgomery
#' @seealso \code{\link{likelihood}},\code{\link{prior}}, \code{\link{estimateTheta}}, \code{\link{estimateSE}}, \code{\link{expectedPV}}, \code{\link{nextItem}}, \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname nextItemMWFI

#' @export
setGeneric("nextItemMWFI", function(cat,...){standardGeneric("nextItemMWFI")})

#' @export
setMethod(f="nextItemMWFI", signature=class.name, definition=function(cat, available_questions) {
  available_questions = data.frame(questions=which(is.na(cat@answers)),MWFI=NA)
  applicable_rows = which(!is.na(cat@answers))
  
  prior.values = prior(cat, cat@X, cat@priorName, cat@priorParams)
  likelihood.values = rep(NA, times=length(cat@X))
  
  for (i in 1:length(likelihood.values)) {
    likelihood.values[i] = likelihood(cat, cat@X[i], applicable_rows)
  }
  
  
  for (i in 1:nrow(available_questions)) {
    FishInfo <- expectedInfo(cat, cat@X, available_questions[i,1])
    available_questions[i,]$MWFI = integrate.xy(cat@X, FishInfo*likelihood.values)
  }
    
  next.item = available_questions[available_questions$MWFI == max(available_questions$MWFI, na.rm=TRUE), 1 ]
  to.return = list(all.estimates=available_questions, next.item=next.item)
  return(to.return)
}
)


