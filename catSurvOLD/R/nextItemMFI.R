#' Computerized Adaptive Testing Survey Maximum Fisher's Information Function
#'
#' This function determines the next item by comparing Fisher's information and choosing the largest and presents it to the respondent.
#'
#' @param cat an object of \code{CATsurv} class.
#'
#' @return The next item to present to the respondent with the largest Fisher's information.
#'
#' @author Josh W. Cutler and Jacob M. Montgomery
#' @seealso \code{\link{likelihood}},\code{\link{prior}}, \code{\link{estimateTheta}}, \code{\link{estimateSE}}, \code{\link{expectedPV}}, \code{\link{nextItem}}, \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname nextItemMFI

#' @export

setGeneric("nextItemMFI", function(cat){standardGeneric("nextItemMFI")})

#' @export
setMethod(f="nextItemMFI", signature=class.name, definition=function(cat) {
  available_questions = data.frame(questions=which(is.na(cat@answers)),MFI=NA)   
  for (i in 1:nrow(available_questions)) {
          items <- available_questions[i,]$questions
          theta.hat <- cat@Theta.est
          exp.portion <- exp(cat@D*cat@difficulty[items]*(theta.hat-cat@discrimination[items]))
          p.prime <- (cat@D*cat@difficulty[items]*(1-cat@guessing[items])*(exp.portion/(1+exp.portion)^2))
          p <- three.pl(cat, theta.hat, cat@difficulty[items], cat@discrimination[items], cat@guessing[items])
          I <- p.prime^2/(p*(1-p))
          available_questions[i,]$MFI = I
          }
            
  next.item = available_questions[available_questions$MFI == max(available_questions$MFI, na.rm=TRUE), 1]
  to.return = list(all.estimates=available_questions, next.item=next.item)
            
return(to.return)
})          
