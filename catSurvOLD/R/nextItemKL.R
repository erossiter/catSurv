#' Computerized Adaptive Testing Survey Next Item Function - Kullback Leibler Method
#'
#' This function takes a respondent's previous answers to determine the next item from the list of available questions, determined according to each remaining item's Kullback Leibler information. 
#'
#' @param cat An object of class \code{CATsurv}
#'
#' @return A list of available items, thier KL information, and the next item to ask the respondent.
#'  
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso \code{\link{three.pl}},\code{\link{likelihood}}, \code{\link{prior.value}}, \code{\link{estimateTheta}}, \code{\link{estimateSE}}, \code{\link{expectedPV}},  \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname nextItemKL
#' @export
setGeneric("nextItemKL", function(cat,...){standardGeneric("nextItemKL")})

#' @export
setMethod(f="nextItemKL", signature="CATsurv", definition=function(cat, available_questions) {
  available_questions = data.frame(questions=which(is.na(cat@answers)),KL=NA)
  
  num.asked <- sum(!is.na(cat@answers))
  
  delta.int <- seq(from=cat@Theta.est+cat@lowerBound*(1-num.asked/length(cat@answers)), to=cat@Theta.est+cat@upperBound*(1-num.asked/length(cat@answers)), length=cat@quadPoints)
  i=2
  for (i in 1:nrow(available_questions)) {
    p.theta.hat <- three.pl(cat,cat@Theta.est,cat@difficulty[available_questions[i,1]], cat@discrimination[available_questions[i,1]], cat@guessing[available_questions[i,1]])
    
    p.theta.null <- three.pl(cat,delta.int,cat@difficulty[available_questions[i,1]], cat@discrimination[available_questions[i,1]], cat@guessing[available_questions[i,1]])
    
    K.function <- p.theta.null*log(p.theta.null/p.theta.hat)+(1-p.theta.null)*log((1-p.theta.null)/(1-p.theta.hat)) 
    
    available_questions[i,]$KL <- integrate.xy(delta.int,K.function)
    }

next.item = available_questions[available_questions$KL == max(available_questions$KL, na.rm=TRUE), 1 ]
to.return = list(all.estimates=available_questions, next.item=next.item)
return(to.return)
  }
)
  
  
