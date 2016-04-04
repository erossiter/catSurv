#' Computerized Adaptive Testing Survey Possible Combination of Question Paths
#'
#' This function returns the all possible combinations of question paths up to 5th stage or \code{n-1}th stage when the length of questions is less than or equal to 5.
#'
#' @param cat an object of \code{CATsurv} class.
#' @param ability.estimator The estimation procedure used to estimate the respondent's position on the latent scale.  The three options are "EAP" for expected a posterior (the default),  "ML" for maximum likelihood, and "MAP" for maximum a posterior.
#' @param item.selection The item selection procedure.  The five options are "EPV" for minimum expected posterior variance (the default),  "KL" for Kullback-Leibler, "MFI" for maximum Fisher's information, "MPWI" for maximum posterior wieghted information, "MWFI" for maximum weighted Fisher's information, "MEI" for maximum expected information. 
#'
#' @return A list consisting of the all possible question paths according to the answer at each stage.
#'
#' @author Josh W. Cutler and Jacob M. Montgomery
#' @seealso \code{\link{likelihood}},\code{\link{prior.value}}, \code{\link{estimateTheta}}, \code{\link{estimateSE}}, \code{\link{expectedPV}}, \code{\link{nextItem}}, \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname question.path
#' @export
setGeneric("question.path", function(cat,ability.estimator="EAP",item.selection="EPV"){standardGeneric("question.path")})

#' @export 
setMethod(f="question.path", signature=class.name, definition=function(cat,ability.estimator="EAP",item.selection="EPV"){
  ability.estimator="EAP"
  item.selection="EPV"
  varNames <- names(cat@discrimination)
  numPossibleAnswers <- rep(NA, length(varNames))
  for(i in 1:length(varNames)){
    numPossibleAnswers[i] <-  length(cat@difficulty[[i]])+1
  }
  n <- length(cat@answers)
  if(n<=10){
    cat("the length of questions is less than 10.\ncalculate all possible question paths up to",length(cat@answers)-1,"stage.\n")
    arg <- list(NULL)
     for(i in 1:(n-1)){
      arg[[i]] <- seq(0, length(cat@discrimination[i]))
      }
    possible.paths <- expand.grid(arg)
    ord <- order(possible.paths[,1],possible.paths[,ifelse(n<3,1,2)],possible.paths[,ifelse(n<4,1,3)],possible.paths[,ifelse(n<5,1,4)])
    possible.paths <- possible.paths[ord,]  
  } else {possible.paths <- expand.grid(x,x,x,x,x)
          ord <- order(possible.paths[,1],possible.paths[,2],possible.paths[,3],possible.paths[,4],possible.paths[,5])
          possible.paths <- possible.paths[ord,]  
  }

  answer <- NA
  names(answer) <- "NA"
  outcome <- list(NULL)
  outcome[[1]] <- list(answer.history=answer, next.item=q)
  for(i in 1:nrow(possible.paths)){
    for(j in 1:ncol(possible.paths)){
      answer.new <- possible.paths[i,j]
      vectorname <- names(answer)
      vectorname <- c(vectorname, paste("Q",q,sep=""))
      cat@answers[q] <- answer.new
      q <- nextItem(cat, ability.estimator, item.selection)$next.item
      answer <- c(answer, answer.new)
      names(answer) <- vectorname
      outcome[[(i-1)*ncol(possible.paths)+j+1]] <- list(answer.history=answer, next.item=q)
    }
    cat@answers[1:length(cat@answers)] <- NA
    answer <- NA
    names(answer) <- "NA"
    q <- nextItem(cat, ability.estimator, item.selection)$next.item
  }
  outcome <- outcome[!duplicated(outcome)]
  l <- NULL
  for(i in 1:length(outcome)){
    new <- 100*length(outcome[[i]][[1]])+i
    l <- c(l, new)
  }
  l <- rank(l)
  output <- list(NULL)
  for(i in 1:length(outcome)){
    output[[i]] <- outcome[[which(l==i)]]  
  }
  return(output)
})
