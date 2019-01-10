#' Estimates theta with multiple specifications
#'
#' Takes response profiles from multiple respondents and multiple Cat objects and returns a set of theta estimates
#'
#' @param respondents A matrix of response profiles
#' @param catObjs A list of \code{Cat} objects of the same class (i.e., LTM, TPM, GRM, GPCM) specifying different estimation routines
#'
#'
#' @details The function takes a \code{Cat} object and generates an estimation for \code{theta} using \code{MAP}, \code{EAP}, etc. 
#' The user must store all \code{Cat} objects in a single list.
#' 
#' @return The function \code{allEst} returns a vector.  Each \code{Cat} object returns a column and each respondent returns a row.
#' 
#' 
#' This function is to allow users to look at data computed before \code{Cat} technologies were availabe and and analyze multpile types of analyses on mulptile resondents to achieve a theta estimate.
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{apply}}, \code{\link{selectItem}}, \code{\link{store}}
#' 
#' @examples 
#' 
#'  ## Loading ltm Cat object
#' data(grm_cat)
#'    
#' ## load sample data
#'  
#' respondents<-apply(thetaValue[1:3], 2, simulateRespondents, cat=grm_cat, n=10)
#' 
#' #Choose your \code{cat} object and the types of estimation you would like to use
#'  grm_MAP<-grm_cat
#'  grm_MAP@estimation<-"MAP"
#'  grm_EAP<-grm_cat
#'  grm_EAP@estimation<-"EAP"
#' 
#' 
#' grmList<-list(grm_MAP, grm_EAP)
#' ## Create a threshold for the estimation
#' 
#' ## You must set some sort of CheckStopRule
#' grm_cat@lengthThreshold<-4
#' 
#' ## Run 
#' 
#' allEst(catObjs=grmList, resp=respondents)
#' 
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil, Jaerin Kim, Dominique Lockett 
#' 
#' @rdname allEst
#' 
#' @export
allEst <- function(catObjs, resp){
    UseMethod("allEst", catObj)
}

allEst<- function(catObjs=list(), resp){
    
    # checks
    if(length(unique(lapply(catObjs, function(x) x@model))) != 1){
        stop("Cat objects must be of the same model e.g., grm.")
    }
    if(any(unlist(lapply(catObjs, function(x) length(x@answers) != length(resp))))){
        stop("Response profile is not compatible with Cat object.")
    }
    
    
    # for loop for now
    out <- matrix(NA, nrow = nrow(resp), ncol = length(catObjs))
    for (i in 1:length(catObjs)){
        out[,i] <- apply(X = resp,
                            MARGIN = 1,
                            FUN = function(x, catObj){
                                cat <- catObj
                                continue <- TRUE
                                while(continue){
                                    item <- selectItem(cat)$next_item
                                    answer <- x[item]
                                    cat <- storeAnswer(catObj = cat, item = item, answer = answer)
                                    
                                    continue <- tryCatch({
                                        checkStopRules(cat)
                                    }, error = function(err){
                                        print(err)
                                        saved_estimation <- cat@estimation
                                        cat@estimation <- "EAP"
                                        continue <- checkStopRules(cat)
                                        cat@estimation <- saved_estimation
                                        return(continue)
                                    })
                                }
                                return(estimateTheta(cat))
                            },
                            catObj = catObjs[[i]])
    }
    colnames(out) <- paste0(catObjs[[1]]@model, unlist(lapply(catObjs, function(x) x@estimation)))
    return(data.frame(out))
}