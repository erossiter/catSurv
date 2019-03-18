#' A function that finds total information that can be gained via answered items .
#'
#' Takes in a a \code{Cat} object, a set of respondents, and their corresponding \code{theta} values, to calculate the amount of information which can be extracted given an adaptive battery  
#'
#' 
#' @param catObjs A list of \code{Cat} objects of the same class.
#' @param theta A vector of numerics representing the true value of theta.
#' @param resp One or many full response profiles.
#' 
#' @details The function takes a \code{Cat} object, \code{theta}, and response profiles. 
#' The user defines the selection type, estimation type, etc. so that the questions can be applied adaptively
#' These adaptive profiles are then used to calculate the total inforamtion gained for a respondent for all answered
#' items, conditioned on \code{theta}.
#' 
#' @return The function \code{allFish} returns a dataframe.  Each \code{Cat} object returns a column and each respondent returns a row.
#' 
#' 
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{fisherTestInfo}}, \code{\link{selectItem}}, \code{\link{store}}
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
#' allFish(catObjs=grmList, resp=respondents)
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil, Jaerin Kim, Dominique Lockett 
#' 
#' @rdname allFish
#' 
#' @export
allFish <- function(catObjs, theta, resp){
    UseMethod("allFish", catObj)
}

allFish <- function(catObjs=list(), theta, resp){
    
    # checks
    if(length(unique(lapply(catObjs, function(x) x@model))) != 1){
        stop("Cat objects must be of the same model e.g., grm.")
    }
    if(any(unlist(lapply(catObjs, function(x) length(x@answers) != length(resp))))){
        stop("Response profile is not compatible with Cat object.")
    }
    if(length(theta) != nrow(resp)){
        stop("Need a value of theta to correspond with each response profile.")
    }

    resp$theta <- theta
    
    # for loop for now
    out <- matrix(NA, nrow = nrow(resp), ncol = length(catObjs))
    for (i in 1:length(catObjs)){
        out[,i] <- apply(X = resp,
                            MARGIN = 1,
                            FUN = function(x, catObj){
                                cat <- catObj
                                end_survey <- FALSE
                                while(! end_survey){
                                    item <- tryCatch({
                                        selectItem(cat)$next_item
                                    }, error = function(err){
                                        print(err)
                                        saved_selection <- cat@selection
                                        cat@selection <- "RANDOM"
                                        item <- selectItem(cat)$next_item
                                        cat@selection <- saved_selection
                                        return(item)
                                    })
                                    
                                    answer <- x[item]
                                    cat <- storeAnswer(catObj = cat, item = item, answer = answer)
                                    end_survey <- checkStopRules(cat) # should survey stop?
                                }
                                
                                return(fisherTestInfo(cat, theta = x["theta"]))
                            },
                            catObj = catObjs[[i]])
    }
    colnames(out) <- paste0("cat", 1:length(catObjs))
    return(data.frame(out))
}