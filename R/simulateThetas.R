#' Estimates theta under different adaptive battery specifications
#'
#' Takes in response profiles from multiple respondents and multiple Cat object (i.e., adaptive battery) specifications and returns a set of theta estimates
#'
#' @param catObjs A list of \code{Cat} objects of the same model with different adaptive battery specifications
#' @param responses A matrix of response profiles
#'
#' @details The function takes multiple \code{Cat} objects, stored in a list, and generates an estimation for \code{theta}.
#' 
#' @return The function \code{allFish} returns a dataframe where each \code{Cat} object corresponds to a column and each respondent corresponds to a row.
#' 
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{apply}}, \code{\link{selectItem}}
#' 
#' @examples 
#' 
#' # Load Cat object
#' data(grm_cat)
#'    
#' # Simulate respondents
#' # Simulate respondents
#' respondents <- plyr::adply(.data = matrix(c(-1, 0, 1)),
#'                            .margins = 1,
#'                            .id = NULL,
#'                            .fun = simulateRespondents, cat = grm_cat, n = 10)
#'  
#' # A stopping rule (here, a common one) is required
#' grm_cat@lengthThreshold <- 3
#' 
#' # Specify different adaptive inventory procedures
#' grm_MAP <- grm_EAP <- grm_cat
#' grm_MAP@estimation <- "MAP"
#' grm_EAP@estimation <- "EAP"
#' 
#' # List of Cat objects 
#' grmList <- list(grm_MAP, grm_EAP)
#' 
#' # Results
#' theta_est_results <- simulateThetas(catObjs = grmList, responses = respondents)
#' 
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil, Jaerin Kim, Dominique Lockett 
#' 
#' @rdname simulateThetas
#' 
#' @export
simulateThetas <- function(catObjs, responses){
    UseMethod("simulateThetas", catObj)
}

simulateThetas<- function(catObjs=list(), responses){
    
    # checks
    if(length(unique(lapply(catObjs, function(x) x@model))) != 1){
        stop("Cat objects must be of the same model e.g., grm.")
    }
    if(any(unlist(lapply(catObjs, function(x) length(x@answers) != length(responses))))){
        stop("Response profile is not compatible with Cat object.")
    }
    
    
    # for loop for now
    out <- matrix(NA, nrow = nrow(responses), ncol = length(catObjs))
    for (i in 1:length(catObjs)){
        out[,i] <- apply(X = responses,
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
                                
                                est <- tryCatch({
                                    estimateTheta(cat)
                                }, error = function(err){
                                    print(err)
                                    if(cat@estimation == "EAP"){
                                        return(NA)
                                    }else{
                                        cat@estimation <- "EAP"
                                        return(estimateTheta(cat)) 
                                    }
                                })
                                return(est)
                            },
                            catObj = catObjs[[i]])
    }
    colnames(out) <- paste0("cat", 1:length(catObjs))
    return(data.frame(out))
}