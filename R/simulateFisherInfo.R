#' Calculates Fisher Information under different adaptive battery specifications
#'
#' Takes in a a \code{Cat} object, a set of respondents, and their corresponding \code{theta} values, and calculates the amount of information given an adaptive battery.  
#'
#' 
#' @param catObjs A list of \code{Cat} objects of the same class.
#' @param theta A vector of numerics representing the true value of theta.
#' @param responses A dataframe of answer profiles corresponding to the true values of theta.
#' 
#' @details The function takes a \code{Cat} object, \code{theta}, and response profiles. 
#' The user defines the selection type, estimation type, etc. so that the questions can be applied adaptively
#' These adaptive profiles are then used to calculate the total inforamtion gained for a respondent for all answered
#' items, conditioned on \code{theta}.
#' 
#' @return The function \code{simulateFisherInfo} returns a dataframe where each \code{Cat} object corresponds to a column and each respondent corresponds to a row.
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{fisherTestInfo}}, \code{\link{selectItem}}
#' 
#' @examples 
#' 
#' # Load Cat object
#' data(grm_cat)
#'    
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
#' fisher_inf_results <- simulateFisherInfo(catObjs = grmList,
#'                               theta = rep(c(-1, 0, 1),
#'                               each = 10),
#'                               responses = respondents)
#' 
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil, Jaerin Kim, Dominique Lockett 
#' 
#' @rdname simulateFisherInfo
#' 
#' @export
simulateFisherInfo <- function(catObjs, theta, responses){
    UseMethod("simulateFisherInfo", catObj)
}

simulateFisherInfo <- function(catObjs=list(), theta, responses){
    
    # checks
    if(length(unique(lapply(catObjs, function(x) x@model))) != 1){
        stop("Cat objects must be of the same model e.g., grm.")
    }
    if(any(unlist(lapply(catObjs, function(x) length(x@answers) != length(responses))))){
        stop("Response profile is not compatible with Cat object.")
    }
    if(length(theta) != nrow(responses)){
        stop("Need a value of theta to correspond with each response profile.")
    }

    responses$theta <- theta
    
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
                                
                                return(fisherTestInfo(cat, theta = x["theta"]))
                            },
                            catObj = catObjs[[i]])
    }
    colnames(out) <- paste0("cat", 1:length(catObjs))
    return(data.frame(out))
}