#' Estimates theta under different adaptive battery specifications
#'
#' Takes in response profiles from multiple respondents and multiple Cat object (i.e., adaptive battery) specifications and returns a set of theta estimates
#'
#' @param catObjs A list of \code{Cat} objects of the same model with different adaptive battery specifications
#' @param responses A matrix of response profiles
#' @param return_adaptive Boolean indicating if user wants dataframe containing only answers chosen via the adaptive design for each Cat object in catObjs list.
#'
#' @details The function takes multiple \code{Cat} objects, stored in a list, and generates an estimation for \code{theta}.
#' 
#' @return The function \code{simulateThetas} returns a dataframe where each \code{Cat} object corresponds to a column and each respondent
#' corresponds to a row if \code{return_adaptive} is \code{FALSE}, the default.
#' Optionally, \code{simulateThetas} returns a list containing that dataframe plus dataframes
#' for the answer profiles simulated via each adaptive design if \code{return_adaptive} is \code{TRUE}.
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
#' grmList <- list(cat1 = grm_MAP, cat2 = grm_EAP)
#' 
#' # Results
#' theta_est_results <- simulateThetas(catObjs = grmList, responses = respondents)
#' 
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil, Jaerin Kim, Dominique Lockett 
#' 
#' @rdname simulateThetas
#' 
#' @export
simulateThetas <- function(catObjs, responses, return_adaptive = FALSE){
    UseMethod("simulateThetas", catObj)
}


simulateThetas<- function(catObjs=list(), responses, return_adaptive = FALSE){
    
    # checks
    if(length(unique(lapply(catObjs, function(x) x@model))) != 1){
        stop("Cat objects must be of the same model e.g., grm.")
    }
    if(any(unlist(lapply(catObjs, function(x) length(x@answers) != length(responses))))){
        stop("Response profile is not compatible with Cat object.")
    }
    if(any(is.na(names(catObjs))) |  is.null(names(catObjs))  ){
        stop("Elements of the catObjs list need to be uniquely named.")
    }

    # If the user wants the profile and the estimate return
    if(return_adaptive){
        # Set up storage
        # First element is a matrix for all estimates
        # Additional elements each a matrix for response profiles
        out <- list(estimates = data.frame(matrix(NA, nrow = nrow(responses), ncol = length(catObjs))))
        colnames(out$estimates) <- names(catObjs)
        for(i in 1:length(catObjs)){
            out[[names(catObjs)[i]]] <- data.frame(matrix(NA, nrow = nrow(responses), ncol = ncol(responses)))
            colnames(out[[names(catObjs)[i]]]) <- colnames(responses)
        }

        # Simulation
        for(resp_i in 1:nrow(responses)){
            for(cat_i in 1:(length(catObjs))){
                temp <- simulator(x = unlist(responses[resp_i,]),
                                  catObj = catObjs[[cat_i]],
                                  return_adaptive = TRUE)
                out$estimates[resp_i, cat_i] <- temp$est
                out[[cat_i+1]][resp_i,] <- temp$ans_profile
            }
        }
    }else{
        # for loop for now
        out <- matrix(NA, nrow = nrow(responses), ncol = length(catObjs))
        for (i in 1:length(catObjs)){
            out[,i] <- apply(X = responses,
                      MARGIN = 1,
                      FUN = simulator,
                      catObj = catObjs[[i]],
                      return_adaptive = FALSE)
            
        }
        colnames(out) <- names(catObjs)
        out <- as.data.frame(out)
    }
    
    return(out)
}



simulator <- function(x, catObj, return_adaptive){
    end_survey <- FALSE
    while(! end_survey){
        item <- tryCatch({
            selectItem(catObj)$next_item
        }, error = function(err){
            print(err)
            saved_selection <- catObj@selection
            catObj@selection <- "RANDOM"
            item <- selectItem(catObj)$next_item
            catObj@selection <- saved_selection
            return(item)
        }, warning = function(cond){
            selectItem(catObj)$next_item
        })
        
        answer <- x[item]
        answer <- ifelse(is.na(answer), -1, answer) # indicate respondent "skipped"
        catObj <- storeAnswer(catObj = catObj, item = item, answer = answer)
        end_survey <- checkStopRules(catObj) # should survey stop?
    }
    
    est <- tryCatch({
        estimateTheta(catObj)
    }, error = function(err){
        print(err)
        if(catObj@estimation == "EAP"){
            return(NA)
        }else{
            catObj@estimation <- "EAP"
            return(estimateTheta(catObj)) 
        }
    }, warning = function(cond){
        estimateTheta(catObj)
    })
    if(return_adaptive){
        out <- list(est = est, ans_profile = catObj@answers) 
    }else{
        out <- est
    }
    
    return(out)
}