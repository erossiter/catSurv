#' Simulate answer profiles given some true value of theta
#'
#' The function simulates \code{n} answer profiles given a true value of theta and a battery's item parameters stored in a \code{Cat} object.
#'
#' @param catObj An object of class \code{Cat}
#' @param theta A numeric representing the true position on the latent trait.
#' @param n A numeric indicating the number of answer profiles to simulate.
#'
#'
#' @return Function returns a dataframe where each row is a possible answer profile simulated given the provided value of theta
#'  
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
#' 
#' @examples
#' 
#' # Load Cat object
#' data(grm_cat)
#' 
#' # Simulate 5 response profiles given a theta of 2
#' sim_resp <- simulateRespondents(catObj = grm_cat, theta = 2, n = 5)
#'
#' 
#' @name simulateRespondents
NULL

setGeneric("simulateRespondents", function(catObj, theta, n) standardGeneric("simulateRespondents"))

#' @rdname simulateRespondents
#' @export
setMethod(f = "simulateRespondents", signature = "Cat", definition = function(catObj, theta, n){
    if(sum(!is.na(catObj@answers)) != 0){
        stop("Cat object should not have respondent specific answers.")
    }
    
    ans_profiles <- matrix(nrow = n, ncol = length(catObj@answers))
    for(respondent in 1:n){
        for(i in 1:length(catObj@answers)){
            probs <- probability(catObj = catObj, theta = theta, item = i)
            
            ## need to calculate answer probabilities from cumulative probabilities
            if(catObj@model == "grm"){
                probs <- diff(probs)
            }
            ## need to append probability of answering a 0
            if(catObj@model == "ltm" | catObj@model == "tpm"){
                probs <- c(1 - probs, probs)
            }
            ## gpcm is fine
            
            ## now generate answers with those probabilities
            ans_profiles[respondent, i] <- sample(1:(length(catObj@difficulty[[i]])+1), 1, prob = probs)
        }
    }
    colnames(ans_profiles) <- names(catObj@discrimination)
    return(as.data.frame(ans_profiles))
})


