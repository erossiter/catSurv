#' Update Answer to Single Item
#'
#' Stores answer to item \eqn{k} to the \code{Cat} object's \code{answers} slot.
#'
#' @param catObj An object of class \code{Cat} or class \code{json}.
#' @param item An integer indicating the index of the question item
#' @param answer The answer to the \code{item} to be updated
#' @param returnJSON Boolean indicating whether the updated \code{Cat} object should be returned as a JSON object.  Defaults to FALSE.
#'
#' @details The function \code{storeAnswer} updates the \code{Cat} object, but the updated object must be assigned to an object for the changes to be stored.  See \strong{Examples}.
#'
#' @return The function \code{storeAnswer} returns an updated object of class \code{Cat} with the \code{answers} slot reflecting the newly stored \code{answer} to the indicated \code{item}.  All previously stored answers remain the same, and all unanswered questions remain \code{NA}.
#'  
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
#' 
#' @examples 
#' ## Loading ltm Cat object
#' data(ltm_cat)
#'
#'## Printing current answers slot
#'getAnswers(ltm_cat)
#'
#'## Storing answer of 0 to item 1
#'ltm_cat <- storeAnswer(ltm_cat, item = 1, answer = 0)
#'
#'## Now object reflects answer to item 1
#'getAnswers(ltm_cat)
#'
#' 
#' @name storeAnswer
NULL

setGeneric("storeAnswer", function(catObj, item, answer, returnJSON = NULL) standardGeneric("storeAnswer"))

#' @rdname storeAnswer
#' @export
setMethod(f = "storeAnswer", signature = "Cat", definition = function(catObj, item, answer, returnJSON = FALSE){
  catObj@answers[item] <- answer
  validObject(catObj)
  if(returnJSON){
      return(toJSONCat(catObj))  
  }
  else{
      return(catObj)
  }
})



#' @rdname storeAnswer
#' @export
setMethod(f = "storeAnswer", signature = "character", definition = function(catObj, item, answer, returnJSON = FALSE){
    catObj <- fromJSONCat(catObj)
    catObj@answers[item] <- answer
    validObject(catObj)
    if(returnJSON){
        return(toJSONCat(catObj))  
    }
    else{
        return(catObj)
    }
})


