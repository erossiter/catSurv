' Update Answer to Single Item
#'
#' Stores answer to item \eqn{k} to the \code{Cat} object's \code{answers} slot.
#'
#' @param catObj An object of class \code{character}.
#' @param item An integer indicating the index of the question item
#' @param answer The answer to the \code{item} to be updated
#'
#' @details Blah
#' @return Blah
#'  
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil, Joshua Landman 
#' 
#' @name processAJAX
NULL

setGeneric("processAJAX", function(catObj, item, answer) standardGeneric("storeAnswer"))


#' @rdname storeAnswer
#' @export
setMethod(f = "processAJAX", signature = "character", definition = function(catObj, item, answer){
    catObj <- fromJSONCat(catObj)
    catObj@answers[item] <- answer
    validObject(catObj)
    nexts <- selectItem(catObj)
    nexts$newCat <- toJSONCat(catObj)
    return(nexts)  
})
