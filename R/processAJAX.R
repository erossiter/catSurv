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

setGeneric("processAJAX", function(catObj, item) standardGeneric("processAJAX"))


#' @rdname processAJAX
#' @export
setMethod(f = "processAJAX", signature = "character", definition = function(catObj, item){
    catObj <- fromJSONCat(catObj)
    firstThing <- F
    
    if (item == -1) {
      item <- selectItem(catObj)$next_item
      firstThing <- T
    }
    
    validObject(catObj)
    nexts <- NULL
    if (!checkStopRules(catObj)) {
        nexts <- as.list(lookAhead(catObj, item))
        nexts$newCat <- toJSONCat(catObj)
        if (firstThing) {
          nexts$firstThing <- item
        }
    } else {
        nexts <- list(next_item = "NULL", next_item_name = "NULL", newCat = "NULL")
    }
    return(nexts)
})
