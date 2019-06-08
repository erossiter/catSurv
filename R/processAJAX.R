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
#' @author Joshua Landman 
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
        nexts <- tryCatch(
          {
            as.list(lookAhead(catObj, item))
          }, error = function(err) {
            list(response_option = c(1, 2, 3, 4, 5), next_item = c(-1, -1, -1, -1, -1))
          }
        )
        
        fakecat <- catObj
        fakecat@answers[item] <- -1
        nullNext <- selectItem(fakecat)$next_item
        
        nexts$response_option <- c(-1, nexts$response_option)
        nexts$next_item <- c(nullNext, nexts$next_item)
        
        nexts$newCat <- toJSONCat(catObj)
        if (firstThing) {
          nexts$firstThing <- item
        }
    } else {
        nexts <- list(all = "NULL")
    }
    return(nexts)
})
