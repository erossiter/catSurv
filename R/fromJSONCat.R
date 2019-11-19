#' Convert JSON object to Cat object
#'
#' This function  object of class \code{Cat}.
#'
#' @param jsonObj A \code{json} object with elements matching Cat object slots
#'
#'@return The function \code{fromJSONCat} returns an object of class \code{Cat} with slots populated with the corresponding information stored in the \code{jsonObj}.
#' 
#' See \code{\link{Cat-class}} for required \code{Cat} object slots.
#' 
#' @details The \code{jsonObj} argument of the function \code{fromJSONCat} is an object of class \code{json} from the \code{rjson} package.  This function simply populates a \code{Cat} object with the information provided in the \code{jsonObj}, thus element and slot names must match.
#' 
#' 
#'
#' 
#' @seealso 
#' 
#' \code{\link{Cat-class}}
#' 
#' 
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil
#' 
#'
#' @rdname fromJSONCat
#' 
#' @import jsonlite
#' @name fromJSONCat
setOldClass("json")


setGeneric("fromJSONCat", function(jsonObj){
  standardGeneric("fromJSONCat")
})

#' @rdname fromJSONCat
#' @export
setMethod("fromJSONCat",
          signature(jsonObj = "json"),
          function(jsonObj){
            list_cat <- jsonlite::fromJSON(jsonObj, simplifyMatrix = FALSE)
            return_cat <- new("Cat")
            
            if(! identical(sort(names(list_cat)), sort(slotNames(return_cat)))){
              stop("jsonObj names must match slots of Cat object.")
            }
            for(i in slotNames(return_cat)){
              slot(return_cat, i) <- list_cat[[i]]
            }
            names(return_cat@discrimination) <- return_cat@ids
            
            if(!validObject(return_cat)){
              stop("Problem...")
            }
            return(return_cat)
})


#' @rdname fromJSONCat
#' @export
setMethod("fromJSONCat",
          signature(jsonObj = "character"),
          function(jsonObj){
              list_cat = jsonlite::fromJSON(jsonObj, simplifyMatrix = FALSE)
              return_cat <- new("Cat")
              
              if(! identical(sort(names(list_cat)), sort(slotNames(return_cat)))){
                  stop("jsonObj names must match slots of Cat object.")
              }
              for(i in slotNames(return_cat)){
                  slot(return_cat, i) <- list_cat[[i]]
              }
              names(return_cat@discrimination) <- return_cat@ids
              
              if(!validObject(return_cat)){
                  stop("Problem...")
              }
              return(return_cat)
          })


