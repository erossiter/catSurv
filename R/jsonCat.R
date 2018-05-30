#' Convert JSON object to Cat object
#'
#' This function  object of class \code{Cat}.
#'
#' @param jsonObj A \code{json} object with elements matching Cat object slots
#'
#'@return The function \code{jsonCat} returns an object of class \code{Cat} with slots populated with the corresponding information stored in the \code{jsonObj}.
#' 
#' See \code{\link{Cat-class}} for required \code{Cat} object slots.
#' 
#' @details The \code{jsonObj} argument of the function \code{jsonCat} is an object of class \code{json} from the \code{rjson} package.  This function simply populates a \code{Cat} object with the information provided in the \code{jsonObj}, thus element and slot names must match.
#' 
#' 
#' @examples
#' ## Creating json object
#' library(jsonlite)
#' json_cat <- toJSON(list(guessing = rep(0, 10),
#' discrimination = rnorm(n = 10, 0, 1.5),
#' difficulty = rnorm(n = 10, 0, 3),
#' answers = sample(c(0,1), size = 10, replace = TRUE),
#' priorName = "NORMAL",
#' priorParams = c(0,1),
#' lowerBound = -5,
#' upperBound = 5,
#' model = "ltm",
#' estimation = "EAP",
#' estimationDefault = "EAP",
#' selection = "EPV",
#' z = .9,
#' lengthThreshold = NA,
#' seThreshold = NA,
#' infoThreshold = NA,
#' gainThreshold = NA,
#' lengthOverride = NA,
#' gainOverride = NA))
#' 
#' cat <- jsonCat(json_cat)
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
#' @rdname jsonCat
#' 
#' @import jsonlite
#' @name jsonCat
setOldClass("json")


setGeneric("jsonCat", function(jsonObj){
  standardGeneric("jsonCat")
})

#' @rdname jsonCat
#' @export
setMethod("jsonCat",
          signature(jsonObj = "json"),
          function(jsonObj){
            list_cat <- fromJSON(jsonObj)
            return_cat <- new("Cat")
            
            if(! identical(sort(names(list_cat)), sort(slotNames(return_cat)))){
              stop("jsonObj names must match slots of Cat object.")
            }
            for(i in slotNames(return_cat)){
              slot(return_cat, i) <- list_cat[[i]]
            }
            
            if(!validObject(return_cat)){
              stop("Problem...")
            }
            return(return_cat)
})