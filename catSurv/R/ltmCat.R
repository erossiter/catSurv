#' Computerized Adaptive Testing Latent Trait Model with Binary Data
#'
#' This function fits the latent trait model for binary data and populates the fitted values for discimination, difficulty, and guessing parameters to an object of class \code{Cat}.  
#'
#' @param data a \code{data.frame} or a numeric \code{matrix} of manifest variables. 
#' @param object an object of class \code{Cat} to be populated. If omitted, a new object of class \code{Cat} is created.
#' @param ... arguments to be passed to methods. For more details about the arguments, see \code{\link{tpm}}.
#'
#'  @return An object of class \code{Cat} with components,
#' \itemize{
#' \item \code{difficulty} a named vector of difficulty parameters for use with dichotomous questions/items.  Each element's name tells the question/item to which it applies.
#' \item \code{guessing} a vector of guessing parameter for each item.
#' \item \code{discrimination} a vector of disrimination parameter for each item.
#' \item \code{answers} a vector of answers to questions as given by the survey respondent.
#' \item \code{priorName} a character vector of length one giving the prior distribution to use for the latent trait estimates.  The options are \code{normal} for the normal distirbution, \code{cauchy} for the Cauchy distribution, are \code{t} for the t-distribution. Defaults to \code{normal}. 
#' \item \code{priorParams} a numeric vector of parameters for the distribution specified in the \code{priorName} slot. See the details section for more infomration.  Defaults to \code{c(0,1)}.   
#' }
#' 
#' @note In case the Hessian matrix at convergence is not positive definite try to use \code{start.val="random"}. 
#' 
#' @seealso \code{\link{grmCat}}, \code{\link{nextItem}}, \code{\link{question.path}}
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @rdname ltmCat
#' @export
setGeneric("ltmCat", function(data, object=NULL, ...){standardGeneric("ltmCat")})

#' @export
setMethod(f="ltmCat", signature="data.frame", 
          definition=function(data, object,...){
            if(!is.null(object)) if(class(object)!="Cat") stop("object is not class Cat")            
            fit <- tpm(data,...)
            coefficient <- coef(fit)
            answer <- rep(NA,nrow(coefficient))
            discrimination <- coefficient[,"Dscrmn"]
            difficulty <- coefficient[,"Dffclt"]
            guessing <- coefficient[,"Gussng"]
            names(difficulty) <- rownames(coefficient)
            if(is.null(object)){
              object<-new("Cat")
            }
            
            object@discrimination <- discrimination
            object@difficulty <- difficulty
            object@poly <- TRUE
            object@guessing <- 0
            object@answers <- answer
            
            # guessing, discrimination, answers, difficulty should all be same length
            test1<-(length(object@discrimination)==length(object@guessing))  
            if(!test1){stop("discrimination and guessing not same length")}
            
            test2<-(length(object@discrimination)==length(object@answers))  
            if(!test2){stop("discrimination and answers not same length")}
            
            test3<-(length(object@discrimination)==length(object@difficulty))  
            if(!test3){stop("discrimination and difficulty not same length")}
            
            ## TEST THAT DIFFICULTY VALUES ARE STRICTLY INCREASING, and not NA
            if(object@poly==T){
              for(i in object@difficulty){
                if (is.list(i)){
                  i<-unlist(i)
                }
                sorted<-sort(i)
                uniques<-unique(i)
                test4<-(isTRUE(all.equal(i,uniques)))
                if(!test4){stop(paste("Repeated difficulty values for question ", which(object@difficulty==i, arr.ind=T)))}
                test5<-(isTRUE(all.equal(i,sorted)))
                if(!test5){stop(paste("Diffulty values for question ", which(object@difficulty==i, arr.ind=T), " are not increasing"))}
                test6<-(all(!is.na(i)))
                if(!test6){stop(paste("Diffulty values for question ", which(object@difficulty==i, arr.ind=T), " include NAs"))}
                
              }
            }
            
            ## test that discrimination and guessing are not NA
            for(i in object@discrimination){
              test7<-!is.na(i)
              if(!test7){stop(paste("Discrimination value for question ", which(object@discrimination==i, arr.ind=T), " is NA"))}
            }
            if(!object@poly){
              for(i in object@guessing){
                test8<-!is.na(i)
                if(!test8){stop(paste("Discrimination value for question ", which(object@discrimination==i, arr.ind=T), " is NA"))}
              }
            }
          })
