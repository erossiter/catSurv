#' Make Tree of Possible Questions
#'
#' This function creates a tree of possible questions asked and stores it as a list of lists.
#'
#' @param cat a \code{Cat} object
#'
#'  @return An object of class \code{list}
#'
#' @seealso \code{\link{FlattenTree}},
#' @author Ryden W. Butler: \email{r.butler@@wustl.edu} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @rdname makeTree
#' @export

makeTree<-function(cat){
  UseMethod("makeTree", cat)
}

#' @export



makeTree <- function(cat, flat = FALSE){

  varNames <- names(cat@discrimination)
  numPossibleAnswers <- rep(NA, length(varNames))
  for(i in 1:length(varNames)){
    numPossibleAnswers[i] <-  length(cat@difficulty[[i]])+2
  }


  ## This should probably be passed in.
  numPossibleAnswers <- c(numPossibleAnswers)
  n <- length(varNames)
  q <- selectItem(cat)$next.item
  this <- list()
  for (i in 1:(numPossibleAnswers[q]-1)){
    this[[paste(i)]] <- NA
  }
  this[[i+1]] <- varNames[q]
  if(cat@poly == FALSE){
    names(this)   <- c(1:(numPossibleAnswers[q]-1)-1, "Next")
  } else {
    names(this)   <- c(1:(numPossibleAnswers[q]-1), "Next")
  }
  #this[["-1"]]<-NA
  this



  treeList <- function(x, cat, varNames, numPossibleAnswers){
    for(i in 1:length(x)){
      namVar <- names(x)
      if(is.na(x[[i]])){
        if(sum(is.na(cat@answers))==1){
          q <- varNames[q]
          x[[namVar[i]]] <-  list(Next=q)
        }
        if(sum(is.na(cat@answers))>1 & sum(!is.na(cat@answers))<(cat@lengthThreshold-1)){
          as.integer(names(x)[i])
          thisQ <- which(varNames==x[["Next"]])
          catNew <- storeAnswer(cat, thisQ, as.integer(names(x)[i]))
          q <- selectItem(catNew)$next.item
          for (j in 1:(numPossibleAnswers[q])){
            x[[namVar[i]]][[j]] <- NA
          }
          x[[namVar[i]]][[j]] <- varNames[q]
          if(cat@poly == FALSE){
          names(x[[namVar[i]]])   <- c(1:(numPossibleAnswers[q]-1)-1,
                                       #-1,
                                       "Next")
          } else {
            names(x[[namVar[i]]])   <- c(1:(numPossibleAnswers[q]-1),
                                         #-1,
                                         "Next")
          }
          x[[namVar[i]]] <- as.list(x[[namVar[i]]])

          x[[namVar[i]]] <- treeList(x=x[[namVar[i]]],cat=catNew,
                                  varNames=varNames, numPossibleAnswers=numPossibleAnswers)
        }
        if(sum(!is.na(cat@answers))>=(cat@lengthThreshold-1)){
          thisQ <- which(varNames==x[["Next"]])
          thisQ
          catNew <- storeAnswer(cat, thisQ, as.integer(names(x)[i]))
          q <-  selectItem(catNew)$next.item
          q <- varNames[q]
          x[[namVar[i]]] <-  list(Next=q)
        }
      }
    }
    return(x)
  }
  tree <- treeList(this, cat, varNames=varNames, numPossibleAnswers=numPossibleAnswers)
  if(flat == FALSE){
    out <- tree
  } else {
    flattenTree <- function(tree) {
      
      flatTree <- unlist(tree)
      names(flatTree) <- sub("Next", "", names(flatTree))
      flatTree <- flatTree[order(nchar(names(flatTree)))]
      
      if(cat@poly == FALSE){
        for(i in 1:length(flatTree)){
          if(substring(names(flatTree[i]), 1,1) == 1){
            flatTree <- c(flatTree, flatTree[i])
          }
        }
        remove <- which(substring(names(flatTree), 1,1) == 1)
        remove <- remove[1:(length(remove)/2)]
        flatTree <- flatTree[-c(remove)]
      } else {
        answerChoices <- length(unique(substring(names(flatTree), 1,1))) - 1
        orderedTree <- flatTree[1]
        for(i in 1:answerChoices){
          answers <- rep(NA, (length(orderedTree)-1)/answerChoices)
          answers <- flatTree[substring(names(flatTree), 1,1) == i]
          orderedTree <- c(orderedTree, answers)
        }
        flatTree <- orderedTree
      }
      output <- matrix(data = NA, nrow = length(flatTree), ncol = length(cat@answers) + 1)
      colnames(output) <- c(names(cat@difficulty), "NextItem")
      
      for(i in 1:length(flatTree)){
        output[i,ncol(output)] <- flatTree[i]
        if(i > 1){
          if(nchar(names(flatTree[i])) == 2){
            output[i, output[1,ncol(output)]] <- substring(names(flatTree[i]),
                                                           nchar(names(flatTree[i]))-1,
                                                           nchar(names(flatTree[i]))-1)
          } else {
            for(j in 1:((nchar(names(flatTree[i]))/2)-1)){
              output[i, flatTree[substring(names(flatTree[i]), 1, nchar(names(flatTree[i]))-2*j)]] <- substring(names(flatTree[i]),
                                                                                                                nchar(names(flatTree[i]))-((j*2) - 1),
                                                                                                                nchar(names(flatTree[i]))-((j*2) - 1))
            }
            output[i, output[1,ncol(output)]] <- substring(names(flatTree[i]), 1,1)
          }
        }
      }
      output <- as.table(as.matrix(output))
      return(output)
    }
    out <- flattenTree(tree)
  }
  
  return(out)
}

