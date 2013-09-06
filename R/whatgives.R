#' Find the function that gives the result you want (for a specific ordering of data)
#'
#' This function takes a list of data and an expected answer and tests all known functions for the expected answer.
#' Data are tested exactly in the order they were entered. To test permutations of the data, use \code{\link{locatr}}.
#' 
#' @param data a list containing the data, formatted as you believe is necessary
#' @param answer the answer you expect (may someday need to be a list?)
#' @return A list of potential functions for the specified data
#' @export

whatgives <- function(data, answer, anstr=NULL){
  fnList <- ls("package:base")
 # badfunctions <- dget("badfunctions.robj")
  if(length(which(duplicated(badfunctions)))>0){
    stop("duplicated badfunctions")
  }
  listNoSideEffects <- fnList[fnList %in% badfunctions==FALSE]
  if((length(listNoSideEffects)==length(fnList)-length(badfunctions))==FALSE){
    stop("you might not be removing something you want to be")
  }
  binaryOps <- listNoSideEffects[1:38]
  output <- NULL
  i <- 1
  if(missing(anstr)){
    anstr <- substitute(data)[-1]
  }
  for (procName in listNoSideEffects){
    if(isit(procName, data, answer)){
      flagb <- procName %in% binaryOps
      output[i] <- prettyfunctionprint(procName, data, anstr, flagb)
      i <- i + 1
    }
  }
#   
#   condition_call <- substitute(answer)
#   env <- list2env(data, parent=parent.frame())
#   r <- eval(condition_call, env)
  
  if(length(output)==0){
    return("sorry, no functions found")
  }
  else{
    return(output)
  }
}





prettyfunctionprint <- function(procName, data, anstr=NULL, flagb){
if(flagb){
  output <- paste(data[[1]], procName, data[[2]])
}
# Otherwise, use the parentheses format
else{
  dataU <- paste0(anstr, collapse=", ")
  output <- paste0(procName, "(", dataU, ")")
}
return(output)
}