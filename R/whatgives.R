#' Find the function that gives the result you want (for a specific ordering of data)
#'
#' This function takes a list of data and an expected answer and tests all known functions for the expected answer.
#' Data are tested exactly in the order they were entered. To test permutations of the data, use \code{\link{locatr}}.
#' 
#' @param data a list containing the data, formatted as you believe is necessary
#' @param answer the answer you expect (may someday need to be a list?)
#' @return A list of potential functions for the specified data
#' @export

whatgives <- function(data, answer, anstr=NULL, names=FALSE){
  listNoSideEffects <- goodfunctions
  
  binaryOps <- c("-",":","::",":::","!","!=" , "*","/","&","&&","%*%","%/%","%%","%in%" ,"%o%", "%x%",
                 "^","+","<","==",">",">=","|","||","~","$")
  parens <- c("(","[","[[", "{")
  output <- NULL
  procNames <- NULL
  i <- 1
  if(missing(anstr)){
    anstr <- substitute(data)[-1]
  }
  for (procName in listNoSideEffects){
    if(isit(procName, data, answer)){
      if(procName %in% binaryOps){
        flagb <- "binary"
      } else if(procName %in% parens){
        flagb <- "paren"
      } else {
        flagb <- "regular"
      }
      output[i] <- prettyfunctionprint(procName, anstr, flagb)
      if(names){
        procNames[i] <- procName
      }
      i <- i + 1
    }
  }
#   
#   condition_call <- substitute(answer)
#   env <- list2env(data, parent=parent.frame())
#   r <- eval(condition_call, env)
  
  if(length(output)==0){
    return("sorry, no functions found")
  }else if (names==FALSE){
    return(output)
  }else {
    return(data.frame(output, procNames))
  }
}



#' This helper function creates nice output strings for locatr functions
#' 
#' @param procName the name of a working function to return
#' @param anstr the data as a string
#' @param flagb a character string containing "binary", "paren" or "normal"
#' 
#' @return a nicely formatted string

prettyfunctionprint <- function(procName, anstr, flagb){
  if(flagb=="binary"){
    output <- paste0(anstr[1], procName, anstr[2])
  } else if(flagb=="paren"){
    parens <- c("(", ")", "[", "]","[[", "]]", "{", "}")
    otherside <- parens[which(parens == procName)+1]
    output <- paste0(anstr[1], procName, paste0(anstr[-1], collapse=", "), otherside)
  } else{
    dataU <- paste0(anstr, collapse=", ")
    output <- paste0(procName, "(", dataU, ")")
  }
  return(output)
}