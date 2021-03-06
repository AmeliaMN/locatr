#' Find the function that gives the result you want (for all permutations of your data)
#' 
#' This function uses \code{\link{whatgives}} and tries all permutations of the data to find functions 
#' that give the answer
#'
#' @param data the data to be tested
#' @param answer the desired answer
#' @return A list of methods which return the desired answer, as well as the permutation of data that 
#' produce the desired result.
#' @export
#' @importFrom gtools permutations

locatr <- function(data, answer){
  ptm <- proc.time()
  stopifnot((proc.time() - ptm)[2]<1)
require(gtools)
output <- NULL
orderings <- permutations(length(data), length(data), 1:length(data))
for (i in 1:dim(orderings)[1]){
  methodList <- NULL
  methodList <- whatgives(data[orderings[i,]], answer, as.character(substitute(data)[c(orderings[i,]+1)]))
  if(all(methodList != "sorry, no functions found")){
    output <- c(output, methodList)
  }
}
  if(all(output == "sorry, no functions found")){
    return("sorry, no functions found")
  }
  else{
    return(data.frame(output))
  }

}
