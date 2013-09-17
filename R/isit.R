#' Test a single procedure to see if it returns the expected answer.
#' 
#' This function takes the name of one function to test, as well as the data and expected answer, 
#' and ouputs a TRUE/FALSE value. 
#' @param procName the name of a function to test
#' @param data a list containing the data you want to use to test the function
#' @param answer a list containing the answer you expect to get
#'
#' @return logical TRUE/FALSE
#' @export
#' @examples
#' isit(sum, list(3,4), 7)

isit <- function(procName, data, answer){
  if(is.list(data)==FALSE){
    stop("please provide a list containing your data")
  }
  ptm <- proc.time()
  stopifnot((proc.time() - ptm)[2]<1)
  trialAnswer <- tryCatch(
    do.call(procName, data)
    , error = function(e) as.character(e)
    , warning = function(w) as.character(w)
  )
  return(isTRUE(all.equal(answer, trialAnswer, tolerance=0.001, check.attributes=FALSE, check.names=FALSE)))
}

