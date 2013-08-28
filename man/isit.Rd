\name{isit}
\alias{isit}
\title{Test a single procedure to see if it returns the expected answer.}
\usage{
  isit(procName, data, answer)
}
\arguments{
  \item{procName}{the name of a function to test}

  \item{data}{a list containing the data you want to use to
  test the function}

  \item{answer}{a list containing the answer you expect to
  get}
}
\value{
  logical TRUE/FALSE
}
\description{
  This function takes the name of one function to test, as
  well as the data and expected answer, and ouputs a
  TRUE/FALSE value.
}
\examples{
isit(sum, list(3,4), 7)
}

