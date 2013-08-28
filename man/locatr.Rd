\docType{package}
\name{locatr}
\alias{locatr}
\alias{locatr-package}
\title{locatr}
\usage{
  locatr(data, answer)
}
\arguments{
  \item{data}{the data to be tested}

  \item{answer}{the desired answer}
}
\value{
  A list of methods which return the desired answer, as
  well as the permutation of data that produce the desired
  result.
}
\description{
  locatr

  This function uses \code{\link{whatgives}} and tries all
  permutations of the data to find functions that give the
  answer
}

