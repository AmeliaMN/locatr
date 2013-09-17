\name{whatgives}
\alias{whatgives}
\title{Find the function that gives the result you want (for a specific ordering of data)}
\usage{
  whatgives(data, answer, anstr = NULL, names = FALSE)
}
\arguments{
  \item{data}{a list containing the data, formatted as you
  believe is necessary}

  \item{answer}{the answer you expect (may someday need to
  be a list?)}
}
\value{
  A list of potential functions for the specified data
}
\description{
  This function takes a list of data and an expected answer
  and tests all known functions for the expected answer.
  Data are tested exactly in the order they were entered.
  To test permutations of the data, use
  \code{\link{locatr}}.
}

