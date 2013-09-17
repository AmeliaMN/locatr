\name{isitperm}
\alias{isitperm}
\title{The function that asks you before it tests every single function}
\usage{
  isitperm(data, answer, ask = TRUE)
}
\arguments{
  \item{data}{the data (in a list) that you want to test}

  \item{answer}{the answer you expect}

  \item{ask}{a binary TRUE/FALSE of whether you want to be
  prompted at the command line every time \code{isitperm}
  tries another function, or if it should just print each
  function name as it goes.}
}
\description{
  This function will probably not be in the final version
  because it's quite time consuming, but can be useful for
  debugging. It runs through all the good functions,
  printing the names of the functions as it goes, so you
  can see which function is causing the hang-up.
}

