\name{Permutation}
\alias{Permutation}

\title{Permutation}
\description{
  A package for doing permutations and bootstraping easily in R. 
  This package provide a means for hypothesis testing when the data does not follow the assumtions of parametric test. 
  A permutation test is done by doing resampling of the observed data without replacement, 
  thereby creating the null-distribution for the association between two variables. 
  Bootstraping is done by doing resampling of the observed data woth replacement, 
  thereby creating the sampling distribution of a desirered estimate, 
  e.g the difference in mean. From the Sampling distribution, 
  the standart error and confidence intervals are calculated.
}
\usage{
  Functions in this package (see more about the specific functions in the documentation about them):
  permutation()
  plot.permutation()
  summary.permutation()
  bootstrap()
  plot.bootstrap()
  summary.bootstrao()
  output()
}
\author{
  Lotte Hindhede  & Cornelia Buus
}
