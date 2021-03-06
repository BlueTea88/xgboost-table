% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/treeNormalise.R
\name{treeNormalise}
\alias{treeNormalise}
\title{Tree Normalise}
\usage{
treeNormalise(in.conditions, in.data, in.weights = NULL)
}
\arguments{
\item{in.conditions}{feature conditions of trees, extracted using \code{treeConditions()}}

\item{in.data}{training data used to fit the model in the form of a \code{data.table}}

\item{in.weights}{vector of weights corresponding to each observation of \code{in.data}, defaults to a vector of 1s}
}
\description{
Normalise tree conditions
}
\details{
Normalise tree conditions by padding out lower order effects and setting the values of lower order conditions based
  on the weighted mean.  The distribution of features is obtained from \code{in.data} and \code{in.weights}.
}
