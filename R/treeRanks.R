#'
#' Tree Ranks
#'
#' Rank the importance of each parameter on an input dataset
#'
#' @param in.conditions table of normalised tree conditions
#' @param in.data input dataset to calculate ranks
#' @param in.weights vector of weights corresponding to each observation of \code{in.data}, defaults to a vector of 1s
#'
#' @return
#' A table of parameters and their associated score.
#'
#' @export
treeRanks <- function(in.conditions, in.data, in.weights = NULL){
  # Convert input data to data.table if necessary
  if (class(in.data)[1] == 'matrix') in.data <- as.data.table(in.data)
  
  # Set weights to 1 if unspecified
  if (is.null(in.weights)) in.weights <- rep(1, nrow(in.data))

  # Extract details
  all.params <- in.conditions[['parameter']]
  all.nfeatures <- in.conditions[['nfeatures']]

  # Get a list of parameters
  params <- unique(all.params)
  
  # Loop across each parameter and calculate the weighted average scores
  params.scores <- mclapply(params, function(x){
    temp.con <- in.conditions[parameter == x]
    temp.scores <- treeScores(temp.con, in.data)
    temp.scores_wabs <- sum(abs(temp.scores)*in.weights)
    return(temp.scores_wabs)
  }, mc.preschedule = FALSE)
  params.scores <- unlist(params.scores, use.names=F)
  
  # Get number of features for each parameter
  params.nfeatures <- sapply(params, function(x) all.nfeatures[all.params == x][1])
  
  # Return table
  out <- data.table(parameter = params, nfeatures = params.nfeatures, importance = params.scores)
  setorderv(out, 'importance', -1)
  return(out)
}