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
  
  # Split data for parallel processing
  nsplits <- min(getOption('mc.cores', 1L), nrow(in.data))
  splits.idx <- sample(1:nsplits, nrow(in.data), replace=TRUE)
  splits.data <- split(in.data, splits.idx)
  splits.weight <- lapply(1:nsplits, function(i) in.weights[splits.idx == i])
  
  # Process data in parallel
  splits.scores <- mclapply(1:nsplits, function(i){
    # Loop across each parameter and calculate the weighted average scores
    p.scores <- sapply(params, function(p){
      temp.con <- in.conditions[parameter == p]
      temp.scores <- treeScores(temp.con, splits.data[[i]])
      temp.scores_wabs <- sum(abs(temp.scores)*splits.weight[[i]])
      return(temp.scores_wabs)
    })
    return(p.scores)
  })
  
  # Sum across parallel runs
  params.scores <- Reduce('+', splits.scores)
  
  # Get number of features for each parameter
  params.nfeatures <- sapply(params, function(x) all.nfeatures[all.params == x][1])
  
  # Return table
  out <- data.table(parameter = params, nfeatures = params.nfeatures, importance = params.scores)
  setorderv(out, 'importance', -1)
  return(out)
}