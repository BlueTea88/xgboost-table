#'
#' Tree Score
#'
#' Score using formulas and values from an input conditions table
#'
#' @param in.conditions table containing a formula column defining 
#' @param in.data input data to score, in the form of a \code{data.table}
#'
#' @details
#' Calculate scores from a conditions table.  Useful for reconciling with the original XGBoost model.
#'
#' @export
treeScores <- function(in.conditions, in.data){
  # Convert input data to data.table if necessary
  if (class(in.data)[1] == 'matrix') in.data <- as.data.table(in.data)

  # Initialise output values and processing vectors
  out <- rep(0, nrow(in.data))
  i.formula <- in.conditions[['formula']]
  i.value <- in.conditions[['value']]
  
  # Return 0 scores if no conditions to process
  if (nrow(in.conditions) == 0) return(out)
  
  # Loop across conditions and add scores
  for (i in 1:nrow(in.conditions)){
    i.mask <- as.numeric(with(in.data, eval(parse(text=i.formula[i]))))
    if (i.formula[i] == '1') i.mask <- rep(1, nrow(in.data))
    i.mask[is.na(i.mask)] <- 0
    out <- out + i.mask * i.value[i]
  }
  if (any(is.na(out))) stop('NA values in output.')
  return(out)
}