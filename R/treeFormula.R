#'
#' Tree Formula
#'
#' Attach formula columns from an input condition table.
#'
#' @param in.conditions input table of conditions
#'
#' @return
#' A new data.table with formula columns attached to the input.
#' \itemize{
#'   \item \code{formula}: overall formula in the form of a string
#'   \item \code{formula_i}: string formula for the i-th feature
#' }
#'
#' @export
treeFormula <- function(in.conditions){
  # Create formula
  out <- copy(in.conditions)
  all.nfeatures <- out[['nfeatures']]
  
  for (i in 1:out[, max(nfeatures)]){
    i.formula <- rep(as.character(NA), nrow(out))  # initialise formula string
    i.mask <- (all.nfeatures >= i)
    i.feature <- out[[paste0('feature_',i)]][i.mask]
    i.lower <- out[[paste0('lower_bound_',i)]][i.mask]
    i.upper <- out[[paste0('upper_bound_',i)]][i.mask]
    i.missing <- out[[paste0('missing_',i)]][i.mask]
    
    # Lower bound is infinite (x < upper_bound)
    temp.mask <- is.infinite(i.lower)
    temp.values <- paste0('(`',i.feature,'` < ',i.upper,')')
    i.formula[i.mask][temp.mask] <- temp.values[temp.mask]
    
    # Upper bound is infinite (x >= lower_bound)
    temp.mask <- is.infinite(i.upper)
    temp.values <- paste0('(`',i.feature,'` >= ',i.lower,')')
    i.formula[i.mask][temp.mask] <- temp.values[temp.mask]
    
    # Both upper and lower bound are non-infinite and lower < upper
    temp.mask <- (!is.infinite(i.lower) & !is.infinite(i.upper) & i.lower < i.upper)
    temp.values <- paste0('(`',i.feature,'` >= ',i.lower,' & `',i.feature,'` < ',i.upper,')')
    i.formula[i.mask][temp.mask] <- temp.values[temp.mask]
    
    # Both upper and lower bound are non-infinite and upper <= lower (complement range)
    temp.mask <- (!is.infinite(i.lower) & !is.infinite(i.upper) & i.upper <= i.lower)
    temp.values <- paste0('(`',i.feature,'` < ',i.upper,' | `',i.feature,'` >= ',i.lower,')')
    i.formula[i.mask][temp.mask] <- temp.values[temp.mask]
    
    # Add missing conditions (TRUE)
    temp.mask <- i.missing
    temp.values <- paste0('is.na(`',i.feature,'`)')
    i.formula[i.mask][temp.mask] <- paste0('(',i.formula[i.mask][temp.mask],' | ',temp.values[temp.mask],')')
    
    # Add missing conditions (FALSE)
    temp.mask <- !i.missing
    temp.values <- paste0('!is.na(`',i.feature,'`)')
    i.formula[i.mask][temp.mask] <- paste0('(',i.formula[i.mask][temp.mask],' & ',temp.values[temp.mask],')')
    
    # Set formula and combine formula across features
    out[, (paste0('formula_',i)) := i.formula]
    if (i == 1){
      out[, formula := i.formula]
    } else {
      temp.values <- paste(out[i.mask, formula],'*',i.formula[i.mask])
      out[i.mask, formula := temp.values]
    }
  }
  
  # Formula for intercept
  if (any(out[['nfeatures']] == 0)) out[nfeatures == 0, formula := '1']
  return(out)
}