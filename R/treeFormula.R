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
  
  for (i in 1:out[, max(nfeatures)]){
    temp.formula <- rep(as.character(NA), nrow(out))  # initialise formula string
    
    # Lower bound conditions
    temp.mask_lower <- out[, nfeatures >= i & !is.infinite(get(paste0('lower_bound_',i)))]
    temp.mask_lower[is.na(temp.mask_lower)] <- FALSE
    temp.values_lower <- out[, paste0('(`',get(paste0('feature_',i)),'` >= ',get(paste0('lower_bound_',i)),')')]
    temp.formula[temp.mask_lower] <- temp.values_lower[temp.mask_lower]
    
    # Upper bound conditions
    temp.mask_upper <- out[, nfeatures >= i & !is.infinite(get(paste0('upper_bound_',i)))]
    temp.mask_upper[is.na(temp.mask_upper)] <- FALSE
    temp.values_upper <- out[, paste0('(`',get(paste0('feature_',i)),'` < ',get(paste0('upper_bound_',i)),')')]
    temp.formula[temp.mask_upper & !temp.mask_lower] <- temp.values_upper[temp.mask_upper & !temp.mask_lower]
    temp.formula[temp.mask_upper & temp.mask_lower] <- paste0('(',temp.values_lower[temp.mask_upper & temp.mask_lower], ' * ',
                                                              temp.values_upper[temp.mask_upper & temp.mask_lower],')')
                                                             
    # Add missing conditions
    temp.mask_missing <- out[, nfeatures >= i & get(paste0('missing_',i))]
    temp.mask_missing[is.na(temp.mask_missing)] <- FALSE
    temp.values_missing <- out[, paste0('is.na(`',get(paste0('feature_',i)),'`)')]
    temp.formula[temp.mask_missing] <- paste0('(',temp.formula[temp.mask_missing],' | ',temp.values_missing[temp.mask_missing],')')
    
    temp.mask_no_missing <- out[, nfeatures >= i & !get(paste0('missing_',i))]
    temp.mask_no_missing[is.na(temp.mask_no_missing)] <- FALSE
    temp.values_no_missing <- out[, paste0('!is.na(`',get(paste0('feature_',i)),'`)')]
    temp.formula[temp.mask_no_missing] <- paste0('(',temp.formula[temp.mask_no_missing],' & ',temp.values_no_missing[temp.mask_no_missing],')')
    
    # Set formula and combine formula across features
    out[, (paste0('formula_',i)) := temp.formula]
    if (i == 1){
      out[, formula := temp.formula]
    } else {
      temp.mask <- out[, nfeatures >= i]
      temp.values <- paste(out[temp.mask, formula],'*',temp.formula[temp.mask])
      out[temp.mask, formula := temp.values]
    }
  }
  out[nfeatures == 0, formula := '1']  # formula for intercept
  return(out)
}