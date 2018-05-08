#'
#' Tree Normalise
#'
#' Normalise tree conditions
#'
#' @param in.conditions feature conditions of trees, extracted using \code{treeConditions()}
#' @param in.data training data used to fit the model in the form of a \code{data.table}
#' @param in.weights vector of weights corresponding to each observation of \code{in.data}, defaults to a vector of 1s
#'
#' @details
#' Normalise tree conditions by padding out lower order effects and setting the values of lower order conditions based
#'   on the weighted mean.  The distribution of features is obtained from \code{in.data} and \code{in.weights}.
#'
#' @export
treeNormalise <- function(in.conditions, in.data, in.weights = NULL){
  # Set weights to 1 if unspecified
  if (is.null(in.weights)) in.weights <- rep(1, nrow(in.data))
  
  # Check length of weights is equal to number of rows
  if (length(in.weights) != nrow(in.data)) stop('Length of in.weights does not match nrow(in.data).')

  # Max interaction dimension
  nmax <- in.conditions[, max(nfeatures)]
  if (nmax == 0) stop('No features were fitted.')

  # Fitted interactions - get a list of vectors with each item containing the feature names of one interaction
  int.index <- lapply(1:nrow(in.conditions), function(i){
    temp <- sapply(1:nmax, function(j) in.conditions[[paste0('feature_',j)]][i])
    return(sort(temp[!is.na(temp)]))
  })
  int.fitted <- unique(int.index)
  int.fitted_parameters <- sapply(int.fitted, paste, collapse='*')
  int.features <- sort(unique(unlist(int.fitted, use.names=F)))

  # Pad out lower order interactions
  int.all <- int.fitted
  if (nmax > 1) for (i in (nmax-1):1){
    int.lower <- lapply(int.fitted, function(x) ifelse(length(x) > i, combn(x, i, simplify=FALSE), 0))
    int.lower <- unlist(int.lower, use.names=FALSE, recursive=FALSE)
    int.lower <- int.lower[sapply(int.lower, function(x) !all(x == 0))]  # remove non-interactions
    int.lower <- lapply(int.lower, function(x) sort(x))  # sort by feature name
    int.lower <- unique(int.lower)  # remove duplicates
    int.all <- unique(c(int.lower,int.all))  # combine with aggregate list
  }
  int.all_parameters <- sapply(int.all, paste, collapse='*')
  
  # Pad out feature conditions, loop from highest dimension to lowest
  xint.conditions <- list()  # list to store interaction conditions
  length(xint.conditions) <- nmax
  
  for (xint.dim in nmax:1){
    # Copy fitted model conditions
    xint.conditions[[xint.dim]] <- in.conditions[nfeatures == xint.dim]
    if (xint.dim == nmax) next  # no need to pad out conditions for the highest interaction dimension
    
    # Pad out lower order effects from higher order conditions
    temp.conditions <- lapply(1:(xint.dim+1), function(i){
      temp <- copy(xint.conditions[[xint.dim + 1]])  # copy table of higher order conditions
      temp[, paste0(c('feature_','lower_bound_','upper_bound_'), i) := NULL]
      leftover <- setdiff(1:(xint.dim+1), i)  # remaining features
      if (!identical(leftover,1:xint.dim)){  # rearrange the index of remaining features (1, 2, ...)
        setnames(temp, paste0('feature_',leftover), paste0('feature_',1:xint.dim))
        setnames(temp, paste0('lower_bound_',leftover), paste0('lower_bound_',1:xint.dim))
        setnames(temp, paste0('upper_bound_',leftover), paste0('upper_bound_',1:xint.dim))
      }
      temp[, nfeatures := xint.dim]
      temp.features <- lapply(1:xint.dim, function(i) temp[[paste0('feature_',i)]])
      temp.features <- lapply(1:nrow(temp), function(i) sapply(temp.features, function(x) x[i]))
      temp.parameters <- sapply(temp.features, function(x) paste(x, collapse='*'))
      temp[, parameter := temp.parameters]
      return(temp)
    })
    
    # Combine conditions
    xint.conditions[[xint.dim]] <- rbindlist(c(list(xint.conditions[[xint.dim]]),temp.conditions), 
                                             use.names=TRUE, fill=TRUE)
  }
  out <- rbindlist(xint.conditions, use.names=TRUE, fill=TRUE)  # aggregate all conditions
  out[, value:=NULL]  # remove value - will be merged back later
  
  # Remove duplicates
  setkey(out, NULL)
  out <- unique(out)  # remove duplicates	   out <- unique(out)  # remove duplicates
  
  # Create formula
  for (i in 1:nmax){
    temp.formula <- rep(as.character(NA), nrow(out))  # initialise formula string
    
    # NA conditions
    temp.mask <- out[, nfeatures >= i & is.na(get(paste0('lower_bound_',i)))]
    temp.values <- out[, paste0('(is.na(',get(paste0('feature_',i)),'))')]
    temp.formula[temp.mask] <- temp.values[temp.mask]
    
    # Lower bound conditions
    temp.mask_lower <- out[, nfeatures >= i & !is.na(get(paste0('lower_bound_',i))) & !is.infinite(get(paste0('lower_bound_',i)))]
    temp.values_lower <- out[, paste0('(',get(paste0('feature_',i)),' >= ',get(paste0('lower_bound_',i)),')')]
    temp.formula[temp.mask_lower] <- temp.values_lower[temp.mask_lower]
    
    # Upper bound conditions
    temp.mask_upper <- out[, nfeatures >= i & !is.na(get(paste0('upper_bound_',i))) & !is.infinite(get(paste0('upper_bound_',i)))]
    temp.values_upper <- out[, paste0('(',get(paste0('feature_',i)),' < ',get(paste0('upper_bound_',i)),')')]
    temp.formula[temp.mask_upper & !temp.mask_lower] <- temp.values_upper[temp.mask_upper & !temp.mask_lower]
    temp.formula[temp.mask_upper & temp.mask_lower] <- paste(temp.values_lower[temp.mask_upper & temp.mask_lower], '*',
                                                             temp.values_upper[temp.mask_upper & temp.mask_lower])
    
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
  
  # Add intercept if required
  if (all(out[, nfeatures != 0])){
    temp <- data.table(nfeatures = 0, parameter = '(intercept)', formula = 1, value = 0) 
    out <- rbindlist(list(out, temp), use.names=TRUE, fill=TRUE)
  }
  
  # Attach exposure
  exp.weights <- rep(0, nrow(out))
  exp.formula <- out[, formula]
  exp.index <- 1:nrow(out)
  
  # Check missing conditions and skip processing if there are no missing values (automatically set exposure to 0)
  exp.skip_index <- numeric(0)
  for (x in int.features){
    temp <- with(in.data, eval(parse(text=paste0('any(is.na(',x,'))'))))
    if (!temp) exp.skip_index <- unique(c(exp.skip_index,grep(paste0('is.na(',x,')'), exp.formula, fixed=T))) 
  }
  exp.index <- base::setdiff(exp.index, exp.skip_index)
  
  # Loop across conditions and attach exposure for each condition
  for (i in exp.index){
    xint.formula <- exp.formula[i]
    xint.mask <- with(in.data, eval(parse(text=xint.formula)))
    exp.weights[i] <- sum(in.weights*xint.mask, na.rm=T)
  }
  out[, exposure := exp.weights/sum(in.weights)]
  
  # Attach values from in.conditions
  out <- merge(out, in.conditions, by=colnames(temp.merge_cols), all.x=TRUE)
  
  # Normalise table
  out[, (paste0('complement_',c(1:nmax))) := FALSE]  # initialise complement indicator
  all.formula <- out[, formula]
  
  for (xint.dim in nmax:1){
    # Process terms from highest to lowest order
    temp.terms_idx <- out[nfeatures == xint.dim & exposure != 0, which=TRUE] # extract terms to process
    
    # Calculate normalisation adjustments
    norm.adjustments <- lapply(temp.terms_idx, function(i){
      # Initialise output
      temp.out <- list(adj.index = rep(0, xint.dim), adj.values = rep(0, xint.dim), complements = list())
      
      # Current coefficient value and exposure
      current.value <- out[i, value]
      current.exposure <- out[i, exposure]
      
      # Remove one feature to merge to lower order conditions
      for (k in 1:xint.dim){
        # Index of lower order term
        if (xint.dim > 1){
          temp.formula <- paste(unlist(out[i, paste0('formula_',setdiff(1:i, k)), with=F]), collapse=' * ')
          temp.idx <- which(all.formula == temp.formula)
          if (length(temp.idx) != 1) stop('Problem matching lower order terms.')
        } else {
          temp.idx <- out[nfeatures == 0, which=TRUE] 
        }
        
        # Get exposure of lower order condition 
        temp.exposure <- out[temp.idx, exposure]
        
        # Adjustment
        temp.adj <- current.value * current.exposure/temp.exposure
        current.value <- current.value - temp.adj
        
        # Create row for complement
        temp.complement <- copy(out[i])
        temp.complement[, `:=`(value = -temp.adj, exposure = temp.exposure - current.exposure)]
        temp.complement[, (paste0('complement_',k)) := TRUE]
        temp.complement[, (paste0('formula_',k)) := paste0('!',get(paste0('formula_',k)))]
        temp.complement[, formula := do.call(.SD, paste, collapse = ' * '), .SDcols = paste0('formula_',1:xint.dim)]
        
        # Update output list
        temp.out[['adj.index']][k] <- temp.idx
        temp.out[['adj.values']][k] <- temp.adj
        temp.out[['complements']][[length(temp.out$complements) + 1]] <- temp.complement
      }
      temp.out[['complements']] <- rbindlist(temp.out$complements, use.names=TRUE)
      temp.out[['value']] <- current.value
      return(temp.out)
    })
    
    # Update current values
    norm.values <- sapply(norm.adjustments, function(x) x$value)
    out[['value']][temp.terms_idx] <- norm.values
    
    # Update lower order conditions with adjustments
    norm.adj <- lapply(norm.adjustments, function(x) data.table(index=x$adj.index, adjustment=x$adj.value))
    norm.adj <- rbindlist(norm.adj, use.names=TRUE)
    norm.adj <- norm.adj[, .(adjustment = sum(adjustment)), by=index]
    out[['value']][norm.adj[, index]] <- out[['value']][norm.adj[, index]] + norm.adj[, adjustment]
    
    # Add complement rows
    norm.complements <- rbindlist(lapply(norm.adjustments, function(x) x$complements), use.names=TRUE)
    out <- rbindlist(list(out, norm.complements), use.names=TRUE)
  }  # end normalisation loop
  
  # Return output table
  return(out)
}
