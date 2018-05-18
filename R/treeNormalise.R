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
  # Convert input data to data.table if necessary
  if (class(in.data)[1] == 'matrix') in.data <- as.data.table(in.data)
  
  # Set weights to 1 if unspecified
  if (is.null(in.weights)) in.weights <- rep(1, nrow(in.data))
  
  # Check length of weights is equal to number of rows
  if (length(in.weights) != nrow(in.data)) stop('Length of in.weights does not match nrow(in.data).')

  # Max interaction dimension
  nmax <- in.conditions[, max(nfeatures)]
  if (nmax == 0) stop('No features were fitted.')
  cols.main <- c('nfeatures','parameter',paste0('feature_',1:nmax),paste0('lower_bound_',1:nmax),paste0('upper_bound_',1:nmax),paste0('missing_',1:nmax))

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
    xint.conditions[[xint.dim]] <- in.conditions[nfeatures == xint.dim, cols.main, with=F]
    if (xint.dim == nmax) next  # no need to pad out conditions for the highest interaction dimension
    
    # Pad out lower order effects from higher order conditions
    temp.conditions <- lapply(1:(xint.dim+1), function(i){
      temp <- copy(xint.conditions[[xint.dim + 1]])  # copy table of higher order conditions
      temp[, paste0(c('feature_','lower_bound_','upper_bound_','missing_'), i) := NULL]
      leftover <- setdiff(1:(xint.dim+1), i)  # remaining features
      if (!identical(leftover,1:xint.dim)){  # rearrange the index of remaining features (1, 2, ...)
        setnames(temp, paste0('feature_',leftover), paste0('feature_',1:xint.dim))
        setnames(temp, paste0('lower_bound_',leftover), paste0('lower_bound_',1:xint.dim))
        setnames(temp, paste0('upper_bound_',leftover), paste0('upper_bound_',1:xint.dim))
        setnames(temp, paste0('missing_',leftover), paste0('missing_',1:xint.dim))
      }
      temp[, nfeatures := xint.dim]
      temp.features <- lapply(1:xint.dim, function(i) temp[[paste0('feature_',i)]])
      temp.features <- lapply(1:nrow(temp), function(i) sapply(temp.features, function(x) x[i]))
      temp.parameters <- sapply(temp.features, function(x) paste(x, collapse='*'))
      temp[, parameter := temp.parameters]
      return(temp)
    })
    
    # Combine conditions
    xint.conditions[[xint.dim]] <- rbindlist(c(list(xint.conditions[[xint.dim]]),temp.conditions), use.names=TRUE, fill=TRUE)
  }
  out <- rbindlist(xint.conditions, use.names=TRUE, fill=TRUE)  # aggregate all conditions
  
  # Remove duplicates
  setkey(out, NULL)
  out <- unique(out)  # remove duplicates
  
  # Attach formula columns
  out <- treeFormula(out)
  
  # Add intercept if required
  if (all(out[, nfeatures != 0])){
    temp <- data.table(nfeatures = 0, parameter = '(intercept)', formula = 1) 
    out <- rbindlist(list(out, temp), use.names=TRUE, fill=TRUE)
  }
  
  # Attach exposure
  exp.weights <- rep(0, nrow(out))
  exp.formula <- out[, formula]
  
  # Loop across conditions and attach exposure for each condition
  temp <- mclapply(1:nrow(out), function(i){
    xint.formula <- exp.formula[i]
    xint.mask <- with(in.data, eval(parse(text=xint.formula)))
    if (xint.formula == '1') xint.mask <- rep(1, nrow(in.data))
    temp.out <- sum(in.weights*xint.mask, na.rm=T)
    return(temp.out)
  })
  exp.weights <- unlist(temp, use.names=F)
  out[, exposure := exp.weights/sum(in.weights)]
  
  # Attach values from in.conditions
  out <- merge(out, in.conditions[, c(cols.main,'value'), with=F], by=cols.main, all.x=TRUE)
  out[is.na(value), value:=0]
  
  # Normalise table
  out[, (paste0('complement_',c(1:nmax))) := FALSE]  # initialise complement indicator
  all.formula <- out[['formula']]
  all.nfeatures <- out[['nfeatures']]
  
  for (xint.dim in nmax:1){
    # Process terms from highest to lowest order
    temp.terms_idx <- out[nfeatures == xint.dim & exposure != 0, which=TRUE] # extract terms to process
    temp.idx0 <- which(all.nfeatures == (xint.dim-1))  # index of lower order terms
    
    # Calculate normalisation adjustments
    norm.adjustments <- mclapply(temp.terms_idx, function(i){
      # Initialise output
      temp.out <- list(adj.index = rep(0, xint.dim), adj.values = rep(0, xint.dim), complements = list())
      
      # Extract current row values for faster processing
      current.row <- out[i]
      current.value <- current.row[['value']]
      current.exposure <- current.row[['exposure']]
      current.formula <- unlist(as.list(current.row)[paste0('formula_',1:xint.dim)])
      
      # Remove one feature to merge to lower order conditions
      for (k in 1:xint.dim){
        # Index of lower order term
        if (xint.dim > 1){
          temp.formula <- paste(current.formula[setdiff(1:xint.dim, k)], collapse = ' * ')
          temp.idx <- temp.idx0[all.formula[temp.idx0] == temp.formula]
          if (length(temp.idx) != 1) stop('Problem matching lower order terms.')
        } else {
          temp.idx <- out[nfeatures == 0, which=TRUE]
        }
        
        # Get exposure of lower order condition 
        temp.exposure <- out[['exposure']][temp.idx]
        
        # Adjustment
        temp.adj <- current.value * current.exposure/temp.exposure
        current.value <- current.value - temp.adj
        
        # Create row for complement
        temp.formula <- copy(current.formula)
        temp.formula[k] <- paste0('(!',current.formula[k],')')
        temp.complement <- copy(current.row)
        temp.complement[['value']] <- -temp.adj
        temp.complement[['exposure']] <- temp.exposure - current.exposure
        temp.complement[[paste0('complement_',k)]] <- TRUE
        temp.complement[[paste0('formula_',k)]] <- temp.formula[k]
        temp.complement[['formula']] <- paste(temp.formula, collapse = ' * ')
        
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
    norm.adj <- norm.adj[, .(adjustment = sum(adjustment)), by='index']
    out[['value']][norm.adj[, index]] <- out[['value']][norm.adj[, index]] + norm.adj[, adjustment]
    
    # Add complement rows
    norm.complements <- rbindlist(lapply(norm.adjustments, function(x) x$complements), use.names=TRUE)
    out <- rbindlist(list(out, norm.complements), use.names=TRUE)
  }  # end normalisation loop
  
  # Return output table
  return(out)
}
