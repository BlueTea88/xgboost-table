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
  
  # Initialise output
  out <- copy(in.conditions)
  out[, exposure := as.numeric(NA)]
  out[, new:=TRUE]  # indicates a new model term
  
  # Add intercept if required
  if (all(out[['nfeatures']] != 0)){
    temp <- data.table(nfeatures = 0, parameter = '(intercept)', formula = 1, value = 0, exposure = 1, new = FALSE)
    out <- rbindlist(list(out, temp), use.names=TRUE, fill=TRUE)
  }
  
  # Process terms from highest to lowest interaction order
  for (idim in nmax:1){  # interaction parameter order
  for (jdim in idim:1){  # j-th feature of the term
    # Prepare new terms
    if (out[, any(get('new'))]){
      new_terms <- out[get('new')]
      new_nmax <- new_terms[, max(nfeatures)]
      existing_formula <- out[['formula']]  # all existing formula
      
      # Pad out lower order conditions
      if (new_nmax > 1) for (xdim in new_nmax:2){
        pad_new <- new_terms[nfeatures == xdim]
        pad_new[, `:=`(value = 0, exposure = as.numeric(NA))]  # set value and exposure for new padded out terms
        if (nrow(pad_new) == 0) next
        
        # Remove each feature from the terms once to pad out lower order conditions
        pad_current <- lapply(1:(xdim), function(i){
          temp <- copy(pad_new)
          temp[, paste0(c('feature_','lower_bound_','upper_bound_','missing_','formula_'), i) := NULL]
          leftover <- setdiff(1:xdim, i)  # remaining features
          if (!identical(leftover,1:(xdim - 1))){  # rearrange the index of remaining features (1, 2, ...)
            setnames(temp, paste0('feature_',leftover), paste0('feature_',1:(xdim - 1)))
            setnames(temp, paste0('lower_bound_',leftover), paste0('lower_bound_',1:(xdim - 1)))
            setnames(temp, paste0('upper_bound_',leftover), paste0('upper_bound_',1:(xdim - 1)))
            setnames(temp, paste0('missing_',leftover), paste0('missing_',1:(xdim - 1)))
          }
          temp[, nfeatures := (xdim-1)]
          temp.features <- lapply(1:(xdim-1), function(i) temp[[paste0('feature_',i)]])
          temp.features <- lapply(1:nrow(temp), function(i) sapply(temp.features, function(x) x[i]))
          temp.parameters <- sapply(temp.features, function(x) paste(x, collapse='*'))
          temp[, parameter := temp.parameters]  # set parameter
          temp <- treeFormula(temp)  # set formula
          return(temp)
        })
        
        # Combine and remove duplicates
        pad_current <- rbindlist(pad_current, use.names=TRUE, fill=TRUE)
        pad_duplicates <- duplicated(pad_current[['formula']])
        pad_current <- pad_current[!pad_duplicates]
        
        # Remove conditions that already exist
        idx <- which(!pad_current[['formula']] %in% existing_formula)
        pad_current <- pad_current[idx]
        
        # Append to new terms
        if (nrow(pad_current) > 0){
          new_terms <- rbindlist(list(new_terms, pad_current), use.names=TRUE, fill=TRUE)
          out <- rbindlist(list(out, pad_current), use.names=TRUE, fill=TRUE)
        }
      }  # end padding out lower order conditions
      
      # Attach exposure to all new terms that have unprocessed (NA) exposure
      if (any(is.na(new_terms[['exposure']]))){
        # Gather index and formula of rows to process
        exp_idx <- which(is.na(new_terms[['exposure']]))
        exp_formula <- new_terms[['formula']][exp_idx]
        
        # Loop across each formula to process
        temp <- mclapply(1:length(exp_formula), function(i){
          xmask <- with(in.data, eval(parse(text=exp_formula[i])))
          if (exp_formula[i] == '1') xmask <- rep(1, nrow(in.data))  # intercept formula
          xout <- sum(in.weights * xmask, na.rm=T)
          return(xout)
        })
        
        # Update exposure
        exp_weights <- unlist(temp, use.names=F)
        new_terms[['exposure']][exp_idx] <- exp_weights/sum(in.weights)
      }  # end attach exposure
      
      # Append processed new terms
      new_terms[, new := FALSE]  # update new term indicator
      out <- rbindlist(list(out[!get('new')], new_terms), use.names=TRUE, fill=TRUE)
    }  # end new terms processing
    
    # Gather data to normalise table
    all.formula <- out[['formula']]
    all.nfeatures <- out[['nfeatures']]
    terms_idx <- out[nfeatures == idim & round(exposure,15) > 0, which=TRUE]  # index of terms to process
    terms_idx0 <- which(all.nfeatures == (idim - 1))  # index of all lower order terms
    terms_idx1 <- which(all.nfeatures == idim)  # index of all current order terms
    
    # Calculate normalisation adjustments
    norm.adjustments <- mclapply(terms_idx, function(i){
      # Initialise output
      xout <- list()
      
      # Extract current row values for faster processing
      current.row <- out[i]
      current.value <- current.row[['value']]
      current.exposure <- current.row[['exposure']]
      current.formula <- unlist(as.list(current.row)[paste0('formula_',1:idim)])
      
      # Remove j-th feature to merge to lower order conditions
      if (idim > 1){
        temp.formula <- paste(current.formula[setdiff(1:idim, jdim)], collapse = ' * ')
        xout[['adj.index']] <- terms_idx0[all.formula[terms_idx0] == temp.formula]
        if (length(xout$adj.index) != 1) stop('Problem matching lower order terms.')
      } else {
        xout[['adj.index']] <- out[nfeatures == 0, which=TRUE]
      }
      
      # Get exposure of lower order condition
      temp.exposure <- out[['exposure']][xout$adj.index]
      
      # Adjustment
      xout[['adj']] <- current.value * current.exposure/temp.exposure
      xout[['value']] <- current.value - xout$adj  # new value
      
      # Create row for complement
      temp.complement <- copy(current.row)
      temp.complement[['new']] <- TRUE
      temp.complement[['value']] <- -xout$adj
      temp.complement[['exposure']] <- temp.exposure - current.exposure
      temp.complement[[paste0('lower_bound_',jdim)]] <- current.row[[paste0('upper_bound_',jdim)]]
      temp.complement[[paste0('upper_bound_',jdim)]] <- current.row[[paste0('lower_bound_',jdim)]]
      temp.complement[[paste0('missing_',jdim)]] <- !current.row[[paste0('missing_',jdim)]]
      temp.complement <- treeFormula(temp.complement)
      
      # Check if complement condition already exists - just need to add adjustment to existing row
      temp.complement_idx <- terms_idx1[all.formula[terms_idx1] == temp.complement[['formula']]]
      
      if (length(temp.complement_idx) == 0){
        # If complement condition does not exist, need to return a new row to add
        xout[['complement']] <- temp.complement
      } else if (length(temp.complement_idx) == 1){
        # If complement condition already exists, return index of existing row and required adjustment
        xout[['adj.index']] <- c(xout$adj.index, temp.complement_idx)
        xout[['adj']] <- c(xout$adj, -xout$adj)
      } else {
        # If more than one match, there is a duplicate error in the table
        stop('Duplicate complement conditions already exist.')
      }
      return(xout)
    })  # end calculate normalisation adjustments
    
    # Update current values
    norm.values <- sapply(norm.adjustments, function(x) x$value)
    out[['value']][terms_idx] <- norm.values
    
    # Update other conditions with adjustments
    norm.adj <- lapply(norm.adjustments, function(x) data.table(index=x$adj.index, adjustment=x$adj))
    norm.adj <- rbindlist(norm.adj, use.names=TRUE)
    norm.adj <- norm.adj[, .(adjustment = sum(adjustment)), by='index']
    out[['value']][norm.adj[['index']]] <- out[['value']][norm.adj[['index']]] + norm.adj[['adjustment']]
    
    # Add new complement rows if required
    norm.complements <- lapply(norm.adjustments, function(x) x$complement)
    norm.complements <- norm.complements[sapply(norm.complements, function(x) !is.null(x))]  # remove NULL entries
    if (length(norm.complements) > 0){
      out <- rbindlist(c(list(out),norm.complements), use.names=TRUE, fill=TRUE)
    }
  }}
  
  # Return output table
  setorderv(out, c('nfeatures','parameter','formula'))
  return(out)
}
