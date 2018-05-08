#'
#' Tree Lower Effects
#'
#' Infer lower order effects from extracted tree conditions.
#'
#' @param in.conditions feature conditions of trees, extracted using \code{treeConditions()}
#' @param in.data training data used to fit the model in the form of a \code{data.table}
#' @param in.weights vector of weights corresponding to each observation of \code{in.data}, defaults to a vector of 1s
#'
#' @details
#' Gives an idea of how the model would look like if lower order effects (eg. main effects) were fitted along with the
#'   interactions implied by trees.
#' Contributions to predictions are split between lower and higher order effects by making the weighted absolute magnitude
#'   of lower effects as large as possible.
#' The distribution of features is obtained \code{in.data} and \code{in.weights}.
#'
#' @export
treeLowerEffects <- (in.conditions, in.data, in.weights = NULL){
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
  int.conditions <- rbindlist(xint.conditions, use.names=TRUE, fill=TRUE)  # aggregate all conditions
  int.conditions[, value:=NULL]  # remove value - will be recalculated later
  
  # Build a comprehensive list of splits at the parameter level
  temp.dt <- list()
  length(temp.dt) <- length(int.all)
  
  for (i in 1:length(int.all)){
    temp.subset <- int.conditions[parameter == int.all_parameters[[i]]]
    
    for (j in 1:length(int.all[[i]])){
      temp.splits <- unique(c(temp.subset[[paste0('lower_bound_',j)]],temp.subset[[paste0('upper_bound_',j)]]))
      temp.splits <- sort(unique(c(-Inf,temp.splits,Inf)), na.last=NA)  # removes NA as well
      temp.lower_bound <- c(temp.splits[1:(length(temp.splits)-1)], NA)
      temp.upper_bound <- c(temp.splits[2:length(temp.splits)], NA)
      temp.table <- data.table(int.all[[i]][j], temp.lower_bound, temp.upper_bound, 1)
      colnames(temp.table) <- c(paste0(c('feature_','lower_bound_','upper_bound_'),j),'key')
      if (j == 1) temp.dt[[i]] <- copy(temp.table) else temp.dt[[i]] <- merge(temp.dt[[i]], temp.table, by='key', allow.cartesian=TRUE)
    }
    temp.dt[[i]][, `:=`(nfeatures = length(int.all[[i]]), parameter = int.all_parameters[[i]], key = NULL)]
  }
  out <- rbindlist(temp.dt, use.names=TRUE, fill=TRUE)
  
  # Apply values to new conditions
  temp.values <- rep(0, nrow(out))  # initialise values to 0
  
  for (i in 1:nrow(in.conditions)){
    temp.nfeatures <- in.conditions[['nfeatures']][i]
    temp.value <- in.conditions[['value']][i]
    temp.formula <- paste0('(parameter == "', in.conditions[['parameter']][i],'")')
    
    # TODO: add support for intercept
    for (j in 1:temp.nfeatures){
      temp.formula <- paste0(temp.formula,' * (lower_bound_',j,' >= ',in.conditions[[paste0('lower_bound_',j)]][i],')')
      temp.formula <- paste0(temp.formula,' * (upper_bound_',j,' <= ',in.conditions[[paste0('upper_bound_',j)]][i],')')
    }
    
    # Identify new conditions that are affected and apply values
    idx <- with(out, eval(parse(text=temp.formula)))
    temp.values[idx] <- temp.values[idx] + temp.value
  }
  
  # Create formula
  for (i in 1:nmax){
    out[nfeatures >= i & is.na(get(paste0('lower_bound_',i))),
        (paste0('formula_lower_',i)) := paste0('(is.na(',get(paste0('feature_',i)),'))')]
    out[nfeatures >= i & !is.na(get(paste0('lower_bound_',i))) & !is.infinite(get(paste0('lower_bound_',i))),
        (paste0('formula_lower_',i)) := paste0('(',get(paste0('feature_',i)),' >= ',get(paste0('lower_bound_',i)),')')]
    out[nfeatures >= i & !is.na(get(paste0('upper_bound_',i))) & !is.infinite(get(paste0('upper_bound_',i))),
        (paste0('formula_upper_',i)) := paste0('(',get(paste0('feature_',i)),' < ',get(paste0('upper_bound_',i)),')')]
  }
  int.formula <- c(lapply(1:nmax, function(i) out[[paste0('formula_lower_',i)]]),
                   lapply(1:nmax, function(i) out[[paste0('formula_upper_',i)]]))
  out[, c(paste0('formula_lower_',1:nmax),paste0('formula_upper_',1:nmax)) := NULL]
  int.formula <- lapply(1:nrow(out), function(i) sapply(int.formula, function(x) x[i]))
  int.formula <- lapply(int.formula, function(x) x[!is.na(x)])
  out[, formula := sapply(int.formula, paste, collapse=' * ')]
  
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
  
  # Normalise table
  

}
