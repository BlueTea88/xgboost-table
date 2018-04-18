#'
#' Infer Lower Effects
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
inferLowerEffects <- (in.conditions, in.data, in.weights = NULL){
  # Set weights to 1 if unspecified
  if (is.null(in.weights)) in.weights <- rep(1, nrow(in.data))

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
      temp[, parameter := do.call(paste, c(.SD, collapse='*')), .SDcols=paste0('feature_',1:xint.dim)]
      return(temp)
    })
    
    # Combine conditions
    xint.conditions[[xint.dim]] <- rbindlist(c(list(xint.conditions[[xint.dim]]),temp.conditions), 
                                             use.names=TRUE, fill=TRUE)
  }
  out <- rbindlist(xint.conditions, use.names=TRUE, fill=TRUE)  # aggregate all conditions
  out[, value:=NULL]  # remove value - will be recalculated later
  setkey(out, NULL)
  out <- unique(out)  # remove duplicates
  
  
  # Extract fitted split points for each interaction and each feature
  xint.conditions <- split(in.conditions, by='parameter')  # split conditions table by parameter
  for (i in 1:length(int.fitted)){
    xint.parameter <-int.fitted_parameter[i]
    xint.nfeatures <- length(int.fitted[[i]])
    xint.splits <- list()
    if (xint.nfeatures > 1) xint.splits <- lapply(1:xint.nfeatures, function(j){
      splits <- xint.conditions[[xint.parameter]][, paste0(c('lower_bound_','upper_bound_'),j), with=FALSE]
      splits <- sort(unique(unlist(splits, use.names=FALSE)), na.last=NA)
      return(splits)
    })
    
    xint.conditions[[xint.parameter]][]
  }
  
  for (int in int.fitted){
    
    
    
  }
  
  # Extract splits - highest dimension to lowest
  for (i in nmax:1){
    
    
    
  }
  
  # Build comprehensive table of conditions
  # Lower order effects should have all splits in their corresponding higher order effects
  for (int in int.all){
    xint.mask <- sapply(int.index, function(x) all(int %in% x))  # rows that are related
    xint.rows <- in.conditions[xint.mask]
    
    xint.splits <- list()  # initialise splits
    for (x in int){
      xint.index <- sapply(int.index[int.mask], function(y) which(y == x))  # feature index per row
      xint.splits[[x]] <- unlist(lapply(1:length(xint.index), function(i){
        return(unlist(xint.rows[i, paste0(c('lower_bound_','upper_bound_'),xint.index[i]), with=F], use.names=F))
      }), use.names=F)
      
      )
      
      
      sapply(x.index)
    }
    
    
    int.conditions <- in.conditions[sapply(int.index, function(x) all(int %in% x))]
    int.index
    
  }
  
  in.conditions[,]


}
