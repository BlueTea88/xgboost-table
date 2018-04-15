#'
#' Tree Conditions
#'
#' Extract all the conditional representations of a model.
#'
#' @param in.tree Trees from a fitted XGBoost model
#' @param split.digits Significant digits for rounding split values
#'
#' @details
#' Search the trees of an XGBoost model and convert them into feature conditions and values.
#' Combine values across the same feature conditions.
#'
#' @return
#' Returns a list where each item represents one condition containing:
#' \itemize{
#'   \item \code{conditions}: list of features and their lower and upper bounds
#'   \item \code{value}: aggregate corresponding prediction value
#' }
#'
#' @export
treeConditions <- function(in.tree, split.digits = 11){
  # Initialise aggregate conditions list
  con.aggregate <- list()

  # Loop across each tree and combine conditions with aggregate list
  for (i in in.tree[, unique(Tree)]){
    con.current <- treeConditionsOne(in.tree[Tree == i])  # conditions from a single tree
    con.aggregate <- c(con.aggregate, con.current)  # append to aggregate list
  }

  # Store conditions in a data.table
  con.aggregate_tables <- lapply(con.aggregate, function(x){
    con.table <- list(value = x$value)
    if (length(x$conditions) > 0){
      for (i in 1:length(x$conditions)){
        names(x$conditions[[i]]) <- paste0(names(x$conditions[[i]]),'_',i)
        con.table <- c(con.table, x$conditions[[i]])  # value, feature_i, lower_bound_i, upper_bound_i
      }
    }
    con.table[['nfeatures']] <- length(x$conditions)
    return(as.data.table(con.table))
  })
  out <- rbindlist(con.aggregate_tables, use.names=TRUE, fill=TRUE)  # combine tables

  # Maximum number of features across all conditions
  con.max_features <- max(sapply(con.aggregate, function(x) length(x$conditions)))

  # Round split values
  for (i in 1:con.max_features){
    out[, (paste0('lower_bound_',i)) := signif(get(paste0('lower_bound_',i)), split.digits)]
    out[, (paste0('upper_bound_',i)) := signif(get(paste0('upper_bound_',i)), split.digits)]
  }

  # Aggregate values for the same condition
  by_columns <- 'nfeatures'
  for (i in 1:con.max_features) by_columns <- c(by_columns, paste0(c('feature_','lower_bound_','upper_bound_'),i))
  out <- out[, .(value = sum(value)), by=by_columns]
  return(out)
}

#
# Extract the conditions of a single tree
# Calls treeConditionsOneRecursive with memoisation layer
#
# @param single.tree Table of a single tree from an XGBoost model
treeConditionsOne <- function(single.tree){

  # Create memoisation function - treeConditionsOneRecursive
  #treeConditionsOneRecursive <- memoise::memoise(treeConditionsOneRecursive, envir=environment())

  # Get the tree index from input table
  tree.index <- single.tree[, unique(Tree)]
  if (length(tree.index) != 1) stop('Input single.tree should only contain data for one tree.')

  # Call recursive function to extract conditions
  tree.recursive <- treeConditionsOneRecursive(single.tree, paste0(tree.index,'-0'))

  # Process list of conditions - extract from list of lists and store them in a single list
  tree.conditions <- list()
  while(length(tree.recursive) > 0){
    # Check if conditions exist in current layer
    mask <- sapply(tree.recursive, function(x) !is.null(x$value))

    # If conditions exist, add them to output and remove them from current list
    if (any(mask)){
      tree.conditions <- c(tree.conditions, tree.recursive[mask])
      tree.recursive[mask] <- NULL

    # If no conditions, unlist and go to next layer
    } else {
      tree.recursive <- unlist(tree.recursive, use.names=FALSE, recursive=FALSE)
    }
  }

  # Remove NULLs
  tree.conditions <- tree.conditions[sapply(tree.conditions, function(x) !is.null(x))]

  # Remove other paths that wont be reached
  temp.remove <- numeric(0)
  for (i in 1:length(tree.conditions)){
    temp.test <- sapply(tree.conditions[[i]]$conditions, function(x){
      temp <- FALSE
      if (!is.na(x$lower_bound)) if(x$lower_bound >= x$upper_bound) temp <- TRUE
      return(temp)
    })
    if (any(temp.test)) temp.remove <- c(temp.remove, i)
  }
  tree.conditions[temp.test] <- NULL

  # Sort feature conditions in the order of their features names
  for (i in 1:length(tree.conditions)){
    tree.features <- sapply(tree.conditions[[i]]$conditions, function(x) x$feature)
    tree.conditions[[i]]$conditions <- tree.conditions[[i]]$conditions[order(tree.features)]
  }
  return(tree.conditions)
}

#
# Recursive function to extract the conditions of a single tree
#
# @param single.tree Table of a single tree from an XGBoost model
# @param current.id ID of the node to start processing
# @param current.list Current list of prior conditions
treeConditionsOneRecursive <- function(single.tree, current.id, current.list = NULL){
  if (is.null(current.list)) current.list <- list()  # initialise current list of conditions

  # Wrap up if a leaf is reached
  if (is.na(single.tree[ID == current.id, Split])){
    out <- list('conditions' = current.list,
                'value' = single.tree[ID == current.id, Quality])
    return(out)
  }

  # Process split nodes
  node.split <- single.tree[ID == current.id, Split]
  node.feature <- single.tree[ID == current.id, Feature]

  # Check if current feature already exists in prior conditions
  feature.exists <- FALSE
  if (length(current.list) > 0){
    feature.mask <- sapply(current.list, function(x) x$feature == node.feature)
    if (any(feature.mask)) feature.exists <- TRUE
  }

  # Branch out to Yes, No and Missing
  out <- lapply(c('Yes','No','Missing'), function(x){
    # If feature does not exist in prior conditions, add new list to conditions
    if (!feature.exists){
      if (x == 'Yes'){
        current.list[[length(current.list) + 1]] <- list('feature' = node.feature,
                                                         'lower_bound' = -Inf,
                                                         'upper_bound' = node.split)
      } else if (x == 'No'){
        current.list[[length(current.list) + 1]] <- list('feature' = node.feature,
                                                         'lower_bound' = node.split,
                                                         'upper_bound' = Inf)
      } else {
        current.list[[length(current.list) + 1]] <- list('feature' = node.feature,
                                                         'lower_bound' = as.numeric(NA),
                                                         'upper_bound' = as.numeric(NA))
      }

    # If feature exists in prior conditions, combine with new conditions
    } else {
      feature.conditions <- current.list[feature.mask][[1]]  # current conditions
      if (x == 'Yes'){
        feature.conditions[['upper_bound']] <- min(feature.conditions$upper_bound, node.split, na.rm=F)
        current.list[feature.mask][[1]] <- feature.conditions  # update conditions
      } else if (x == 'No'){
        feature.conditions[['lower_bound']] <- max(feature.conditions$lower_bound, node.split, na.rm=F)
        current.list[feature.mask][[1]] <- feature.conditions  # update conditions
      } else if (x == 'Missing'){
        # Only continue missing if previous values were missing
        if (!is.na(feature.conditions$lower_bound) | !is.na(feature.conditions$upper_bound)) return(NULL)
      }
    }

    # Send for recursive processing
    return(treeConditionsOneRecursive(single.tree, single.tree[ID == current.id, get(x)], current.list))
  })  # end lapply: Yes, No, Missing split
  return(out)
}
