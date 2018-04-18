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
  # Prepare tree - convert Yes, No and Missing to integers
  tree <- copy(in.tree)
  if (tree[, class(Yes)] == 'character') tree[, Yes := as.numeric(gsub('(^.*\\-)(.*$)','\\2',Yes))]
  if (tree[, class(No)] == 'character') tree[, No := as.numeric(gsub('(^.*\\-)(.*$)','\\2',No))]
  if (tree[, class(Missing)] == 'character') tree[, Missing := as.numeric(gsub('(^.*\\-)(.*$)','\\2',Missing))]
  trees_index <- tree[, unique(Tree)]

  # Initialise aggregate conditions list
  con.aggregate <- list()
  length(con.aggregate) <- length(trees_index)

  # Loop across each tree and combine conditions with aggregate list
  for (i in 1:length(trees_index)) con.aggregate[[i]] <- treeConditionsOne(tree[Tree == trees_index[i]])
  con.aggregate <- unlist(con.aggregate, recursive=F, use.names=F)

  # Store conditions in a data.table
  con.aggregate_tables <- lapply(con.aggregate, function(x){
    con.table <- list(value = x$value)
    con.table[['parameter']] <- paste(sapply(x$conditions, function(x) x$feature), collapse='*')
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
  by_columns <- c('nfeatures','parameter')
  for (i in 1:con.max_features) by_columns <- c(by_columns, paste0(c('feature_','lower_bound_','upper_bound_'),i))
  out <- out[, .(value = sum(value)), by=by_columns]
  return(out)
}


#
# Extract the conditions of a single tree
#
# @param single.tree Table of a single tree from an XGBoost model
#
treeConditionsOne <- function(single.tree){

  # Sort tree by node for faster processing
  setorderv(single.tree, 'Node')

  # Start from node 0 and loop until there are no nodes to process
  tree.conditions <- list()  # list to store processed conditions
  tree.process <- list(list('current.node' = 0,  # current node to process
                            'current.conditions' = list()))  # list of prior conditions

  while(length(tree.process) > 0){
    tree.next_process <- list()  # staging list to store next nodes to process

    for (i in 1:length(tree.process)){  # loop across all the current nodes to process
      # Gather current node details
      node.id <- tree.process[[i]]$current.node
      node.conditions <- tree.process[[i]]$current.conditions
      node.split <- single.tree[['Split']][node.id + 1]
      node.feature <- single.tree[['Feature']][node.id + 1]

      # Wrap up if a leaf node is reached
      if (is.na(node.split)){
        tree.conditions[[length(tree.conditions) + 1]] <- list('conditions' = node.conditions, 'value' = single.tree[['Quality']][node.id + 1])
        next  # no further processing required
      }

      # Check if feature exists in prior conditions
      feature.exists <- FALSE
      if (length(node.conditions) > 0){
        feature.mask <- sapply(node.conditions, function(x) x$feature == node.feature)
        if (any(feature.mask)) feature.exists <- TRUE
      }

      # Branch out to next nodes to process - Yes, No and Missing
      nnext.process <- lapply(c('Yes','No','Missing'), function(x){
        nnext.conditions <- copy(node.conditions)  # initialise next conditions list

        # If feature does not exist in prior conditions, add new list to conditions
        if (!feature.exists){
          if (x == 'Yes'){
            nnext.conditions[[length(nnext.conditions) + 1]] <- list('feature'=node.feature, 'lower_bound'=-Inf, 'upper_bound'=node.split)
          } else if (x == 'No'){
            nnext.conditions[[length(nnext.conditions) + 1]] <- list('feature'=node.feature, 'lower_bound'=node.split, 'upper_bound'=Inf)
          } else {
            nnext.conditions[[length(nnext.conditions) + 1]] <- list('feature'=node.feature, 'lower_bound'=as.numeric(NA), 'upper_bound'=as.numeric(NA))
          }

        # If feature exists in prior conditions, combine with new conditions
        } else {
          feature.conditions <- nnext.conditions[feature.mask][[1]]  # current conditions
          if (x == 'Yes'){
            feature.conditions[['upper_bound']] <- min(feature.conditions$upper_bound, node.split, na.rm=FALSE)
            nnext.conditions[feature.mask][[1]] <- feature.conditions  # update conditions
          } else if (x == 'No'){
            feature.conditions[['lower_bound']] <- max(feature.conditions$lower_bound, node.split, na.rm=FALSE)
            nnext.conditions[feature.mask][[1]] <- feature.conditions  # update conditions
          } else if (x == 'Missing'){
            # Only continue missing if previous values were missing
            if (!is.na(feature.conditions$lower_bound)) return(NULL)
          }
        }
        out <- list('current.node' = single.tree[[x]][node.id + 1], 'current.conditions' = nnext.conditions)
        return(out)
      })  # end branch out

      # Remove NULLs (missing from non-missing) and other paths that will not be reached
      nnext.process <- nnext.process[sapply(nnext.process, function(x) !is.null(x))]

      temp.remove <- numeric(0)  # initialise vector of indicies to remove
      for (j in 1:length(nnext.process)){
        temp.test <- sapply(nnext.process[[j]]$current.conditions, function(x){
          temp <- FALSE
          if (!is.na(x$lower_bound)) if (x$lower_bound >= x$upper_bound) temp <- TRUE
          return(temp)
        })
        if (any(temp.test)) temp.remove <- c(temp.remove, j)
      }
      nnext.process[temp.remove] <- NULL

      # Update staging list
      tree.next_process <- c(tree.next_process, nnext.process)
    }  # end for loop across all current nodes to process

    # Update list to process
    tree.process <- copy(tree.next_process)
  }

  # Sort feature conditions in the order of their features names
  for (i in 1:length(tree.conditions)){
    tree.features <- sapply(tree.conditions[[i]]$conditions, function(x) x$feature)
    tree.conditions[[i]]$conditions <- tree.conditions[[i]]$conditions[order(tree.features)]
  }
  return(tree.conditions)  # return list of conditions
}
