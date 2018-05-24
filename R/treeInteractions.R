#' Tree Interactions
#'
#' Extract interactions fitted in an XGBoost model.
#'
#' @param in.tree Trees from a fitted XGBoost model
#' @param in.depth Maximum depth of the input model
#'
#' @details
#' Search the trees of an XGBoost model to identify fitted interactions.
#' For tree based models, interactions are modelled by features sharing the same tree.
#' Pads out lower order interactions that are contained within a higher order interaction.
#'
#' @return
#' Returns a list of vectors where each item of the list contains the feature names of one interaction.
#'
#' @import data.table
#' @export
treeInteractions <- function(in.tree, in.depth){
  tree <- copy(in.tree)
  if (in.depth < 2) return(list())
  if (nrow(in.tree) == 1) return(list())

  # Attach parent nodes
  for (i in 2:in.depth){
    if (i == 2) tree[, ID_merge:=ID] else tree[, ID_merge:=get(paste0('parent_',i-2))]
    parents.left <- tree[!is.na(Split), list(i.id=ID, i.feature=Feature, ID_merge=Yes)]
    parents.right <- tree[!is.na(Split), list(i.id=ID, i.feature=Feature, ID_merge=No)]

    setorderv(tree, 'ID_merge')
    setorderv(parents.left, 'ID_merge')
    setorderv(parents.right, 'ID_merge')

    tree <- merge(tree, parents.left, by='ID_merge', all.x=T)
    tree[!is.na(i.id), c(paste0('parent_',i-1), paste0('parent_feat_',i-1)):=list(i.id,i.feature)]
    tree[, c('i.id','i.feature'):=NULL]

    tree <- merge(tree, parents.right, by='ID_merge', all.x=T)
    tree[!is.na(i.id), c(paste0('parent_',i-1), paste0('parent_feat_',i-1)):=list(i.id,i.feature)]
    tree[, c('i.id','i.feature'):=NULL]
  }

  # Extract nodes with interactions
  temp <- tree[!is.na(Split) & !is.na(parent_1)]
  temp <- temp[, c('Feature',paste0('parent_feat_',1:(in.depth-1))), with=F]
  temp <- split(temp, 1:nrow(temp))
  out <- lapply(temp, as.character)

  # Remove NAs (no parent interaction)
  out <- lapply(out, function(x) x[!is.na(x)])

  # Remove non-interactions (same variable)
  out <- lapply(out, unique)
  temp <- sapply(out, length)
  out <- out[temp > 1]
  out <- unique(lapply(out, sort))
  return(out)
}
