
# Simple example of converting an XGBoost model into a table and ranking main effects and interactions
# Load libraries
library(data.table)
library(xgboost)
library(xgboostcon)

# Generate sample data
set.seed(1024)
x <- list()
for (i in 1:10) x[[i]] = i*rnorm(2000, 10)
x <- as.data.table(x)
y = -1*x[, rowSums(.SD)] + x[['V1']]*x[['V2']]*50 + x[['V3']]*x[['V4']]*x[['V5']] + rnorm(1000, 0.001) + 3*sin(x[['V7']])
train = as.matrix(x)

# Fit model
bst <- xgboost(data = train,
               label = y,
               max_depth = 3,
               eta = 0.1,
               nrounds = 50,
               verbose = 0,
               base_score = 0)

# Extract tree conditions and calculate ranks
bst.tree <- xgb.model.dt.tree(feature_names = colnames(train), model = bst)
bst.conditions <- treeConditions(bst.tree)
bst.norm <- treeNormalise(bst.conditions, x, rep(1:nrow(x)))
bst.ranks <- treeRanks(bst.norm, x, rep(1:nrow(x)))

# Print out ranks
print(bst.ranks[1:10])

# Score using conditions and compare with XGBoost scores
bst.pred_con <- treeScores(bst.norm, x)
bst.pred_model <- predict(bst, train)
pct_diff <- sum(abs(bst.pred_con - bst.pred_model) > 0.1)/nrow(x)
print(paste0('Percentage of obs with large differences: ',pct_diff))
