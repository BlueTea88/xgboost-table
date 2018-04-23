
# Simple example of converting an XGBoost model into a table and ranking main effects and interactions

# Load libraries and codes
library(xgboost)
library(data.table)
source('D:/GitHub/xgboost-table/R/treeInteractions.R')
source('D:/GitHub/xgboost-table/R/treeConditions.R')


# Generate sample data
set.seed(1024)
x <- list()
for (i in 1:10) x[[i]] = i*rnorm(1000000, 10)
x <- as.data.table(x)
y = -1*x[, rowSums(.SD)] + x[['V1']]*x[['V2']] + x[['V3']]*x[['V4']]*x[['V5']] + rnorm(1000, 0.001) + 3*sin(x[['V7']])
train = as.matrix(x)

# Fit model
bst <- xgboost(data = train,
               label = y,
               max_depth = 4,
               eta = 0.1,
               nrounds = 200)

# Extract tree conditions
bst.tree <- xgb.model.dt.tree(model=bst)
bst.conditions <- treeConditions(bst.tree)

# Test lower order parameters
in.conditions <- copy(bst.conditions)
in.data <- copy(x)

time <- Sys.time()
test <- treeConditions(bst.tree)
print(Sys.time() - time)


Rprof('D:/GitHub/xgboost-table/test.out', line.profiling=TRUE)
test2 <- treeConditions(bst.tree)
Rprof(NULL)
summaryRprof('D:/GitHub/xgboost-table/test_treeConditions.out', lines='show')
