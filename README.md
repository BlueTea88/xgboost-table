# xgboost_conditions

R functions to extract tree conditions from an XGBoost model and rank the importance of interactions 
alongside individual feature contributions.

## Installation

``` r
# Install from github using devtools:
devtools::install_github("BlueTea88/xgboostcon")
```

## Overview

Trees can be written as a set of conditions with an associated prediction value if the conditions are 
fulfilled.

Summing up all the prediction values give you your model prediction.

Conditions can be ranked based on their magnitude of contribution to the final predicted value.

We can also rank conditions at the parameter level by applying them to a dataset and aggregating the 
weighted absolute value of contributions for each parameter.

However, a major drawback of trees is that each level of depth implies another interaction dimension.
So our trees are seemingly full of interaction parameters.  In reality, we know this is unlikely to 
be true - main effects normally explain the majority of our response and interactions add only 
marginal benefit.  So we would like a way to separate lower order contributions from the initial
tree conditions.

Luckily, lower order conditions can be inferred with prediction values based on a weighted average of 
associated higher order conditions.

And again, applying our new "adjusted" conditions to a dataset, and aggregating, we can rank individual
feature conditions alongside interactions!

## Quick Guide

See the vignette below for a quick example:

## Notes

* Extracted conditions does not include the original base score so the intercept value is not accurate.
* Input data needs to be converted in the form of a data.table.
* Trees can be extracted from an XGBoost model using xgb.model.dt.tree, extracted trees must have column names.
* Can be slow if the model had a high max_depth (greater than 3) or has a large number of trees.
* If early_stopping_rounds is used, should use only the trees up to best_ntreelimit
