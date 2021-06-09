# Script to train, tune and evaluate overtime wp model

# Load necessary packages
library(tidyverse)

# Load necessary packages
source('train_ov_wp_model_LOSO_grid.R')
source('plot_ov_wp_model_LOSO_grid.R')

# generate parameter grid of all combinations of parameters to test for tuning
param_grid <- crossing(booster="gbtree",
                       objective="binary:logistic",
                       eval_metric="logloss",
                       eta=0.2,
                       gamma=0,
                       subsample=c(0.5,0.8,1),
                       colsample_bytree=1,
                       max_depth=c(4,6,8),
                       min_child_weight=1,
                       max_delta_step=0,
                       nrounds=10)

# get the list of model parameters that are being tuned (i.e. that have more
# than one valueC)
param_group <- colnames(param_grid %>% summarise(across(.fns=n_distinct)) %>%
                                        select_if(function(x) x>1))

# call the grid search function
cv_results_tuning <- train_ov_wp_model_LOSO_grid(ov_plays,param_grid)

# plot the grid search results
print(plot_ov_wp_model_LOSO_grid(cv_results_tuning,"RMSE",param_group))