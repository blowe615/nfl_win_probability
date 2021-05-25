# Script to train, tune and evaluate overtime wp model

# generate parameter grid of all combinations of parameters to test for tuning
param_grid <- crossing(booster="gbtree",
                       objective="binary:logistic",
                       eval_metric="logloss",
                       eta=0.2,
                       gamma=0,
                       subsample=1,
                       colsample_bytree=1,
                       max_depth=6,
                       min_child_weight=1,
                       max_delta_step=0,
                       nrounds=seq(10,40,10))

# call the grid search function
cv_results_tuning <- train_ov_wp_model_LOSO_grid(ov_plays,param_grid)

# plot the grid search results
print(plot_ov_wp_model_LOSO_grid(cv_results_tuning,"RMSE"))