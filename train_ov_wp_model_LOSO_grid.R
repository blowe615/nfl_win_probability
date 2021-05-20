# Script to perform grid search through xgboost parameters to tune an
# overtime play-by-play model

# generate parameter grid of all combinations of parameters to test for tuning
param_grid <- crossing(nrounds=seq(10,40,10),eta=seq(0.2,0.4,0.05),max_delta_step=seq(1,10,1))

# pass the training parameters into an anonymous function which will iterate
# through each combination and create a df of all of the results of training
# and LOSO cv
cv_results_nr <- map_dfr(transpose(param_grid),function(x){
    # call the train_ov_wp_model_LOSO function on the overtime plays dataset
    # and pass it a single combination of tuning parameters
    cv_data<-train_ov_wp_model_LOSO(ov_plays,x$nrounds,x$eta,x$max_delta_step)
    # add labels to cv data for the tuning parameters used
    cv_data <- cv_data %>%
        mutate(nrounds = x$nrounds,
               eta=x$eta,
               max_delta_step=x$max_delta_step)
})