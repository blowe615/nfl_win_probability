train_ov_wp_model_LOSO_grid<-function(pbp_data,param_grid){
    # Function to perform grid search through xgboost parameters to tune an
    # overtime play-by-play model
    
    # Inputs:
    # pbp_data: tbl, containing labeled overtime plays. Must have the following
    # columns, in any order:
    #   label: int, 1 if possessing team won the game, otherwise 0
    #   first_drive: int, 1 if first drive of overtime, otherwise 0
    #   game_seconds_remaining: int, number of seconds remaining in overtime
    #                           (900 prior to 2017, 600 starting in 2017)
    #   Diff_Time_Ratio: dbl, score_differential/exp(-4 * elapsed_share))
    #   score_differential: int, difference between possessing team score and defending team score
    #   down: int, down of each play (1-4)
    #   ydstogo: int, yards to first down
    #   yardline_100: int, yards from opponent's end zone
    #   home: int, 1 if possessing team is home, otherwise 0
    #   posteam_timeouts_remaining: int, number of possessing team's timeouts
    #   defteam_timeouts_remaining: int, number of defending team's timeouts
    #   season: int, 4 digit year
    # param_grid: tbl, contains all combinations of the model parameters to use during tuning
    
    # Output
    # cv_results_grid: tbl containing the input columns along with predicted win
    #             probability for each play based on a model trained all seasons
    #             excluding the season being evaluated as well as columns containing
    #             the tuning parameters used for that model
    
    # Source the training function for a single param set
    source("train_ov_wp_model_LOSO.R")
    
    # pass the training parameters into an anonymous function which will iterate
    # through each combination and create a df of all of the results of training
    # and LOSO cv
    
    cv_results_grid <- map_dfr(transpose(param_grid),function(x){
        # extract all parameters except nrounds and assign them to params
        params<-x
        params$nrounds<-NULL
        # call the train_ov_wp_model_LOSO function on the overtime plays dataset
        # and pass it a single combination of tuning parameters
        cv_data<-train_ov_wp_model_LOSO(pbp_data,x$nrounds,params)
        # add labels to cv data for the tuning parameters used
        cv_data <- cv_data %>%
            mutate(booster=x$booster,
                   objective=x$objective,
                   eval_metric=x$eval_metric,
                   eta=x$eta,
                   gamma=x$gamma,
                   subsample=x$subsample,
                   colsample_bytree=x$colsample_bytree,
                   max_depth=x$max_depth,
                   min_child_weight=x$min_child_weight,
                   max_delta_step=x$max_delta_step,
                   nrounds=x$nrounds)
    })
}