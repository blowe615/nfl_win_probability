train_ov_wp_model <- function(pbp_data, nrounds=12, params=list(booster="gbtree",
    objective="binary:logistic", eval_metric="logloss",eta=0.2,gamma=0,
    subsample=0.5,colsample_bytree=1,max_depth=4,min_child_weight=1,
    max_delta_step=5)){
    ## Function to train an XGBoost win probability model on all seasons of
    ## overtime play-by-play data.
    ## This should be performed after the Leave One Season Out (LOSO) cross
    ## validation to confirm that the model parameters are sufficiently accurate.
    
    # Inputs:
    # pbp_data: tbl, containing labeled overtime plays. Must have the following
    # columns, in any order:
    #   label: int, 1 if possessing team won the game, otherwise 0
    #   first_drive: int, 1 if first drive of overtime, otherwise 0
    #   game_seconds_remaining: int, number of seconds remaining in overtime
    #                           (900 prior to 2017, 600 starting in 2017)
    #   score_differential: int, difference between possessing team score and defending team score
    #   down: int, down of each play (1-4)
    #   ydstogo: int, yards to first down
    #   yardline_100: int, yards from opponent's end zone
    #   home: int, 1 if possessing team is home, otherwise 0
    #   posteam_timeouts_remaining: int, number of possessing team's timeouts
    #   defteam_timeouts_remaining: int, number of defending team's timeouts
    #   season: int, 4 digit year
    # nrounds: int, number of boosting iterations to perform (default=12)
    # params: list, contains model parameters required for xgboost model
    #        (defaults based on grid search and LOSO cross validation)
    
    # Output:
    # ov_wp_model: XGBoost model
    
    # create training data set
    train_data <- pbp_data %>%
        select(label, first_drive, game_seconds_remaining,
                score_differential, down, ydstogo, yardline_100,
               home, posteam_timeouts_remaining, defteam_timeouts_remaining)
    # train the model
    ov_wp_model <- xgboost::xgboost(data=as.matrix(train_data %>% select(-label)),
                                    label=train_data$label,params=params,nrounds=nrounds,
                                    verbose=0)
    
    # return the model object
    return(ov_wp_model)
    }
