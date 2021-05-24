train_ov_wp_model_LOSO <- function(pbp_data, nrounds,eta=0.3,max_delta_step=1){
    ## Function to perform Leave One Season Out (LOSO) cross validation on NFL overtime
    ## play-by-play (pbp) data using XGBoost. LOSO will create n models for n seasons
    ## (21 in this case) that trains a model on all seasons but one then uses that
    ## season to cross validate the model.
    
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
    
    # Output:
    # cv_results: tbl containing the input columns along with predicted win
    #             probability for each play based on a model trained all seasons
    #             excluding the season being evaluated.
    
    # create a list of pbp seasons
    seasons <- 1999:2020
    
    cv_results <- map_dfr(seasons,function(x){
        print(paste("training",x,"season..."))
        # Drop the season from the training data
        train_data <- pbp_data %>%
            filter(season!=x) %>%
            # Select the relevant features and the label
            select(label, first_drive, game_seconds_remaining,
                    score_differential, down, ydstogo, yardline_100,
                   home, posteam_timeouts_remaining, defteam_timeouts_remaining,
                   -season)
        # Assign the dropped season to the test dataset
        test_data <- pbp_data %>%
            filter(season==x) %>%
            select(label, first_drive, game_seconds_remaining,
                    score_differential, down, ydstogo, yardline_100,
                   home, posteam_timeouts_remaining, defteam_timeouts_remaining,
                   -season)
        
        # define model training parameters (same as nflfastR)
        nrounds <- nrounds
        params <- list(booster="gbtree",objective="binary:logistic",
                      eval_metric="logloss",eta=eta,gamma=0,subsample=0.8,
                      colsample_bytree = 0.8, max_depth=4, min_child_weight=1,
                      max_delta_step=max_delta_step)
        
        # train the models
        my_ov_wp_model <- xgboost::xgboost(data=as.matrix(train_data %>% select(-label)),
                                        label=train_data$label,params=params,nrounds=nrounds,
                                        verbose=0)
        
        print("training complete!")
        
        # add predictions for each season to a prediction df
        preds <- as.data.frame(matrix(predict(my_ov_wp_model,
                    as.matrix(test_data %>% select(-label))))) %>%
                    rename(wp=V1)
    
        # add column of predictions to pbp data
        cv_data <- bind_cols(test_data,preds) %>% mutate(season=x)
        return(cv_data)
    })}
