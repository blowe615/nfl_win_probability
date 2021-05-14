train_ov_wp_model_LOSO <- function(pbp_data, nrounds){
    ## Function to perform Leave One Season Out (LOSO) cross validation on NFL overtime
    ## play-by-play (pbp) data using XGBoost. LOSO will create n models for n seasons
    ## (21 in this case) that trains a model on all seasons but one then uses that
    ## season to cross validate the model.
    
    # create a list of pbp seasons
    seasons <- 1999:2020
    
    cv_results <- map_dfr(seasons,function(x){
        print(paste("training",x,"season..."))
        # Drop the season from the training data
        train_data <- pbp_data %>%
            filter(season!=x) %>%
            # Select the relevant features and the label
            select(label, first_drive, game_seconds_remaining,
                   Diff_Time_Ratio, score_differential, down, ydstogo, yardline_100,
                   home, posteam_timeouts_remaining, defteam_timeouts_remaining,
                   -season)
        # Assign the dropped season to the test dataset
        test_data <- pbp_data %>%
            filter(season==x) %>%
            select(label, first_drive, game_seconds_remaining,
                   Diff_Time_Ratio, score_differential, down, ydstogo, yardline_100,
                   home, posteam_timeouts_remaining, defteam_timeouts_remaining,
                   -season)
        
        # define model training parameters (same as nflfastR)
        nrounds <- nrounds
        params <- list(booster="gbtree",objective="binary:logistic",
                      eval_metric="logloss",eta=0.2,gamma=0,subsample=0.8,
                      colsample_bytree = 0.8, max_depth=4, min_child_weight=1)
        
        # train the models
        my_ov_wp_model <- xgboost::xgboost(data=as.matrix(train_data %>% select(-label)),
                                        label=train_data$label,params=params,nrounds=nrounds)
        print("training complete!")
        
        # add predictions for each season to a prediction df
        preds <- as.data.frame(matrix(predict(my_ov_wp_model,
                    as.matrix(test_data %>% select(-label))))) %>%
                    rename(wp=V1)
    
        # add column of predictions to pbp data
        cv_data <- bind_cols(test_data,preds) %>% mutate(season=x)
        return(cv_data)
    })}
