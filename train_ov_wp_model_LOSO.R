## Script to perform Leave One Season Out (LOSO) cross validation on NFL overtime
## play-by-play (pbp) data using XGBoost. LOSO will create n models for n seasons
## (21 in this case) that trains a model on all seasons but one then uses that
## season to cross validate the model.

# create a list of pbp seasons
seasons <- 1999:2020

# create a df of plays for all seasons
wp_model_data <- map_df(seasons,function(x){
    readRDS(url(
        paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
        )
    ) %>%
        # select columns needed for cleaning, mutating, and training
        select(season,game_id,home_team,away_team,posteam,defteam,half_seconds_remaining,
               game_seconds_remaining,score_differential,down,ydstogo,yardline_100,
               side_of_field,posteam_timeouts_remaining,defteam_timeouts_remaining,
               qtr,result,drive) %>%
        # filter to overtime plays before performing mutations
        filter(qtr==5) %>%
        # group by game_id to perform mutations
        group_by(game_id) %>%
        mutate(first_drive = if_else(drive == first(na.omit(drive)),1,0),
               home = if_else(posteam == home_team,1,0),
               elapsed_share = if_else(season >= 2017,
                                       (4200 - game_seconds_remaining)/4200,
                                       (4500 - game_seconds_remaining)/4500),
               Diff_Time_Ratio = score_differential/exp(-4 * elapsed_share),
               winner = case_when(result > 0 ~ home_team,
                                  result < 0 ~ away_team,
                                  result == 0 ~ "TIE"),
               # add label for training
               label = if_else(posteam == winner,1,0)) %>%
        # filter out plays with crucial missing data
        filter(!is.na(down),!is.na(score_differential)) %>%
        # if yardline_100 is null, use a RegEx to pull yardline_100 from side_of_field
        mutate(yardline_100 = if_else(is.na(yardline_100),
            as.numeric(regmatches(side_of_field,gregexpr("[0-9]*\\.?[0-9]+",side_of_field))),
            yardline_100)) %>%
        ungroup()
})
cv_results65 <- map_dfr(seasons,function(x){
    print(paste("training",x,"season..."))
    # Drop the season from the training data
    train_data <- wp_model_data %>%
        filter(season!=x) %>%
        # Select the relevant features and the label
        select(label, first_drive, game_seconds_remaining,
               Diff_Time_Ratio, score_differential, down, ydstogo, yardline_100,
               home, posteam_timeouts_remaining, defteam_timeouts_remaining,
               -season)
    # Assign the dropped season to the test dataset
    test_data <- wp_model_data %>%
        filter(season==x) %>%
        select(label, first_drive, game_seconds_remaining,
               Diff_Time_Ratio, score_differential, down, ydstogo, yardline_100,
               home, posteam_timeouts_remaining, defteam_timeouts_remaining,
               -season)
    
    # define model training parameters (same as nflfastR)
    nrounds <- 65
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
})
