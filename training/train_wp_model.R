## Script to train an XGBoost model on all seasons of NFL play-by-play (pbp) data.
## This should be performed after the Leave One Season Out (LOSO) cross
## validation to confirm that the model parameters are sufficiently accurate.

# create a list of NFL pbp seasons
seasons <- 1999:2020

# create a df of plays for all seasons
wp_model_data <- map_df(seasons,function(x){
    readRDS(url(
        paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
        )
    ) %>%
        # select columns needed for cleaning, mutating and training
        select(season,game_id,home_team,away_team,posteam,defteam,half_seconds_remaining,
               game_seconds_remaining,score_differential,down,ydstogo,yardline_100,
               side_of_field,posteam_timeouts_remaining,defteam_timeouts_remaining,
               qtr,result) %>%
        
        # group by game_id to perform mutations
        group_by(game_id) %>%
        mutate(receive_2h_ko = if_else(qtr <= 2 & posteam==first(na.omit(defteam)),1,0),
               home = if_else(posteam==home_team,1,0),
               elapsed_share = (3600 - game_seconds_remaining)/3600,
               Diff_Time_Ratio = score_differential/exp(-4 * elapsed_share),
               Winner = case_when(result > 0 ~ home_team,
                                  result < 0 ~ away_team,
                                  result == 0 ~ "TIE"),
               # add label for training
               label = if_else(posteam==Winner,1,0)) %>%
        # filter out plays with crucial missing data
        filter(!is.na(down),!is.na(score_differential),qtr<=4) %>%
        # if yardline_100 is null, use a RegEx to pull yardline_100 from side_of_field
        mutate(yardline_100 = if_else(is.na(yardline_100),
            as.numeric(regmatches(side_of_field,gregexpr("[0-9]*\\.?[0-9]+",side_of_field))),
            yardline_100)) %>%
        ungroup()
})

# create training data set
train_data <- wp_model_data %>%
    # Select the relevant features and the label (+ qtr for plotting)
    select(label, receive_2h_ko, half_seconds_remaining, game_seconds_remaining,
           Diff_Time_Ratio, score_differential, down, ydstogo, yardline_100,
           home, posteam_timeouts_remaining, defteam_timeouts_remaining, qtr,
           -season)

# define model parameters (same as nflfastR)
nrounds <- 65
params <- list(booster="gbtree",objective="binary:logistic",
               eval_metric="logloss",eta=0.2,gamma=0,subsample=0.8,
               colsample_bytree = 0.8, max_depth=4, min_child_weight=1)

# train the model
my_wp_model <- xgboost::xgboost(data=as.matrix(train_data %>% select(-label,-qtr)),
                                label=train_data$label,params=params,nrounds=nrounds)
print("training complete!")