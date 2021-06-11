get_ov_plays <- function(){
    # create a list of NFL pbp seasons
    seasons <- 1999:2020
    
    # create a df of plays for all seasons
    ov_plays <- map_df(seasons,function(x){
        readRDS(url(
            paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
        )
        ) %>%
            # select columns needed for cleaning, mutating and training
            # select(season,game_id,home_team,away_team,posteam,defteam,
            #        game_seconds_remaining,score_differential,down,ydstogo,yardline_100,
            #        side_of_field,posteam_timeouts_remaining,defteam_timeouts_remaining,
            #        qtr,drive,result) %>%
            filter(qtr==5) %>%
            # group by game_id to perform mutations
            group_by(game_id) %>%
            mutate(first_drive = if_else(drive==first(na.omit(drive)),1,0),
                   home = if_else(posteam==home_team,1,0),
                   winner = case_when(result > 0 ~ home_team,
                                      result < 0 ~ away_team,
                                      result == 0 ~ "TIE"),
                   # add label for training
                   label = if_else(posteam==winner,1,0)) %>%
            # filter out plays with crucial missing data
            filter(!is.na(down),!is.na(score_differential)) %>%
            # if yardline_100 is null, use a RegEx to pull yardline_100 from side_of_field
            mutate(yardline_100 = if_else(is.na(yardline_100),
                                          as.numeric(regmatches(side_of_field,gregexpr("[0-9]*\\.?[0-9]+",side_of_field))),
                                          yardline_100)) %>%
            ungroup()
    })
    return(ov_plays)
}