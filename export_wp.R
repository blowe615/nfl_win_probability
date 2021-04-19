export_wp <- function(model,pbp_data) {
    # A function to join win probability predictions to play-by-play data for
    # the purposes of generating win probability plots. This function also adds
    # extra rows for cleaner plotting.
    
    # Inputs:
    # model: a win probability model with the following 11 input features:
    #   receive_2h_ko, half_seconds_remaining, game_seconds_remaining,
    #   Diff_Time_Ratio, score_differential, down, ydstogo, yardline_100, home,
    #   posteam_timeouts_remaining, defteam_timeouts_remaining
    # pbp_data: tibble, play-by-play data with at minimum the following columns
    #   game_id: chr, unique identifier for each game
    #   home_team: chr, home team abbreviation
    #   away_team: chr, away team abbreviation
    #   posteam: chr, abbreviation of team possessing the ball for each play
    #   defteam: chr, abbreviation of team not possessing the ball for each play
    #   half_seconds_remaining: int, number of seconds remaining in half (0-1800)
    #   game_seconds_remaining: int, number of seconds remaining in regulation (0-3600)
    #   score_differential: int, difference between possessing team score and defending team score
    #   down: int, down of each play (1-4)
    #   ydstogo: int, yards to first down
    #   yardline_100: int, yards from opponent's end zone
    #   side_of_field: chr, abbreviation of the team whose side the ball is on
    #   posteam_timeouts_remaining: int, number of possessing team's timeouts
    #   defteam_timeouts_remaining: int, number of defending team's timeouts
    #   qtr: int, quarter number in regulation (1-4)
    #   total_home_score: home team score at each play of the game
    #   total_away_score: away team score at each play of the game
    #   desc: description of each play
    
    # Output:
    # filtered pbp_data with the following columns:
    #   home_team
    #   away_team
    #   game_seconds_remaining
    #   quarter_seconds_remaining
    #   qtr
    #   desc
    #   total_home_score
    #   total_away_score
    #   wp: win probability for the possessing team from the input model
    #   home_wp: win probability for the home team from the input model
    #   away_wp: win probability for the away team from the input model
    
    # remove plays with crucial missing data
    pbp_filtered <- pbp_data %>%
        filter(!is.na(down),!is.na(score_differential))
    
    # drop wp columns if they exist
    pbp_filtered <- pbp_filtered %>%
        select(-any_of(c("wp","home_wp","away_wp","home_wp_post","away_wp_post")))
    
    # call clean_plays function to prepare pbp data for wp model
    model_input <- clean_plays(pbp_filtered)
    
    # get win probability predictions and rename them as "wp"
    wp_preds <- as.data.frame(matrix(predict(model,as.matrix(model_input)))) %>%
        rename(wp=V1)
    
    # append "wp" column to filtered pbp data
    pbp_filtered <- pbp_filtered %>%
        bind_cols(wp_preds)
    
    # add new columns for home and away wp based on the wp model
    pbp_filtered<- pbp_filtered %>%
        mutate(home_wp = if_else(posteam==home_team,wp,1-wp),
               away_wp = if_else(posteam==away_team,wp,1-wp),
               # add post-play wp for win probability added (wpa)
               home_wp_post = lead(home_wp),
               away_wp_post = 1-home_wp_post,
               home_wpa = home_wp_post - home_wp,
               wpa_pos = if_else(posteam==home_team,home_wpa,-1*home_wpa),
               winning_team = case_when(
                   wp > 0.5 ~ posteam,
                   wp < 0.5 ~ defteam,
                   wp == 0.5 ~ "TIE"),
               # add column to flag plays where the winning team changes
               wp_chg = if_else(winning_team!=lead(winning_team),1,0),
               # add column to flag plays where the winning team changed on the
               # previous play
               wp_chgd = if_else(lag(wp_chg)==1,1,0)
               )
    # This section adds extra rows with win probability set to 50%. These rows
    # are added in between two plays where the winning team (with win probability
    # greater than 50%) changes so that there is a data point at 50% win
    # probability which allows for cleaner plotting and shading. The
    # game_seconds_remaining is interpolated between the two plays.
    
    # filter plays where win probability changes or just changed and create a
    # wide table to pair the plays before/after the wp change
    wp_chgs <- bind_cols(
        pbp_filtered %>%
            # filter plays where winning team changes
            filter(wp_chg==1) %>%
            select(game_seconds_remaining,winning_team,away_wp,wp_chg,wp_chgd,qtr),
        pbp_filtered %>%
            # filter plays where winning team just changed
            filter(wp_chgd==1) %>%
            select(game_seconds_remaining,winning_team,away_wp,wp_chg,wp_chgd,qtr) %>%
            # rename columns with a 2 to avoid duplicates
            rename_with(function(x){paste0(x,"2")}))
    
    # calculate dummy times (plays that didn't happen in the game) by interpolating
    # between the before/after plays
    dummy_times <- (wp_chgs$game_seconds_remaining2 - wp_chgs$game_seconds_remaining)/
        (wp_chgs$away_wp2 - wp_chgs$away_wp) * (0.5 - wp_chgs$away_wp) +
        wp_chgs$game_seconds_remaining
    
    # calculate quarter_time_remaining from game_seconds_remaining
    dummy_qtr_times <- wp_chgs$game_seconds_remaining + ((wp_chgs$qtr - 4)*900)
    
    # add dummy rows to filtered pbp data
    pbp_filtered <- pbp_filtered %>%
        add_row(game_seconds_remaining = round(dummy_times),
                quarter_seconds_remaining = round(dummy_qtr_times), 
                winning_team = wp_chgs$winning_team,
                wp=0.5,home_wp = 0.5, away_wp = 0.5) %>%
        # add row at beginning of the game to help with plot shading
        add_row(game_seconds_remaining = 3600, quarter_seconds_remaining = 900,
                total_home_score=0, total_away_score=0,
                wp=0.5, home_wp = 0.5, away_wp = 0.5) %>%
        # add row at end of game to assign wp of 1 to winning team to help with plot shading
        add_row(game_seconds_remaining = 0, quarter_seconds_remaining = 0,
                home_wp = case_when(last(pbp_filtered$result) > 0 ~ 1, 
                                       last(pbp_filtered$result) < 0 ~ 0,
                                       last(pbp_filtered$result) == 0 ~ 0.5),
                away_wp = 1-home_wp) %>%
        # add row at end of the game to help with plot shading
        add_row(game_seconds_remaining = 0, quarter_seconds_remaining = 0,
                wp=0.5, home_wp = 0.5, away_wp = 0.5) %>%
        # sort pbp data by game_seconds_remaining
        arrange(-game_seconds_remaining)
    
    # fill in missing values in dummy plays from previous rows
    pbp_filtered <- pbp_filtered %>%
        fill(c(total_home_score,total_away_score,qtr,home_team,away_team,posteam,defteam),
             .direction = "downup")
    
    # add columns for easier plotting
    pbp_filtered <- pbp_filtered %>%
        # add column for elapsed time (inverse of game_seconds_remaining)
        mutate(elapsed_time = 3600-game_seconds_remaining,
               # add column to label when away team has at least 50% wp
               winning_team_away = if_else(away_wp >= 0.5,away_team,home_team),
               # add column to label when home team has at least 50% wp
               winning_team_home = if_else(home_wp >= 0.5,home_team,away_team),
               # add column with away wp floor of 50%
               away_wp_floor = if_else(away_wp >= 0.5,away_wp,0.5),
               # add column with home wp ceiling of 50%
               home_wp_ceil = if_else(away_wp <= 0.5,away_wp,0.5))
    
    # return filtered pbp data with columns needed for plotting
    return(pbp_filtered %>%
               select(home_team,away_team, game_seconds_remaining, quarter_seconds_remaining,
                      qtr,desc,total_home_score,total_away_score,wp,home_wp,away_wp,
                      elapsed_time,winning_team_away,winning_team_home,away_wp_floor,
                      home_wp_ceil))
}