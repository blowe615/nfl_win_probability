export_wp <- function(model,ov_model,pbp_data) {
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
    #   season
    #   game_id
    #   season_type
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
    #   elapsed_time: time (seconds) since start of game
    #   winning_team_away: returns away team if wp >= 50%, otherwise home team
    #   winning_team_home: returns home team if wp >= 50%, otherwise away team
    #   away_wp_floor: returns greater of away wp or 50%
    #   home_wp_ceil: returns greater of away wp or 50%
    #   away_team_alt: flags the away team (by adding a "2") if they have the
    #                   same team color as the home team (for easier plotting)
    
    # manually fix (mutate down, ydstogo, yardline_100) kickoffs to model wp
    # of a touchback
    # add 0.01 seconds to game_seconds_remaining for kickoffs to avoid plotting
    # two plays at the same time (only effects untimed plays like touchbacks)
    pbp_filtered <- pbp_data %>%
        mutate(down=(if_else(kickoff_attempt==1,1,down)),
               ydstogo=(if_else(kickoff_attempt==1,10,ydstogo)),
               yardline_100=(if_else(kickoff_attempt==1,
                                     if_else(season<2016,80,75),yardline_100)),
               game_seconds_remaining =(if_else(kickoff_attempt==1,
                                                game_seconds_remaining+0.01,
                                                game_seconds_remaining))) %>%
        # remove plays with crucial missing data (i.e. timeouts, end of quarter)
        filter(!is.na(down),!is.na(score_differential))
    
    # drop wp columns if they exist
    pbp_filtered <- pbp_filtered %>%
        select(-any_of(c("wp","home_wp","away_wp","home_wp_post","away_wp_post")))
    
    # call clean_plays function to prepare pbp data for reg season wp model
    model_input <- clean_plays(pbp_filtered)
    
    # get win probability predictions and rename them as "wp"
    wp_preds <- as.data.frame(matrix(predict(model,as.matrix(model_input)))) %>%
        rename(wp=V1)
    
    # append "wp" column to filtered pbp data
    pbp_filtered_reg <- pbp_filtered %>%
        filter(qtr<5) %>%
        bind_cols(wp_preds)
    
    # call clean_plays function to prepare pbp data for ov season wp model
    model_input_ov <- clean_ov_plays(pbp_filtered)
    
    # get win probability predictions and rename them as "wp"
    wp_preds_ov <- as.data.frame(matrix(predict(ov_model,as.matrix(model_input_ov)))) %>%
        rename(wp=V1)
    
    # append "wp" column to filtered pbp data
    pbp_filtered_ov <- pbp_filtered %>%
        filter(qtr>4) %>%
        bind_cols(wp_preds)
    
    # combine regulation and overtime pbp data with wp added
    pbp_filtered <- full_join(pbp_filtered_reg,pbp_filtered_ov) %>%
        # add column for elapsed time (inverse of game_seconds_remaining)
        mutate(elapsed_time = if_else(qtr<5,3600-game_seconds_remaining,
                                      if_else(season>2016,4200-game_seconds_remaining,
                                              4500-game_seconds_remaining))) %>%
        arrange(season, game_id, elapsed_time)
    
    # add new columns for home and away wp based on the wp model
    pbp_filtered <- pbp_filtered %>%
        group_by(game_id) %>%
        mutate(home_wp = if_else(posteam==home_team,wp,1-wp),
               away_wp = if_else(posteam==away_team,wp,1-wp)) %>%
        ungroup()
    
    # create df with result of each game to add a row at end of each game to
    # assign wp of 1 to winning team to help with plot shading
    game_results <- pbp_filtered %>%
        group_by(game_id) %>%
        # identify columns for each game that need to be specified in added rows
        summarise(result=last(result), season=last(season),
                  season_type=last(season_type),home_team=last(home_team),
                  away_team=last(away_team), elapsed_time=max(elapsed_time),
                  qtr=max(qtr))
    
    # add additional rows at beginning and end of each game to help plotting
    pbp_filtered <- pbp_filtered %>%
        # add row at beginning of each game with wp 50% to help with plot shading
        add_row(game_id = game_results$game_id, season=game_results$season,
                season_type = game_results$season_type,
                game_seconds_remaining = 3600.02,quarter_seconds_remaining = 900,
                total_home_score=0,total_away_score=0, wp=0.5, home_wp = 0.5,
                away_wp = 0.5, elapsed_time = -0.02) %>%
        # add row at end of each game to assign wp of 1 to winning team to help with plot shading
        add_row(game_id = game_results$game_id, season=game_results$season,
                season_type = game_results$season_type,
                home_team = game_results$home_team, away_team = game_results$away_team,
                game_seconds_remaining = 0, quarter_seconds_remaining = 0,
                elapsed_time = if_else(game_results$qtr<5,3600,
                                       game_results$elapsed_time+0.005),
                home_wp = case_when(game_results$result > 0 ~ 1, 
                                    game_results$result < 0 ~ 0,
                                    game_results$result == 0 ~ 0.5),
                away_wp = 1-home_wp,
                desc = case_when(game_results$result > 0 ~ paste(home_team,"WINS"),
                                 game_results$result < 0 ~ paste(away_team,"WINS"),
                                 game_results$result == 0 ~ "TIE")) %>%
        # add row at end of each game with wp 50% to help with plot shading
        add_row(game_id = game_results$game_id, season=game_results$season,
                season_type = game_results$season_type,
                wp=0.5, home_wp = 0.5, away_wp = 0.5,
                game_seconds_remaining = -0.01, quarter_seconds_remaining = 0,
                elapsed_time = if_else(game_results$qtr<5,3600.01,
                                       game_results$elapsed_time+0.01)) %>%
        
        # sort by game_id and elapsed_time
        arrange(season,game_id,elapsed_time)
    
    # add column to identify which team has higher win probability
    pbp_filtered <- pbp_filtered %>%
        group_by(game_id) %>%
        mutate(winning_team = case_when(
                home_wp > 0.5 ~ home_team,
                home_wp < 0.5 ~ away_team,
                home_wp == 0.5 ~ "TIE"),
            # add column to flag plays where the winning team changes
            wp_chg = if_else(winning_team!=lead(winning_team),1,0),
            # add column to flag plays where the winning team changed on the
            # previous play
            wp_chgd = if_else(lag(wp_chg)==1,1,0)) %>%
        ungroup()
    
    # add post-play wp for win probability added (wpa)
    pbp_filtered <- pbp_filtered %>%
        group_by(game_id) %>%
        mutate(home_wp_post = lead(home_wp),
               away_wp_post = 1-home_wp_post,
               home_wpa = home_wp_post - home_wp,
               wpa_pos = if_else(posteam==home_team,home_wpa,-1*home_wpa)) %>%
        ungroup()
    
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
            select(game_id,season,season_type,game_seconds_remaining,
                   winning_team,away_wp,wp_chg,wp_chgd,qtr,elapsed_time),
        pbp_filtered %>%
            # filter plays where winning team just changed
            filter(wp_chgd==1) %>%
            select(game_id,season,season_type,game_seconds_remaining,
                   winning_team,away_wp,wp_chg,wp_chgd,qtr,elapsed_time) %>%
            # rename columns with a 2 to avoid duplicates
            rename_with(function(x){paste0(x,"2")})) %>%
        # filter out plays added at the end of the game for plotting
        filter(game_seconds_remaining2 >=0)
    
    # calculate dummy times (plays that didn't happen in the game) by interpolating
    # between the before/after plays
    dummy_times <- (wp_chgs$elapsed_time2 - wp_chgs$elapsed_time)/
        (wp_chgs$away_wp2 - wp_chgs$away_wp) * (0.5 - wp_chgs$away_wp) +
        wp_chgs$elapsed_time
    
    # calculate quarter_time_remaining from game_seconds_remaining
    dummy_qtr_times <- wp_chgs$game_seconds_remaining + ((wp_chgs$qtr - 4)*900)
    
    # add dummy rows to filtered pbp data
    pbp_filtered <- pbp_filtered %>%
        add_row(game_id = wp_chgs$game_id, season = wp_chgs$season,
                season_type = wp_chgs$season_type,
                elapsed_time = dummy_times,
                quarter_seconds_remaining = dummy_qtr_times, 
                winning_team = wp_chgs$winning_team,
                wp=0.5,home_wp = 0.5, away_wp = 0.5) %>%
        # sort pbp data by game_seconds_remaining
        arrange(game_id,elapsed_time)
    
    # fill in missing values in dummy plays from previous rows
    pbp_filtered <- pbp_filtered %>%
        group_by(game_id) %>%
        fill(c(total_home_score,total_away_score,qtr,home_team,away_team,posteam,defteam),
             .direction = "downup") %>%
        ungroup()
    
    # create lists of teams that have the same plot colors
    black_list = c("CIN","JAX","PIT")
    blue_list = c("DAL","DEN","LA","LAC","LAR","NE","SD","SEA","STL","TEN","IND",
                  "HOU","BUF","CHI","NYG")
    green_list = c("GB","NYJ","PHI")
    purple_list = c("BAL","MIN")
    red_list = c("ARI","ATL","SF","TB","KC","WAS")
    lt_blue_list = c("CAR","DET","MIA")
    
    # add columns for easier plotting
    pbp_filtered <- pbp_filtered %>%
           # add column to flag teams that have the same plot colors
           mutate(away_team_alt = if_else((home_team %in% black_list) & 
                                       (away_team %in% black_list),
                                   paste0(away_team,"2"),
                           if_else((home_team %in% blue_list) & 
                                       (away_team %in% blue_list),
                                   paste0(away_team,"2"),
                           if_else((home_team %in% green_list) & 
                                       (away_team %in% green_list),
                                   paste0(away_team,"2"),
                           if_else((home_team %in% purple_list) & 
                                       (away_team %in% purple_list),
                                   paste0(away_team,"2"),
                           if_else((home_team %in% red_list) & 
                                       (away_team %in% red_list),
                                   paste0(away_team,"2"),
                           if_else((home_team %in% lt_blue_list) & 
                                       (away_team %in% lt_blue_list),
                                   paste0(away_team,"2"),away_team)))))),
               # add column to label when away team has at least 50% wp
               winning_team_away = if_else(away_wp >= 0.5,away_team_alt,home_team),
               # add column to label when home team has at least 50% wp
               winning_team_home = if_else(home_wp >= 0.5,home_team,away_team_alt),
               # add column with away wp floor of 50%
               away_wp_floor = if_else(away_wp >= 0.5,away_wp,0.5),
               # add column with home wp ceiling of 50%
               home_wp_ceil = if_else(away_wp <= 0.5,away_wp,0.5),)
    
    # mutate season_type for plot filtering
    pbp_filtered <- pbp_filtered %>%
        mutate(season_type = recode(season_type,
                                    "REG" = "Regular",
                                    "POST" = "Postseason"))
    
    # return filtered pbp data with columns needed for plotting
    return(pbp_filtered %>%
               select(season,game_id,season_type,home_team,away_team,
                      game_seconds_remaining,quarter_seconds_remaining,qtr,desc,
                      total_home_score,total_away_score,wp,home_wp,away_wp,
                      elapsed_time,winning_team_away,winning_team_home,
                      away_wp_floor,home_wp_ceil,away_team_alt))
}