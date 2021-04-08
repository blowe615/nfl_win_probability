export_wp <- function(model,pbp_data) {
    pbp_filtered <- pbp_data %>%
        filter(!is.na(down),!is.na(score_differential))
    model_input <- clean_plays(pbp_filtered)
    wp_preds <- as.data.frame(matrix(predict(model,as.matrix(model_input)))) %>%
        rename(my_wp=V1)
    pbp_filtered <- pbp_filtered %>%
        bind_cols(wp_preds)
    pbp_filtered<- pbp_filtered %>%
        mutate(my_home_wp = if_else(posteam==home_team,my_wp,1-my_wp),
               my_away_wp = if_else(posteam==away_team,my_wp,1-my_wp),
               my_home_wp_post = lead(my_home_wp),
               my_away_wp_post = 1-my_home_wp_post,
               my_home_wpa = my_home_wp_post - my_home_wp,
               my_wpa_pos = if_else(posteam==home_team,my_home_wpa,-1*my_home_wpa),
               winning_team = case_when(
                   my_wp > 0.5 ~ posteam,
                   my_wp < 0.5 ~ defteam,
                   my_wp == 0.5 ~ "TIE"),
               wp_chg = if_else(winning_team!=lead(winning_team),1,0),
               wp_chgd = if_else(lag(wp_chg)==1,1,0)
               )
    wp_chgs <- bind_cols(
        pbp_filtered %>%
            filter(wp_chg==1) %>%
            select(game_seconds_remaining,winning_team,my_away_wp,wp_chg,wp_chgd,qtr),
        pbp_filtered %>%
            filter(wp_chgd==1) %>%
            select(game_seconds_remaining,winning_team,my_away_wp,wp_chg,wp_chgd,qtr) %>%
            rename_with(function(x){paste0(x,"2")}))
    dummy_times <- (wp_chgs$game_seconds_remaining2 - wp_chgs$game_seconds_remaining)/
        (wp_chgs$my_away_wp2 - wp_chgs$my_away_wp) * (0.5 - wp_chgs$my_away_wp) +
        wp_chgs$game_seconds_remaining
    dummy_qtr_times <- wp_chgs$game_seconds_remaining + ((wp_chgs$qtr - 4)*900)
    pbp_filtered <- pbp_filtered %>%
        add_row(game_seconds_remaining = round(dummy_times),
                quarter_seconds_remaining = round(dummy_qtr_times), 
                winning_team = wp_chgs$winning_team,
                my_wp=0.5,my_home_wp = 0.5, my_away_wp = 0.5) %>%
        arrange(-game_seconds_remaining)
    
    pbp_filtered <- pbp_filtered %>%
        fill(c(total_home_score,total_away_score,qtr,home_team,away_team,posteam,defteam))
    return(pbp_filtered %>%
               select(home_team,away_team, game_seconds_remaining, quarter_seconds_remaining,
                      qtr,desc,total_home_score,total_away_score,my_wp,my_home_wp,my_away_wp))
}