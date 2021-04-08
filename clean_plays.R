clean_plays <- function(pbp_data) {
    pbp_data %>%
        select(game_id,home_team,away_team,posteam,defteam,half_seconds_remaining,
               game_seconds_remaining,score_differential,down,ydstogo,yardline_100,
               side_of_field,posteam_timeouts_remaining,defteam_timeouts_remaining,
               qtr) %>%
        group_by(game_id) %>%
        mutate(receive_2h_ko = if_else(qtr <= 2 & posteam==first(na.omit(defteam)),1,0),
               home = if_else(posteam==home_team,1,0),
               elapsed_share = (3600 - game_seconds_remaining)/3600,
               Diff_Time_Ratio = score_differential/exp(-4 * elapsed_share)) %>%
        filter(!is.na(down),!is.na(score_differential),qtr<=4) %>%
        mutate(yardline_100 = if_else(is.na(yardline_100),
                as.numeric(regmatches(side_of_field,gregexpr("[0-9]*\\.?[0-9]+",side_of_field))),
                yardline_100)) %>%
        ungroup() %>%
        select(receive_2h_ko, half_seconds_remaining, game_seconds_remaining,
               Diff_Time_Ratio, score_differential, down, ydstogo, yardline_100,
               home, posteam_timeouts_remaining, defteam_timeouts_remaining)
}