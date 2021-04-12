clean_plays <- function(pbp_data) {
    # A function that cleans play-by-play data so that it is in the proper format
    # for a win probability model
    
    # Input:
    # pbp_data: tibble containing at minimum the following 15 columns in any order:
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
    
    # Output:
    # tibble containing 11 columns in the following order:
    #   receive_2h_ko: int, 1 if play occurs in the first half and possessing team
    #                       not receives second half kickoff, otherwise 0
    #   half_seconds_remaining
    #   game_seconds_remaining
    #   Diff_Time_Ratio: score_differential/exp(-4 * elapsed_share))
    #   score_differential
    #   down
    #   ydstogo
    #   yardline_100
    #   home: int, 1 if possessing team is also the home team, otherwise 0
    #   posteam_timeouts_remaining
    #   defteam_timeouts_remaining
    
    # select the columns needed for cleaning
    pbp_data %>%
        select(game_id,home_team,away_team,posteam,defteam,half_seconds_remaining,
               game_seconds_remaining,score_differential,down,ydstogo,yardline_100,
               side_of_field,posteam_timeouts_remaining,defteam_timeouts_remaining,
               qtr) %>%
        
        # group by game_id if multiple games are in input
        group_by(game_id) %>%
        
        # add 4 columns that were not included in the input data
        mutate(receive_2h_ko = if_else(qtr <= 2 & posteam==first(na.omit(defteam)),1,0),
               home = if_else(posteam==home_team,1,0),
               elapsed_share = (3600 - game_seconds_remaining)/3600,
               Diff_Time_Ratio = score_differential/exp(-4 * elapsed_share)) %>%
        
        # remove plays with crucial missing data
        filter(!is.na(down),!is.na(score_differential),qtr<=4) %>%
        
        # if yardline_100 is null, use a RegEx to pull yardline_100 from side_of_field
        mutate(yardline_100 = if_else(is.na(yardline_100),
                as.numeric(regmatches(side_of_field,gregexpr("[0-9]*\\.?[0-9]+",side_of_field))),
                yardline_100)) %>%
        ungroup() %>%
        
        # select and return columns in the order required as input to win probability model
        select(receive_2h_ko, half_seconds_remaining, game_seconds_remaining,
               Diff_Time_Ratio, score_differential, down, ydstogo, yardline_100,
               home, posteam_timeouts_remaining, defteam_timeouts_remaining)
}