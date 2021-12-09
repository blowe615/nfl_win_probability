clean_ov_plays <- function(pbp_data){
    # A function that cleans play-by-play data so that it is in the proper format
    # for an overtime win probability model
    
    # Input:
    # pbp_data: tibble containing at minimum the following 14 columns of nfl 
    # play-by-play data in any order:
    #   game_id: chr, unique identifier for each game
    #   home_team: chr, home team abbreviation
    #   posteam: chr, abbreviation of team possessing the ball for each play
    #   defteam: chr, abbreviation of team not possessing the ball for each play
    #   game_seconds_remaining: int, number of seconds remaining in regulation (0-3600)
    #   score_differential: int, difference between possessing team score and defending team score
    #   down: int, down of each play (1-4)
    #   ydstogo: int, yards to first down
    #   yardline_100: int, yards from opponent's end zone
    #   side_of_field: chr, abbreviation of the team whose side the ball is on
    #   posteam_timeouts_remaining: int, number of possessing team's timeouts
    #   defteam_timeouts_remaining: int, number of defending team's timeouts
    #   qtr: int, quarter number in overtime (5)
    #   drive: int, drive number in the game
    
    # Output:
    # tibble containing 9 columns in the following order:
    #   first_drive: int, 1 if play occurs in the first drive of overtime, otherwise 0
    #   game_seconds_remaining
    #   score_differential
    #   down
    #   ydstogo
    #   yardline_100
    #   home: int, 1 if possessing team is also the home team, otherwise 0
    #   posteam_timeouts_remaining
    #   defteam_timeouts_remaining
    
    # select columns needed for cleaning, mutating and training
    pbp_data %>%
        select(game_id,home_team,posteam,defteam,game_seconds_remaining,
               score_differential,down,ydstogo,yardline_100,side_of_field,
               posteam_timeouts_remaining,defteam_timeouts_remaining,qtr,drive) %>%
        filter(qtr>4) %>%
        # group by game_id to perform mutations
        group_by(game_id) %>%
        mutate(first_drive = if_else(drive==first(na.omit(drive)),1,0),
               home = if_else(posteam==home_team,1,0)) %>%
        # filter out plays with crucial missing data
        filter(!is.na(down),!is.na(score_differential)) %>%
        # if yardline_100 is null, use a RegEx to pull yardline_100 from side_of_field
        mutate(yardline_100 = if_else(is.na(yardline_100),
                                      as.numeric(regmatches(side_of_field,gregexpr("[0-9]*\\.?[0-9]+",side_of_field))),
                                      yardline_100)) %>%
        ungroup() %>%
        
        # select and return columns in the order required as input to win probability model
        select(first_drive, game_seconds_remaining,
               score_differential, down, ydstogo, yardline_100,
               home, posteam_timeouts_remaining, defteam_timeouts_remaining)
}