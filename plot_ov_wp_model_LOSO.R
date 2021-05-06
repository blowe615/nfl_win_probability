plot_ov_wp_model_LOSO <- function(cv_results) {
    # A function to bin and plot predicted vs actual win probabilities for the
    # purpose of cross validation of an overtime win probability model
    
    # Input:
    # cv_results: tibble, containing nfl plays labeled with the game result
    # and a model predicted win probability with the following columns:
    #   wp: num, model assigned win probability
    #   label: int, 1 if possessing team won, otherwise 0
    
    # Output:
    # Plot of observed vs predicted win probability binned by 5% win probability
    # increments
    
    cv_results %>%
        # create 5% bins
        mutate(bin_pred_prob = round(wp/0.05) * 0.05) %>%
        group_by(bin_pred_prob) %>%
        # calculate actual win probability by counting number of winning plays
        # as a percentage of number of total plays
        summarize(n_plays = n(),
                  n_wins = sum(label),
                  bin_actual_prob = n_wins/n_plays) %>%
        # modify quarter labels for plotting
        #mutate(qtr = recode(qtr,
        #    "1" = "1st Quarter", "2" = "2nd Quarter",
        #    "3" = "3rd Quarter", "4" = "4th Quarter"
        #)) %>%
        # initialize plot
        ggplot() +
        # plot actual (y) vs predicted (x) wp, sized by number of plays
        geom_point(aes(x=bin_pred_prob,y=bin_actual_prob,size=n_plays)) +
        # add a line with unit slope for visual comparison
        geom_abline(slope=1,intercept=0,color='blue') +
        
        # create and position labels
        labs(x="Predicted Win Probability",
             y="Observed Win Probability",
             title="Overtime WP Model for 1999-2020 Seasons",
             size="Number of Plays") +
        theme_bw() +
        theme(plot.title = element_text(hjust=0.5),
              axis.title.x = element_text(size=12),
              axis.title.y = element_text(size=10),
              axis.text.x = element_text(angle=90,hjust=0),
              legend.position = "bottom") +
        
        # specify that plots range from 0 to 1 and are square
        expand_limits(x=c(0,1),y=c(0,1)) +
        coord_equal()
        # arrange plots in a 1x4 table
        #facet_wrap(~qtr,ncol=4)
}