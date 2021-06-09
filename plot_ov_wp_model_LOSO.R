plot_ov_wp_model_LOSO <- function(cv_results) {
    # A function to bin and plot predicted vs actual win probabilities for the
    # purpose of cross validation of an overtime win probability model
    
    # Input:
    # cv_results: tibble, containing nfl plays labeled with the game result
    # and a model predicted win probability with the following columns:
    #   wp: num, model assigned win probability
    #   label: int, 1 if possessing team won, otherwise 0
    
    # Outputs:
    # Prints the correlation coefficient between observed and predicted win probability
    # Plot of observed vs predicted win probability binned by 5% win probability
    # increments
    
    cv_bins <- cv_results %>%
        # create 5% bins
        mutate(bin_pred_prob = round(wp/0.05) * 0.05) %>%
        group_by(bin_pred_prob) %>%
        # calculate actual win probability by counting number of winning plays
        # as a percentage of number of total plays
        summarize(n_plays = n(),
                  n_wins = sum(label),
                  bin_actual_prob = n_wins/n_plays)
    
    # print the correlation, rounded to 4 decimals
    print(paste("Correlation:",round(cor(cv_bins$bin_actual_prob,cv_bins$bin_pred_prob),4)))
    
    # generate scatterplot of predicted vs observed win probability in 5% bins
    cv_bins %>%
        # initialize plot
        ggplot() +
        # plot predicted (y) vs actual (x) wp, sized by number of plays
        geom_point(aes(x=bin_actual_prob,y=bin_pred_prob,size=n_plays)) +
        # add a line with unit slope for visual comparison
        geom_abline(slope=1,intercept=0,color='blue') +

        # create and position labels
        labs(x="Observed Win Probability",
             y="Predicted Win Probability",
             title="Overtime WP Model for 1999-2020 Seasons",
             size="Number of Plays") +
        theme_bw() +
        theme(plot.title = element_text(hjust=0.5),
              axis.title.x = element_text(size=12),
              axis.title.y = element_text(size=12),
              axis.text.x = element_text(angle=90,hjust=0),
              legend.position = "bottom") +

        # specify that plots range from 0 to 1 and are square
        expand_limits(x=c(0,1),y=c(0,1)) +
        coord_equal()

}