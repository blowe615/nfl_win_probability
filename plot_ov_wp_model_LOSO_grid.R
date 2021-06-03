plot_ov_wp_model_LOSO_grid <- function(cv_results,sort_by="COR",param_group) {
    # A function to bin and plot predicted vs actual win probabilities for the
    # purpose of cross validation of an overtime win probability model
    
    # Input:
    # cv_results: tibble, containing nfl plays labeled with the game result
    # and a model predicted win probability with the following columns:
    #   wp: num, model assigned win probability
    #   label: int, 1 if possessing team won, otherwise 0
    # param_group: list, contains names of tuning parameters that were modified,
    #               to be used in facet_wrap
    
    # Outputs:
    # Prints the correlation coefficient between observed and predicted win probability
    # Plot of observed vs predicted win probability binned by 5% win probability
    # increments
    
    cv_bins <- cv_results %>%
        # create 5% bins
        mutate(bin_pred_prob = round(wp/0.025) * 0.025) %>%
        # group by tuning parameters and bins
        group_by(booster, objective, eval_metric, eta, gamma, subsample,
                 colsample_bytree, max_depth, min_child_weight,
                 max_delta_step, nrounds, bin_pred_prob) %>%
        # calculate actual win probability by counting number of winning plays
        # as a percentage of number of total plays
        summarize(n_plays = n(),
                  n_wins = sum(label),
                  bin_actual_prob = n_wins/n_plays) %>%
        ungroup()
    
    # create a separate tbl with correlations, rounded to 4 decimals
    cv_bin_cor <- cv_bins %>%
        group_by(booster, objective, eval_metric, eta, gamma, subsample,
                 colsample_bytree, max_depth, min_child_weight,
                 max_delta_step, nrounds) %>%
        summarize(COR = round(cor(bin_actual_prob,bin_pred_prob),4),
                  RMSE = round(rmse(bin_actual_prob,bin_pred_prob),4)) %>%
        ungroup()
    
    # print the correlations and RMSE for each parameter combination, in 
    # descending order
    # sort the cv results based on the user input, either correlation or RMSE
    # sort by correlation if sort_by == "COR"
    ifelse(sort_by=="COR",
        print(as.data.frame(arrange(cv_bin_cor,-COR)%>%head(10))),
        # otherwise sort by RMSE
        print(as.data.frame(arrange(cv_bin_cor,RMSE)%>%head(10))))
    
    # generate scatterplot of predicted vs observed win probability in 2.5% bins
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
              axis.title.y = element_text(size=10),
              axis.text.x = element_text(angle=90,hjust=0),
              legend.position = "bottom") +

        # specify that plots range from 0 to 1 and are square
        expand_limits(x=c(0,1),y=c(0,1)) +
        coord_equal() +
        # facet by the parameters passed by param_group so each plot is labelled
        # with its tuning parameter combination
        facet_wrap(as.formula(paste("~",paste(param_group,collapse = " + "))),
                   labeller=label_both,ncol=4)
        

}