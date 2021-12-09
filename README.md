## Table of Contents
* [Installations](#installations)
* [Project Motivation](#project-motivation)
* [File Descriptions](#file-descriptions)
* [Results](#results)
* [Acknowledgements](#acknowledgements)

## Installations
The following open-source software packages were used to create this visualization:
- R studio
- tidyverse (collection of R packages)
- Tableau Desktop 2020.4 (Can also use Tableau Public)

## Project Motivation
I recently took some Coursera courses on Tableau and R as part of my ongoing efforts to expand my Data Science skills.  I decided to recreate the NFL Win Probability models created by [Ben Baldwin](https://github.com/guga31bb) and [Sebastian Carl](https://github.com/mrcaseb) to practice and apply these newly learned skills.

## File Descriptions
* `ETL_wp_plotting.R`: R script to Extract pbp data from AWS server, Transform it for the win probability plot and Load it into a different table in the server.
* `win_prob_plot.twb`: Tableau Desktop workbook to make the interactive win probability plot.
* `teams_colors_logos.csv`: CSV file containing hex codes for NFL team colors and URLs for team logos.

#### Regulation
* `clean_plays.R`: R function to filter and mutate play-by-play data to serve as input to a win probabiity model.
* `export_wp.R`: R function to join win probability predictions to play-by-play data for the purposes of generating win probability plots.  This function also adds extra rows for cleaner plotting.
* `train_wp_model.R`: R script to load, clean and filter play-by-play data for the 1999-2020 season and then train the XGBoost win probability model. To be run after cross validation.
* `train_wp_model_LOSO.R`: R script to load, clean and filter play-by-play data for the 1999-2020 season and then train the XGBoost win probability model using Leave One Season Out (LOSO) cross validation.
* `plot_wp_model_LOSO.R`: R function to mutate output of `train_wp_model_LOSO` to make plots for cross validation.
* `wp_model.RDS`: Trained XGBoost win probability model that you can load instead of training your own.

#### Overtime
* `clean_ov_plays.R`: R function to filter and mutate overtime play-by-play data to serve as input to an overtime win probabiity model.
* `export_wp_ov.R`: R function to join win probability predictions to overtime play-by-play data for the purposes of generating win probability plots.  This function also adds extra rows for cleaner plotting.
* `get_ov_plays.R`: R function to filter play-by-play data down to overtime plays for use in training an overtime win probability model.
* `ov_wp_model.RDS`: trained XGBoost overtime win probability model.
* `plot_ov_wp_model_LOSO.R`: R function to bin and plot predicted vs actual win probabilities for the purpose of cross validation of an overtime win probability model.
* `plot_ov_wp_model_LOSO_grid.R`: R function to bin and plot predicted vs actual win probabilities for a grid of model parameters. To be used during tuning.
* `train_ov_wp_model.R`: R function to train an XGBoost win probability model on all seasons of overtime play-by-play data.
* `train_ov_wp_model_LOSO.R`: R function to perform Leave One Season Out (LOSO) cross validation on NFL overtime play-by-play (pbp) data using XGBoost.
* `train_ov_wp_model_LOSO_grid.R`: R function to perform grid search through XGBoost parameters to tune an overtime play-by-play model
* `train_tune_plot_ov_model.R`: Script to train, tune, and evaluate an overtime wp model.  It calls `train_ov_wp_model_LOSO_grid` and `plot_ov_wp_model_LOSO_grid`.



## Results
Running `train_wp_model_LOSO.R` will print out the training status for the model, including the beginning and ending of each season and the logloss evaluation metric for each pass of the XGBoost algorithm.

![](https://github.com/blowe615/nfl_win_probability/blob/master/training_output.png)

Running `plot_wp_model_LOSO.R` will generate plots comparing observed and predicted win probabilities broken up by quarter and 5% bins.

![](https://github.com/blowe615/nfl_win_probability/blob/master/winprobCVplots.png)

Running `get_ov_plays.R` followed by `train_tune_plot_ov_model.R` will perform a grid search of the parameters specified in `train_tune_plot_ov_model` to train an XGBoost model on overtime play-by-play data. The results of the grid search will be plotted and displayed in a table so you can identify the model parameters that result in the lowest RMSE or highest correlation.

![](https://github.com/blowe615/nfl_win_probability/blob/master/grid_search_plot.png)

![](https://github.com/blowe615/nfl_win_probability/blob/master/grid_search_results.png)

An example of the win probability chart created in Tableau using Super Bowl LV.  The interactive visualization can be found [here](https://public.tableau.com/profile/brandon.lowe8009#!/vizhome/win_prob_plot/Dashboard1?publish=yes).

![](https://github.com/blowe615/nfl_win_probability/blob/master/SB_win_prob_chart.png)

## Acknowledgements
It goes without saying that my work would not have been possible without the work of Ben Baldwin and Sebastian Carl and their [nflfastR](https://github.com/mrcaseb/nflfastR) package.  Their work is based on the original NFL play-by-play scraping package - [nflscrapR](https://github.com/maksimhorowitz/nflscrapR) - created by Maksim Horowitz, Ronald Yurko, and Samuel Ventura.

I also found this [Open Source Football](https://www.opensourcefootball.com/posts/2020-09-28-nflfastr-ep-wp-and-cp-models/#wp-model-features) article by Ben Baldwin very helpful in understanding the Win Probability model inputs and training.

The following Coursera courses helped introduce me to Tableau and R:
[Data Visualization with Tableau](https://www.coursera.org/learn/data-visualization-tableau/home/welcome)
[R Programming](https://www.coursera.org/learn/r-programming/home/welcome)

And as always, thanks to many posts on Stack Overflow and Tableau Community.
