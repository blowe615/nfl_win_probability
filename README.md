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
Files are organized in the following directories

**dashboard**

The Tableau notebook file and team colors CSV for plotting
* `win_prob_plot.twb`: Tableau Desktop workbook to make the interactive win probability dashboard.
* `teams_colors_logos.csv`: CSV file containing hex codes for NFL team colors and URLs for team logos.

**ETL**

Files to extract, clean, or load play-by-play data
* `ETL_wp_plotting.R`: R script to Extract pbp data from AWS server, Transform it for the win probability plot and Load it into a different table in the server.
* `get_ov_plays.R`: R function to filter play-by-play data down to overtime plays for use in training an overtime win probability model.
* `clean_plays.R`: R function to filter and mutate play-by-play data to serve as input to a win probabiity model.
* `clean_ov_plays.R`: R function to filter and mutate overtime play-by-play data to serve as input to an overtime win probabiity model.
* `export_wp.R`: R function to join win probability predictions to play-by-play data for the purposes of generating win probability plots.  This function also adds extra rows for cleaner plotting.
* `export_wp_ov.R`: R function to join win probability predictions to overtime play-by-play data for the purposes of generating win probability plots.  This function also adds extra rows for cleaner plotting.

**models**

Saved model files
* `wp_model.RDS`: Trained XGBoost win probability model that you can load instead of training your own.
* `ov_wp_model.RDS`: Trained XGBoost overtime win probability model.

**training**

Functions for training or cross validation of regulation or overtime play-by-play data
* `train_wp_model.R`: R script to load, clean and filter play-by-play data for the 1999-2020 season and then train the XGBoost win probability model.
* `train_wp_model_LOSO.R`: R script to load, clean and filter play-by-play data for the 1999-2020 season and then train the XGBoost win probability model using Leave One Season Out (LOSO) cross validation.
* `train_ov_wp_model.R`: R function to train an XGBoost win probability model on all seasons of overtime play-by-play data. To be run after cross validation.
* `train_ov_wp_model_LOSO.R`: R function to perform Leave One Season Out (LOSO) cross validation on NFL overtime play-by-play (pbp) data using XGBoost.
* `train_ov_wp_model_LOSO_grid.R`: R function to perform grid search through XGBoost parameters to tune an overtime play-by-play model
* `train_tune_plot_ov_model.R`: Script to train, tune, and evaluate an overtime wp model.  It calls `train_ov_wp_model_LOSO_grid` and `plot_ov_wp_model_LOSO_grid`.

**plotting**

Functions for plotting cross validation and tuning results
* `plot_wp_model_LOSO.R`: R function to mutate output of `train_wp_model_LOSO` to make plots for cross validation.
* `plot_ov_wp_model_LOSO.R`: R function to bin and plot predicted vs actual win probabilities for the purpose of cross validation of an overtime win probability model.
* `plot_ov_wp_model_LOSO_grid.R`: R function to bin and plot predicted vs actual win probabilities for a grid of model parameters. To be used during tuning.

## Results
Running `train_wp_model_LOSO.R` will print out the training status for the model, including the beginning and ending of each season and the logloss evaluation metric for each pass of the XGBoost algorithm.

![](https://github.com/blowe615/nfl_win_probability/blob/master/images/training_output.png)

Running `plot_wp_model_LOSO.R` will generate plots comparing observed and predicted win probabilities broken up by quarter and 5% bins.

![](https://github.com/blowe615/nfl_win_probability/blob/master/images/winprobCVplots.png)

Running `get_ov_plays.R` followed by `train_tune_plot_ov_model.R` will perform a grid search of the parameters specified in `train_tune_plot_ov_model` to train an XGBoost model on overtime play-by-play data. The results of the grid search will be plotted and displayed in a table so you can identify the model parameters that result in the lowest RMSE or highest correlation.

![](https://github.com/blowe615/nfl_win_probability/blob/master/images/grid_search_plot.png)

![](https://github.com/blowe615/nfl_win_probability/blob/master/images/grid_search_results.png)

An example of the win probability dashboard created in Tableau using the 2013 NFC Championship Game.  The interactive visualization can be found [here](https://public.tableau.com/app/profile/brandon.lowe8009/viz/WinProbabilityDashboard/WinProbabilityDashboard?publish=yes).

![](https://github.com/blowe615/nfl_win_probability/blob/master/images/NFCCG_dashboard.png)

The dashboard shows the win probability over the course of the game as well as a list of the 5 plays that had the largest win probability added (WPA). Hovering over any portion of the plot will bring up the game scenario and the play at that time.  Hovering on any of the top plays will highlight on the plot where the play occurred in the game.  The filters on the left side of the dashboard allow the user to pick which game to view.

## Acknowledgements
It goes without saying that my work would not have been possible without the work of Ben Baldwin and Sebastian Carl and their [nflfastR](https://github.com/mrcaseb/nflfastR) package.  Their work is based on the original NFL play-by-play scraping package - [nflscrapR](https://github.com/maksimhorowitz/nflscrapR) - created by Maksim Horowitz, Ronald Yurko, and Samuel Ventura.

I also found this [Open Source Football](https://www.opensourcefootball.com/posts/2020-09-28-nflfastr-ep-wp-and-cp-models/#wp-model-features) article by Ben Baldwin very helpful in understanding the Win Probability model inputs and training.

The following Coursera courses helped introduce me to Tableau and R:
[Data Visualization with Tableau](https://www.coursera.org/learn/data-visualization-tableau/home/welcome)
[R Programming](https://www.coursera.org/learn/r-programming/home/welcome)

And as always, thanks to many posts on Stack Overflow and Tableau Community.
