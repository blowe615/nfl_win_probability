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
* `clean_plays.R`: R function to filter and mutate play-by-play data to serve as input to a win probabiity model.
* `export_wp.R`: R function to join win probability predictions to play-by-play data for the purposes of generating win probability plots.  This function also adds extra rows for cleaner plotting.
* `train_wp_model.R`: R script to load, clean and filter play-by-play data for the 1999-2020 season and then train the XGBoost win probability model. To be run after cross validation.
* `train_wp_model_LOSO.R`: R script to load, clean and filter play-by-play data for the 1999-2020 season and then train the XGBoost win probability model using Leave One Season Out (LOSO) cross validation.
* `plot_wp_model_LOSO.R`: R function to mutate output of `train_wp_model_LOSO` to make plots for cross validation.
* `win_prob_plot.twb`: Tableau Desktop workbook to make the interactive win probability plot.
* `SB_plays.csv`: CSV file containing play-by-play data with win probability from Super Bowl LV between the Kansas City Chiefs and Tampa Bay Buccaneers.  This is the output of `export_wp`.
* `teams_colors_logos.csv`: CSV file containing hex codes for NFL team colors and URLs for team logos.

## Results
Running `train_wp_model_LOSO.R` will print out the training status for the model, including the beginning and ending of each season and the logloss evaluation metric for each pass of the XGBoost algorithm.

![](https://github.com/blowe615/nfl_win_probability/blob/master/training_output.png)

Running `plot_wp_model_LOSO.R` will generate plots comparing observed and predicted win probabilities broken up by quarter and 5% bins.

![](https://github.com/blowe615/nfl_win_probability/blob/master/winprobCVplots.png)

An example of the win probability chart created in Tableau using Super Bowl LV.  The interactive visualization can be found [here](https://public.tableau.com/profile/brandon.lowe8009#!/vizhome/win_prob_plot/Dashboard1?publish=yes).

![](https://github.com/blowe615/nfl_win_probability/blob/master/SB_win_prob_chart.png)

## Acknowledgements
It goes without saying that my work would not have been possible without the work of Ben Baldwin and Sebastian Carl and their [nflfastR](https://github.com/mrcaseb/nflfastR) package.  Their work is based on the original NFL play-by-play scraping package - [nflscrapR](https://github.com/maksimhorowitz/nflscrapR) - created by Maksim Horowitz, Ronald Yurko, and Samuel Ventura.

I also found this [Open Source Football](https://www.opensourcefootball.com/posts/2020-09-28-nflfastr-ep-wp-and-cp-models/#wp-model-features) article by Ben Baldwin very helpful in understanding the Win Probability model inputs and training.

The following Coursera courses helped introduce me to Tableau and R:
[Data Visualization with Tableau](https://www.coursera.org/learn/data-visualization-tableau/home/welcome)
[R Programming](https://www.coursera.org/learn/r-programming/home/welcome)

And as always, thanks to many posts on Stack Overflow and Tableau Community.
