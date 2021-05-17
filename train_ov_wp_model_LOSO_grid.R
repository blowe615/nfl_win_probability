# Script to iterate through different number of rounds of xgboost for an
# overtime play-by-play model

# generate vector of number of rounds
n_rounds_vec <- seq(10,100,10)

cv_results_nr <- map_dfr(n_rounds_vec,function(x){
    cv_data<-train_ov_wp_model_LOSO(ov_plays,x)
    cv_data <- cv_data %>%
        mutate(nrounds = x)
})