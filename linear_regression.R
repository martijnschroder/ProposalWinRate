# linear regression
train_set <- train_set_orig %>% na.omit() %>% select(-code)
test_set <- test_set_orig %>% na.omit() %>% select(-code)

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# determine overall win rate
mu = mean(train_set$stage)

# randomise to 0 and 1 with p = mu

# determine RMSE
naive_rmse <- RMSE(test_set$stage, mu)

predictions <- rep(mu, nrow(test_set))
rmse_naive <- RMSE(test_set$stage, predictions)

rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
rmse_results
# add accuracy ?

# account for account: Y_i= \mu + b_a + \epsilon_i


account_avg <- train_set_linreg %>%
  group_by(account) %>%
  summarise(b_a = mean(stage - mu))

predicted_ratings <- mu + test_set_linreg$stage





