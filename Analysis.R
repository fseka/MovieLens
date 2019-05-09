library(tidyverse)
library(caret)

# Creating the Root Mean Square Error  evaluation function to evaluate the performance of our different approaches

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


# We will partiion the edx dataframe in train and test sets. The validation dataframe will be kept for th final evaluation of our algorihtm.

set.seed(1)
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in validation set are also in edx set

edx_test <- temp %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, edx_test)
edx_train <- rbind(edx_train, removed)

rm(temp, removed)



# Let's begin with a simple approach to check that our data is load correctly. We will predict the rating of a movie using the rating mean value from the training dataset. This approach does not consider the specificity of the user.

# Calculate the mean of the ratings
mu_hat <- mean(edx_train$rating)

# Use the RMSE function created to evaluate the error. As expected, this strategy will not provide the best RMSE.
simple_approach_rmse<-RMSE(validation$rating, mu_hat)

# We will create a dataframe to store our different results, for comparison purposes.
rmse_results <- data_frame(method = "Naive Mean approach", RMSE = simple_approach_rmse)

# As learned from the course, we will attempt to model the movie effect, where some movies simply get rated better than others. This can be modelled through a bias term

mu <- mean(edx_train$rating) 
movie_avgs <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
# If we now test the performance of this approach
predicted_ratings <- edx_test %>% mutate(rating=mu) %>% left_join(movie_avgs, by='movieId') %>% mutate(prediction=rating+b_i)
movie_effect_result<- RMSE(edx_test$rating,predicted_ratings$prediction)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = movie_effect_result))

# As we can see, implementing the movie effect already brings some improvement to predictions.
# A similar approach can be implemented to account for the "user variability", in the sense that different user will be more or less severe when rating a film. This parameter will be different from one user to the other.

user_avgs <- edx_train %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- edx_test %>% 
  mutate(rating=mu) %>% 
  left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

user_effect_result<- RMSE(edx_test$rating,predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie and User Effect Model",  
                                     RMSE = user_effect_result))

# Taking into account the user effect significantly improves the algorithm's performance, as the additional decrease in RMSE can show.

lambda <- 1.5
mu <- mean(edx_train$rating)
movie_reg_avgs <- edx_train %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())
predicted_ratings <- edx_test %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
model_3_rmse <- RMSE(predicted_ratings, edx_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse))



