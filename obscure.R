
# exploring obsucre films due to number of ratings

movie_titles <- edx_train %>% 
  select(movieId, title) %>%
  distinct()

# most rated
edx_train %>% count(movieId) %>% 
  left_join(movie_avgs) %>% left_join(movie_titles, by="movieId") %>% arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10)

#least rated
edx_train %>% count(movieId) %>% 
  left_join(movie_avgs) %>% left_join(movie_titles, by="movieId") %>% arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10)

# introducing penalized least squares

lambda <- 3
mu <- mean(edx_train$rating)
movie_reg_avgs <- edx_train %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())

# best movies
edx_train %>% count(movieId) %>% left_join(movie_reg_avgs) %>% left_join(movie_titles, by="movieId") %>% 
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10)

#worst movies
edx_train %>% count(movieId) %>% left_join(movie_reg_avgs) %>% left_join(movie_titles, by="movieId") %>% 
  arrange((b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10)

predicted_ratings <- edx_train %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

model_3_rmse <- RMSE(predicted_ratings, edx_train$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse))

# identifying lambda
lambdas <- seq(0, 10, 0.25)

mu <- mean(edx_train$rating)
just_the_sum <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- edx_test %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, edx_test$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]


## user and movie effect

lambdas <- seq(0, 10, 1)

rmses2 <- sapply(lambdas, function(l){
  
  print(l)
  mu <- mean(edx_train$rating)
  
  b_i <- edx_train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx_train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    edx_test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, edx_test$rating))
})

qplot(lambdas, rmses2)  
