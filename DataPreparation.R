
## Data loading and preparation scrip
# In order to avoid having to download each time the data sets from the internet, 
# the movies and ratings datasets have been saved locallu in a "MovieLensData.RData" which
# can be retrieved at each R session using the load command. The file is stored in a project
# subdirectory named ml-10M100K
# the rest of the script is the data preparation as provided in the project instructions


library(tidyverse)
library(caret)

load("ml-10M100K/MovieLensData.RData") # loading the data from the the RData file

colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(ratings, movies, test_index, temp, movielens, removed)