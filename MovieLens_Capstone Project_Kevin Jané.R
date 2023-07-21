#####
# Libraries Installation
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(dslabs)) install.packages("dslabs")
if(!require(stringr)) install.packages("stringr")
if(!require(forcats)) install.packages("forcats")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
#####
# Loading all needed libraries
library(dplyr)
library(dslabs)
library(tidyverse)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(data.table)
library(caret)

#####
# Initial code provided by edX.
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
ratings <- fread(text = gsub("::", "\t", 
                             readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))
movies <- str_split_fixed(
  readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% 
  mutate(movieId = as.numeric(movieId),
         title = as.character(title),
         genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")
# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, 
                                  times = 1, p = 0.1, 
                                  list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#####
# We create a train and test datasets for modeling training
# Test_set set will be 10% of Edx data
test_index_2 <- createDataPartition(y = edx$rating, 
                                    times = 1, p = 0.1, 
                                    list = FALSE)
train_set <- edx[-test_index_2,]
temp_2 <- edx[test_index_2,]
# Make sure userId and movieId in test_set set are also in train_set
test_set <- temp_2 %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")
# Add rows removed from test_set set back into train_set
removed_2 <- anti_join(temp_2, test_set)
train_set <- rbind(train_set, removed_2)
#####
# Initial data Exploration
# A first look
### Edx database
edx %>% # Numbers of movies and users
  summarize(Users = n_distinct(userId),
            Movies = n_distinct(movieId))

sapply(edx, function(x) sum(is.na(x))) # If there is any NA in the dataset

### Validation Database
validation %>% # Numbers of movies and users
  summarize(Users = n_distinct(userId),
              Movies = n_distinct(movieId))

sapply(validation, function(x) sum(is.na(x))) # If there is any NA in the dataset

### Train_Set
train_set %>% # Numbers of movies and users
  summarize(Users = n_distinct(userId),
              Movies = n_distinct(movieId))

sapply(train_set, function(x) sum(is.na(x))) # If there is any NA in the dataset

### Test_Set
test_set %>%  # Numbers of movies and users
  summarize(Users = n_distinct(userId),
              Movies = n_distinct(movieId))

sapply(test_set, function(x) sum(is.na(x))) # If there is any NA in the dataset

#####
# Wrangling Data Process

## 1. Convert timestamp variable to a more readable date format;

# edx database
edx$date <- as.POSIXct(edx$timestamp, 
                       origin="1970-01-01")
# validation database
validation$date <- as.POSIXct(validation$timestamp, 
                              origin="1970-01-01")
# train_set database
train_set$date <- as.POSIXct(train_set$timestamp, 
                             origin="1970-01-01")
# test_set database
test_set$date <- as.POSIXct(test_set$timestamp, 
                            origin="1970-01-01")

## 2. Extract the month and the year from the last step and we convert the columns into the desired data type, i.e, ```yearofRate``` should be numeric;
# edx database
edx$yearOfRate <- format(edx$date,"%Y")
edx$yearOfRate <- as.numeric(edx$yearOfRate)
edx$monthOfRate <- format(edx$date,"%m")
edx$monthOfRate <- as.numeric(edx$monthOfRate)
# validation database
validation$yearOfRate <- format(validation$date,"%Y")
validation$yearOfRate <- as.numeric(validation$yearOfRate)
validation$monthOfRate <- format(validation$date,"%m")
validation$monthOfRate <- as.numeric(validation$monthOfRate)
# train_set database
train_set$yearOfRate <- format(train_set$date,"%Y")
train_set$yearOfRate <- as.numeric(train_set$yearOfRate)
train_set$monthOfRate <- format(train_set$date,"%m")
train_set$monthOfRate <- as.numeric(train_set$monthOfRate)
# test_set database
test_set$yearOfRate <- format(test_set$date,"%Y")
test_set$yearOfRate <- as.numeric(test_set$yearOfRate)
test_set$monthOfRate <- format(test_set$date,"%m")
test_set$monthOfRate <- as.numeric(test_set$monthOfRate)

## 3. Extract the release year for each movie from the title;
# edx database
edx <- edx %>% 
  mutate(release = as.numeric(str_sub(title,-5,-2)))
# validation database
validation <- validation %>% 
  mutate(release = as.numeric(str_sub(title,-5,-2)))
# train_set database
train_set <- train_set %>% 
  mutate(release = as.numeric(str_sub(title,-5,-2)))
# test_set database
test_set <- test_set %>% 
  mutate(release = as.numeric(str_sub(title,-5,-2)))

## 4. Then, we remove the unnecessary columns on the databases.
# edx database
edx <- edx %>% 
  select(-timestamp, -date)
# validation database
validation <- validation %>% 
  select(-timestamp, -date)
# train_set database
train_set <- train_set %>% 
  select(-timestamp, -date)
# test_set database
test_set <- test_set %>% 
  select(-timestamp, -date)
# Processed database
head(edx, 5)
head(validation, 5)
head(train_set, 5)
head(test_set, 5)

#####
# Modeling
## Loss Function - RMSE {#RMSE}
RMSE <- function(true_ratings = NULL, predicted_ratings = NULL) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
} # The RMSE used in this project

## Naive Model - A first model.
muhat <- mean(train_set$rating) # Average rating of all movies
muhat

naive_rmse <- RMSE(test_set$rating, muhat) # Prediction of all unknown ratings
naive_rmse
results <- data.frame(model="Naive Model", RMSE=naive_rmse) # We create a database of the results

## Movie Effect Model {#MModel}

muhat <- mean(train_set$rating) # Average rating of all movies
muhat
movie_avgs <- train_set %>% # Average rating by movie
  group_by(movieId) %>%
  summarize(b_i = mean(rating - muhat))

movie_eff_pred <- muhat + test_set %>% # Calculating the movie effect
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
rmse_movie <- RMSE(movie_eff_pred, test_set$rating) # Prediction of all unknown rating with movie effect
rmse_movie
results <- results %>% # We add the result to the results database
  add_row(model="Movie Effect Model", RMSE=rmse_movie)

## Movie + User Effects Model 
muhat <- mean(train_set$rating) # Average rating of all movies
muhat
user_avgs <- train_set %>% # Average rating by user
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - muhat - b_i))
user_eff_pred <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = muhat + b_i + b_u) %>%
  pull(pred)
rmse_user <- RMSE(user_eff_pred, test_set$rating) # Prediction of all unknown rating with movie+user effect
rmse_user
results <- results %>% # We add the result to the results database
  add_row(model="Movie + User Effects Model", RMSE=rmse_user)

## Penalized least squares - Regularization
# In this case, we are going to use the best model that we had until now. In this case, Movie + User Effects Model.
### Penalized Movie + User Effects Model
muhat <- mean(train_set$rating) # Average rating of all movies
muhat
lambdas <- seq(0, 10, 0.5)
# Modeling with Regularized Movie + User Effect Model
rmses_m_u <- sapply(lambdas, function(l){
  # Average rating by movie
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - muhat)/(n()+l))
  # Average rating by user
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - muhat)/(n()+l))
  # RMSE prediction on the test_set database
  predicted_ratings <-
    test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = muhat + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})
rmse_movie_user_pen <- min(rmses_m_u)
rmse_movie_user_pen
results <- results %>% # We add the result to the results database
  add_row(model="Regularized Movie + User Effect Model", RMSE=rmse_movie_user_pen)
lambdas[which.min(rmses_m_u)] # The lambda value that minimize the RMSE

#####
## Validation {#Validation}
muhat <- mean(edx$rating) # Average rating of all movies
muhat
# After trying 4 models, we are going to validated with the best model, that is: Penalized Movie + User Effects Model
lambdas <- seq(0, 10, 0.1)
# Modeling with Regularized Movie + User Effect Model
rmses_m_u_val <- sapply(lambdas, function(l){
  # Average rating by movie
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - muhat)/(n()+l))
  # Average rating by user
  b_u <- edx %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - muhat)/(n()+l))
  # RMSE prediction on the validation database
  predicted_ratings <-
    validation %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = muhat + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})
rmse_movie_user_pen_val <- min(rmses_m_u_val)
rmse_movie_user_pen_val
results <- results %>% # We add the result to the results database
  add_row(model="Regularized Movie + User Effect Model Validated", RMSE=rmse_movie_user_pen_val)
#####
results
