
# Movielens project

######### Part 1 ###############
# Create edx set, validation set
################################

# Ensure the right packages are available

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(lubridate)) install.packages("lubridate")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(rmarkdown)) install.packages("rmarkdown")
if(!require(knitr)) install.packages("knitr")

# Get the data, read data, wrangle data
# Note: this process could take a couple of minutes

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Create validation set to be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)

## end of input code ##

# include date and year in the edx dataset
edx$date <- as.POSIXct(edx$timestamp, origin="1970-01-01")
edx$year <- format(edx$date,"%Y")

# make small dataset - eds (short for "edx small") - for faster development and testing
set.seed(1, sample.kind="Rounding")
eds <- sample_n(edx, 10000, replace = FALSE)

# make test and training sets - edx
set.seed(1, sample.kind="Rounding")
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)

######### Part 2 ###############
# Data exploration
################################

# TODO: double graph with movies and users from 33.7.1 Movielens data

## Histogram - no of ratings for the movies
sum_movie <- edx %>% 
  group_by(movieId) %>%
  summarize(ave_rating = mean(rating), count = n()) %>%
  arrange(desc(count))

sum_movie %>% ggplot(aes(count)) +
  geom_histogram(bins=30, fill = "#993366") +
  labs(title = "Number of ratings for movies",
       x = "No of ratings (log10)",
       y = "No of movies") +
  scale_x_log10() +
  theme_light()

# key stats - number of unique users and movies
edx %>% summarize(unique_users = n_distinct(userId), unique_movies = n_distinct(movieId))
summary(sum_movie)
sum_movie %>% filter(count == 1)
cor(sum_movie$count, sum_movie$ave_rating)

## Variability between rating
sum_movie %>% ggplot(aes(x=ave_rating)) + 
  geom_histogram(binwidth = 0.1, fill = "#993366") +
  labs(title = "Average rating vs movies",
       x = "Average rating",
       y = "No of movies") +
  theme_light()

# high variability between movie ratings

edx %>% summarize(ave_rating = mean(rating), sd = sd(rating))
summary(edx$rating)
# average rating ca. 3.5, standard deviation ca 1.05 stars

## No of ratings vs average rating
sum_movie %>% ggplot(aes(x=count, y=ave_rating)) + 
  geom_point(alpha = 0.5, colour = "#993366") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Average rating vs no of ratings",
       x = "Number of ratings",
       y = "Average rating") + 
  geom_smooth() +
  theme_light()

rm(sum_movie)
## Boxplot per genre

# First, split movies with several genres
genre <- edx %>% separate_rows(genres, sep = "\\|") %>% select(genres, rating)

# Summary table with average rating and standard deviation per genre
genre %>% group_by(genres) %>%
  summarize(ave_rating = mean(rating), sd = sd(rating)) %>%
  arrange(desc(ave_rating))
# average of ca 0.8 stars difference between highest rate genre (Film-Noir) and lowest rated (Horror)
# low variation of standard deviation (between 0.89 and 1.15)

# Make the boxplot
genre %>% mutate(genres = reorder(genres, rating, FUN = mean))%>% 
  ggplot(aes(x=genres, y=rating)) +
  geom_boxplot(outlier.shape = NA, colour = "#993366") +
  labs(x="", y="Rating", title = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

rm(genre)

## Variability between users
# Make user table
sum_user <- edx %>% 
  group_by(userId) %>%
  summarize(ave_rating = mean(rating), count = n()) %>%
  arrange(desc(count))

sum_user %>% ggplot(aes(x=ave_rating)) + 
  geom_histogram(binwidth = 0.1, fill = "#993366") +
  labs(y = "No of users", x = "Average user rating", title = "Average user ratings") +
  theme_light()

summary(sum_user)
# there is quite a bit of variability between users - normally distributed with a mean of 3.6
rm(sum_user)

# Variability over time
sum_year <- edx %>%
  group_by(year) %>%
  summarize(ave_rating = mean(rating), count = n()) %>%
  arrange(year)

sum_year %>% ggplot(aes(x=year, y=ave_rating)) + 
  geom_bar(stat="identity", fill = "#993366") +
  labs(y = "Average rating", x = "", title = "Ratings over time") +
  theme_light()

# the average rating is stable at ca 3.5 except for the first year where average was ca. 4.0

rm(sum_year)

# Based on this, the movie itself and the user are the most important variables to model.
# The genre and the rating year show less variation

######### Part 3 ###############
# Modeling and RMSE - edx data
################################

### Define the loss function (RSME) - similar to std deviation: The typical error when making a prediction

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

### A first model - just the average rating for all movies
# Yui=μ+εu,i where u is the "true rating" and εu,i are the errors centered at 0

mu_edx <- mean(edx_train$rating)
mu_edx
# average movie rating is ca 3.5 - let us use that as the predictor

## Run on edx set
rmse1_edx <- RMSE(edx_test$rating, mu_edx)
rmse1_edx
# we're getting an RMSE of 1.06 - so typically we're more than one star off

### Include Movie effects - edx set
# We know there is high variability between movies
# Yui=μ+bi+εu,i where bi is the average rating for movie i

avgs_movie_edx <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu_edx))

pred_movie <- edx_test %>% 
  left_join(avgs_movie_edx, by='movieId') %>%
  mutate(movie_pred = mu_edx + b_i) %>%
  pull(movie_pred)

# calculate RMSE
rmse2_edx <- RMSE(edx_test$rating, pred_movie)
rmse2_edx

# The RMSE has been reduced from 1.06 to 0.94. Not bad!

### Add user effects
# We know there is high variability between movies
# Yui=μ+bi+bu+εu,i where bu is the user-specific effect

## edx data set
avgs_user_edx <- edx_train %>% 
  left_join(avgs_movie_edx, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_edx - b_i))

pred_user <- edx_test %>% 
  left_join(avgs_movie_edx, by='movieId') %>%
  left_join(avgs_user_edx, by='userId') %>%
  mutate(pred = mu_edx + b_i + b_u) %>%
  pull(pred)

# calculate RMSE
rmse3_edx <- RMSE(edx_test$rating, pred_user)
rmse3_edx

#### Include regularization - movie effects

# First attempt - with a fixed lambda - movie effects - edx train set
lambda <- 3
avgs_movie_edx_reg <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu_edx)/(n()+lambda), n_i = n()) 

# Plot the original vs regularized estimates
tibble(original = avgs_movie_edx$b_i, 
       regularized = avgs_movie_edx_reg$b_i, 
       n = avgs_movie_edx_reg$n_i) %>%
  ggplot(aes(original, regularized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5, colour = "#993366") +
  theme_light()

pred_ratings <- edx_test %>% 
  left_join(avgs_movie_edx_reg, by = "movieId") %>%
  mutate(pred = mu_edx + b_i) %>%
  pull(pred)

reg_rmse <- RMSE(edx_test$rating, pred_ratings )
reg_rmse

### Edx set with lambda optimization - movie effects
# Lambda table
lambdas <- seq(0, 8, 0.1)
# Use sapply to calculate RMSE with various Lambdas
reg1_rmse_edx <- sapply(lambdas, function(lambda) {
  # Calculate average per movie
  b_i <- edx_train %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_edx) / (n() + lambda))
  
  # Predict rating and apply to test set
  pred_ratings <- edx_test %>%
    left_join(b_i, by='movieId') %>%
    mutate(pred = mu_edx + b_i) %>%
    pull(pred)
  
  # Predict the RMSE on the test set
  return(RMSE(edx_test$rating, pred_ratings))
  })

# Plot the lambdas graph
lambda_plot <- data.frame(RMSE = reg1_rmse_edx, lambdas = lambdas) %>%
  ggplot(aes(lambdas, reg1_rmse_edx)) +
  geom_point(colour = "#993366") +
  labs(y = "RMSE",x = "Lambda") +
  theme_light()
lambda_plot
# Get the lambda value that minimize the RMSE
min_lambda <- lambdas[which.min(reg1_rmse_edx)]

# calculate RMSE
reg1_rmse_edx <- min(reg1_rmse_edx)
reg1_rmse_edx

#### Regularization with movie and user effects - edx train set
# Use sapply to calculate RMSE with various lambdas
reg2_rmse_edx <- sapply(lambdas, function(lambda) {
  # Calculate movie average
  b_i <- edx_train %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_edx) / (n() + lambda))
  # Calculate user average
  b_u <- edx_train %>%
    left_join(b_i, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_edx) / (n() + lambda))

  # Predict ratings on the test set
  predicted_ratings <- edx_test %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    mutate(pred = mu_edx + b_i + b_u) %>%
    pull(pred)
  
  # Predict the RMSE on the test set
  return(RMSE(edx_test$rating, predicted_ratings))
})

# Find minimum RMSE
reg2_rmse_edx <- min(reg2_rmse_edx)
reg2_rmse_edx


######### Part 4 ####################
# Modeling and RMSE - final run 
#####################################


### Define the loss function (RSME) - similar to std deviation: The typical error when making a prediction

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

### A first model - just the average rating for all movies
# Yui=μ+εu,i where u is the "true rating" and εu,i are the errors centered at 0

# Calculate the average rating for all movies in the train set
mu_val <- mean(edx$rating) 

## Run on validation set
rmse1_val <- RMSE(validation$rating, mu_val)
rmse1_val

### Include Movie effects
# We know there is high variability between movies
# Yui=μ+bi+εu,i where bi is the average rating for movie i

## Validation set
mu_val <- mean(edx$rating) 
avgs_movie_val <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu_val))

pred_movie <- validation %>% 
  left_join(avgs_movie_val, by='movieId') %>%
  mutate(movie_pred = mu_val + b_i) %>%
  pull(movie_pred)

rmse2_val <- RMSE(validation$rating, pred_movie)
rmse2_val

### Add user effects
# We know there is high variability between movies
# Yui=μ+bi+bu+εu,i where bu is the user-specific effect

## Validation data set
mu_val <- mean(edx$rating)
avgs_user_val <- edx %>% 
  left_join(avgs_movie_val, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_val - b_i))

pred_user <- validation %>% 
  left_join(avgs_movie_val, by='movieId') %>%
  left_join(avgs_user_val, by='userId') %>%
  mutate(pred = mu_val + b_i + b_u) %>%
  pull(pred)

# calculate RMSE
rmse3_val <- RMSE(validation$rating, pred_user)
rmse3_val

# We're now down to an RMSE of lower than 0.87. 

#### Include regularization - movie effects
### Validation set

# Lambdas table
lambdas <- seq(0, 8, 0.1)
# Use sapply to calculate RMSE with various Lambdas
reg1_rmse_val <- sapply(lambdas, function(lambda) {
  
  # Calculate the average per movie
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_val) / (n() + lambda))
  
  # Predict rating and apply to validation set
  predicted_ratings <- validation %>%
    left_join(b_i, by='movieId') %>%
    mutate(pred = mu_val + b_i) %>%
    pull(pred)
  
  # Predict the RMSE on the validation set
  return(RMSE(validation$rating, predicted_ratings))
})

# Plot the lambdas graph
lambda_plot2 <- data.frame(RMSE = reg1_rmse_val, lambdas = lambdas) %>%
  ggplot(aes(lambdas, reg1_rmse_val)) +
  geom_point(colour = "#993366") +
  labs(y = "RMSE",x = "Lambda") +
  theme_light()

lambda_plot2

# Get the lambda value that minimize the RMSE
min_lambda <- lambdas[which.min(reg1_rmse_val)]
min_lambda
# Predict the RMSE on the validation set
reg1_rmse_val <- min(reg1_rmse_val)
reg1_rmse_val

#### Regularization with movie and user effects
### Validation set

# Compute the predicted ratings on validation dataset using different values of lambda
reg2_rmse_val <- sapply(lambdas, function(lambda) {
  # Calculate movie average
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_val) / (n() + lambda))
  # Calculate user average
  b_u <- edx %>%
    left_join(b_i, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_val) / (n() + lambda))
  # Compute the predicted ratings on validation dataset
  predicted_ratings <- validation %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    mutate(pred = mu_val + b_i + b_u) %>%
    pull(pred)
  
  # Predict the RMSE on the validation set
  return(RMSE(validation$rating, predicted_ratings))
})

# Find minimum RMSE
reg2_rmse_val <- min(reg2_rmse_val)
reg2_rmse_val


######### Part 5 ####################
# RMSE - comparison table
#####################################

# Add results - just the average
rm(rmse_results)
rmse_results <- tibble(method = "Just the average", 
                       RMSE_edx = rmse1_edx, 
                       RMSE_validation = rmse1_val)
rmse_results

# Add results - movie effect
rmse_results <- add_row(rmse_results, 
                        method = "Movie effect", 
                        RMSE_edx = rmse2_edx, 
                        RMSE_validation = rmse2_val)
rmse_results

# Add results - movie + user effects
rmse_results <- add_row(rmse_results, 
                        method = "User effect", 
                        RMSE_edx = rmse3_edx, 
                        RMSE_validation = rmse3_val)
rmse_results

# Add results - movie w/regularization
rmse_results <- add_row(rmse_results, 
                        method = "Movie w/ regularization", 
                        RMSE_edx = reg1_rmse_edx, 
                        RMSE_validation = reg1_rmse_val)
rmse_results

# Add results - movie + user w/regularization
rmse_results <- add_row(rmse_results, 
                        method = "Movie + user w/ regularization", 
                        RMSE_edx = reg2_rmse_edx, 
                        RMSE_validation = reg2_rmse_val)
rmse_results

