##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# Check if the file exists
datafile <- "MovieLens.RData"
if(!file.exists(datafile))
{
  print("Download")
  dl <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  
  ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                   col.names = c("userId", "movieId", "rating", "timestamp"))
  
  movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  
  # if using R 4.0 or later:
  movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                              title = as.character(title),
                                              genres = as.character(genres))
  
  
  movielens <- left_join(ratings, movies, by = "movieId")
  
  # Validation set will be 10% of MovieLens data
  set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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
  save(edx, validation, file = datafile)
} else {
  # If the file exists, just load it
  load(datafile)
}

# Function to display a comma in the thousands place
my_comma <- scales::label_comma(big.mark = ".", decimal.mark = ",")

tribble(~"Dataset",~"Rows",~"Columns",
  "training (edx)",     my_comma(nrow(edx)),           my_comma(ncol(edx)),
  "validation",   my_comma(nrow(validation)),     my_comma(ncol(validation))
)

# Inicial rows - edx dataset
head(edx) 

# Inicial rows - validation dataset
head(validation) 

# Look for NA in the edx dataset
sapply(edx, {function(x) any(is.na(x))})

# Summarise Data - edx dataset
summary(edx) 

# Look for NA in the validation dataset
sapply(validation, {function(x) any(is.na(x))})

# Summarise Data - validation dataset
summary(validation) 

# Review Training rating distribution
edx %>% 
  ggplot(aes(x = rating)) + 
  geom_histogram(binwidth=0.5, color="black", fill="blue") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "Rating Distribution - Training",
    x = "Rating",
    y = "Count"
  )

# Order the number of ratings per user
edx %>% count(userId) %>% arrange(.,n)

# Number of ratings per user
edx %>% count(userId) %>%
ggplot(aes(n)) +
geom_histogram(bins = 50, color = "black", fill="blue") +
scale_x_log10() +
labs(
  title = "Number of ratings per user",
  x = "Users",
  y = "Ratings"
)

edx %>%
group_by(userId) %>%
summarise(mean_rating = mean(rating)) %>%
ggplot(aes(mean_rating)) +
geom_histogram(bins = 50, color = "black", fill = "blue") +
labs(
  title = "Mean movie rating per user",
  x = "Mean movie rating",
  y = "Number of users"
)

# Order the number of ratings per user
edx %>% count(movieId) %>% arrange(.,n)

# Number of rates per movie
edx %>%
count(movieId) %>%
ggplot(aes(n)) +
geom_histogram(bins = 20, color = "black", fill="blue") +
scale_x_log10() +
labs(
  title = "Number of ratings per movie",
  x = "Movie",
  y = "Ratings"
)

# RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Model using Movie and User Effect - training (edx) dataset
lambdas <- seq(0, 2, 0.05)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  
  b_u <- edx %>%
    left_join(b_i, by='movieId') %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n() +l))
  
  predicted_ratings <- edx %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i +  b_u) %>% .$pred
  
  return(RMSE(predicted_ratings, edx$rating))
})

qplot(lambdas, rmses)
lambdas[which.min(rmses)]
min(rmses)

# Model using Movie and User Effect - validation dataset
mu <- mean(validation$rating)
l <- 0.15
b_i <- validation %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  
b_u <- validation %>%
    left_join(b_i, by='movieId') %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n() +l))
  
predicted_ratings <- validation %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i +  b_u) %>% .$pred

RMSE(predicted_ratings, validation$rating)
