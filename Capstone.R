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

########## solutions to movielens data set quiz ############
dim(edx)
# about 9M reviews
summary(edx)

edx %>% filter(rating == 0) %>% tally()
edx %>% filter(rating == 3) %>% tally()
# no. of "3" ratingns  2.12M

edx %>% ggplot(aes(x = rating)) + geom_histogram()
hist(edx$rating)

n_distinct(edx$movieId)
# no. of distinct movies = 10677

n_distinct(edx$userId)
# no. of distinct users = 98677

genre <- edx %>% group_by(genres) %>%
  summarize(n = n())
# no. of ratings for drama genre = 733296

#my attempt below inefficient
edx %>% filter(str_detect(edx$genres, "Drama") == TRUE) %>% summarize(n = n())
# drama no. of ratings = 3910127
edx %>% filter(str_detect(edx$genres, "Comedy") == TRUE) %>% summarize(n = n())
edx %>% filter(str_detect(edx$genres, "Romance") == TRUE) %>% summarize(n = n())
edx %>% filter(str_detect(edx$genres, "Thriller") == TRUE) %>% summarize(n = n())

#model answer
# str_detect
genres = c("Drama", "Comedy", "Thriller", "Romance")
sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})

# separate_rows, much slower!
# edx %>% separate_rows(genres, sep = "\\|") %>%
  #group_by(genres) %>%
  #summarize(count = n()) %>%
  #arrange(desc(count))

title <- edx %>% group_by(title) %>% summarise(n = n()) %>% arrange(desc(n))

edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

edx %>% group_by(rating) %>% summarize(count = n()) %>% arrange(desc(count))

#model answer
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
  arrange(desc(count))

check <- edx %>% mutate(status = if_else(rating %% 1 == 0, "whole", "half")) %>%
                 group_by(status) %>% summarize(count = n())
#model answer
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()

####### start of capstone project ###################
dim(edx) # 9M rows 6 col
str(edx) 

#check for duplicates and nulls
#userid check how many unique userid
#movieid check no. of unique movieids
#ratings distribution of ratings
#timestamp convert integer to date timestamp into relevant date
#feature engineer timestamp 1) movie age 2) release year 3) day of week 4) month
#title - strip out release year from title on to a separate column or feature, could possibly be an important feature
#title - feature engineering sentiment based -> clustering into broad based segments
#genres - investigate the grouping further # of distinct groups, if can minmize duplicates/permutations
#genres - feature engineering 1) genre_span_size <- c(1,2,3,4,5) or a class (singular, double, multiple)
#genres - can possibly be expanded to increase no. of features or dimensions