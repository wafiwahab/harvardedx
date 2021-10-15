##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
install.packages("kableExtra")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(stringr)
library(knitr)
library(kableExtra)

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

#add features to original dataset, so test set has the same features
#features- release year, title, rating date, rating hour
movielens <- movielens %>%
  mutate(release_yr = parse_number(str_split(trial$title, pattern = "\\(", simplify = TRUE)[,2])) %>%
  mutate(title = str_split(title, pattern = "\\(", simplify = TRUE)[,1]) %>%
  mutate(rating_date = date(as_datetime(timestamp[1]))) %>%
  mutate(rating_hour = hour(as_datetime(edx$timestamp[1])))

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

#these steps need to happen before the data split as testset need same features###
#timestamp convert integer to date timestamp into relevant date
# timestamp is in UTC format
date(as_datetime(edx$timestamp[1])) #UTC timestamp to date

trial <- edx[1:100,]
trial <- trial %>% mutate(movtitle = str_split(trial$title, pattern = "\\(", simplify = TRUE)[,1]) %>%
  mutate(release_yr = parse_number(str_split(trial$title, pattern = "\\(", simplify = TRUE)[,2]))
print(trial)
#title - strip out release year from title on to a separate column or feature, could possibly be an important feature
#generate release year of movie
trial2 <- edx[1:5,]
trial2 <- trial2 %>% mutate(release_yr = parse_number(trial$title)) %>%
  mutate(title = str_split(trial$title, pattern = "\\(", simplify = TRUE)[,1])
print(trial2)

# edx %>% separate_rows(genres, sep = "\\|") %>%
#group_by(genres) %>%
#summarize(count = n()) %>%
#arrange(desc(count))
trial2 <- trial2 %>% separate_rows(genres, sep = "\\|")

genres = c("Drama", "Comedy", "Thriller", "Romance")
sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})

str_detect(trial$genres, "\\|")
str_extract_all(trial$genres, "\\|",simplify = TRUE)
n_distinct(edx$genres) #797 different type of genre combinations
# no. of genre are there and what are they?

starttime = sys.time()
genre_tab <- edx[,c(1,6)] %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(n_movies = n_distinct(movieId)) # this code takes so long
endtime = Sys.time()

duration = endtime - starttime

edx %>% separate_rows(genres,sep = "\\|") %>% summarise(genre_count = n_distinct(genres))
class(genre_test)
#feature engineering - genre
trial <- trial %>% mutate(genre_count = str_count(trial$genres, "\\|")+1)
#check for duplicates and nulls
#need a unique identifier timestamp+userid+movieid n_distinct = no. of rows therefore zero duplicates
#create a table of the high level stats (look at rafa)
#no. of rows
dim(edx)[1]
# no. of unique ratings
#userid check how many unique userid
n_distinct(edx$userId)
#movieid check no. of unique movieids
n_distinct(edx$movieId)

#ratings distribution of ratings

#feature engineer timestamp 1) movie age 2) release year 3) day of week 4) month

#title - feature engineering sentiment based -> clustering into broad based segments
#genres - investigate the grouping further # of distinct groups, if can minmize duplicates/permutations
#genres - feature engineering 1) genre_span_size <- c(1,2,3,4,5) or a class (singular, double, multiple)
#genres - can possibly be expanded to increase no. of features or dimensions