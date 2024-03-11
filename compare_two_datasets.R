library(dplyr)
library(tidyr)
library(fuzzyjoin)

movies <- read.csv("movies_with_highest_box_office.csv")
books <- read.csv("goodreads_books_with_many_ratings.csv")

books_normal_join <- books %>% select(book_names,avg_rating, no_of_ratings) %>% 
                     left_join(movies %>% select(movie_title, worldwide_gross, rating ),
                               by = c("book_names"= "movie_title"))

books_fuzzy_join <- stringdist_join(books, movies, 
                                    by= c("book_names"= "movie_title"),
                                    mode="left", #use left join
                                    method = "jw", #use jw distance metric
                                    max_dist=2, 
                                    distance_col='dist') %>%
                    group_by(book_names) %>%
                    slice_min(order_by=dist, n=1)
