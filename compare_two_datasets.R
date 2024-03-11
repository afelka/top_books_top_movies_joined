library(dplyr)
library(tidyr)
library(fuzzyjoin)

movies <- read.csv("movies_with_highest_box_office.csv")
books <- read.csv("goodreads_books_with_many_ratings.csv")

movies <- movies %>%
          mutate(movie_title_shortened = gsub("\\([^)]*\\)", "", movie_title))

books <- books %>%
          mutate(book_names_shortened = gsub("\\([^)]*\\)", "", book_names))

books_normal_join <- books %>% select(book_names_shortened,avg_rating, no_of_ratings) %>% 
                      left_join(movies %>% select(movie_title_shortened, worldwide_gross, rating ),
                      by = c("book_names_shortened"= "movie_title_shortened"))


books_fuzzy_join <- stringdist_join(books, movies, 
                                    by= c("book_names_shortened"= "movie_title_shortened"),
                                    mode="left", #use left join
                                    method = "jw", #use jw distance metric
                                    max_dist=2, 
                                    distance_col='dist') %>%
                    group_by(book_names) %>%
                    slice_min(order_by=dist, n=1) %>% filter(dist <= 0.2)

