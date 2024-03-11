library(dplyr)
library(tidyr)
library(fuzzyjoin)
library(ggplot2)
library(ggimage)
library(ggrepel)

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
                    group_by(book_names_shortened) %>%
                    slice_min(order_by=dist, n=1) %>% filter(dist <= 0.2) %>% 
                    select(book_names_shortened,movie_title_shortened, dist)


books_fuzzy_join <- books_fuzzy_join %>% filter(dist <= 0.125 | movie_title_shortened == "The Lord of the Rings")

books_combined <- books %>% inner_join(books_fuzzy_join, by = "book_names_shortened") %>% 
                  left_join(movies %>% select(movie_title_shortened, worldwide_gross, rating), 
                            by = "movie_title_shortened")

relative_path <- file.path("..", "goodreads_books_with_many_ratings")

books_combined$image_name <- file.path(relative_path, books_combined$image_name)

highest_no_of_rating <- books_combined %>% arrange(desc(no_of_ratings)) %>% slice(1)
highest_box_office <- books_combined %>% arrange(desc(worldwide_gross)) %>% slice(1)
highest_box_office_under_twoandhalf_goodreads <- books_combined %>% filter(no_of_ratings <= 2500000) %>% arrange(desc(worldwide_gross)) %>% slice(1)


# Create a ggplot2 plot with movie posters as points
gg <- ggplot(books_combined, aes(x = no_of_ratings, y = worldwide_gross)) +
  geom_image(aes(image = image_name), size = 0.03) +  # Add book covers 
  geom_text_repel(data = highest_no_of_rating, aes(x = no_of_ratings, y = worldwide_gross,
                                                 label = paste0(book_names, "\nhas the highest number of goodreads ratings with \n",
                                                                scales::comma(no_of_ratings))),
                  color = "red", size = 3 , vjust = 2.0, hjust = 0.7) +
  geom_text_repel(data = highest_box_office, aes(x = no_of_ratings, y = worldwide_gross,
                                                 label = paste0(book_names,  "\n            has the highest box office revenue with \n                                    $",
                                                                scales::comma(worldwide_gross))),
                  color = "darkblue", size = 3  , vjust = 0.75, hjust = -0.2) +
  geom_text_repel(data = highest_box_office_under_twoandhalf_goodreads, aes(x = no_of_ratings, y = worldwide_gross,
                                                label = paste0(book_names,  "\nhas the highest box office revenue within \nthe books under 2.5 million Goodreads votes with \n$",
                                                          scales::comma(worldwide_gross))),
                  color = "purple", size = 3 , vjust = -0.5) +
  labs(title = "Number of Goodreads Ratings vs Box Office",
       x = "Number of Goodreads Ratings",
       y = "Box Office in $") +
  scale_y_continuous(labels = scales::comma) +  
  scale_x_continuous(labels = scales::comma) +  
  theme_minimal()  

# Save the plot as an image file
ggsave( "box_office_vs_goodreads.png",plot = gg, bg="white")

