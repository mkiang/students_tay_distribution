## Imports ----
library(tidyverse)
library(here)
library(tidytext)
data("stop_words")
source("https://raw.githubusercontent.com/mkiang/airline_testing_strategies/master/code/mk_nytimes.R")

swiftframe <- readRDS(here("data", "working_data.RDS"))$swiftframe
taytaframe <- readRDS(here("data", "working_data.RDS"))$taytaframe 

taytaframe <- taytaframe %>%
    mutate(
        release_date = lubridate::ymd(release_date),
        album_cat = factor(album, 
                           levels = taytaframe$album, 
                           ordered = TRUE),
        label = sprintf("%s\n(%s)", album, format(release_date, "%b %Y"))
    ) %>% 
    mutate(label = case_when(album == "Evermore" ~ paste0("\n", label), 
                             TRUE ~ label))

words <- swiftframe %>%
    unnest_tokens(word, lyric) %>%
    mutate(stopword = (word %in% stop_words$word) + 0)

summary_df <- words %>%
    group_by(album) %>%
    summarize(
        n_words = n(),
        n_tracks = n_distinct(track_n),
        n_nonstop = sum(stopword == 1),
        n_distinct = n_distinct(word)
    ) %>%
    mutate(words_per_track = n_nonstop / n_tracks,
           distinct_per_track = n_distinct / n_tracks) %>%
    gather(metric, value, n_words:distinct_per_track) %>%
    mutate(metric_cat = factor(
        metric,
        levels = c(
            "n_words",
            "n_tracks",
            "n_nonstop",
            "n_distinct",
            "words_per_track",
            "distinct_per_track"
        ),
        labels = c(
            "All words",
            "Tracks",
            "Excluding stop words",
            "Distinct words",
            "Words per track",
            "Distinct words per track"
        ),
        ordered = TRUE
    )) %>%
    left_join(taytaframe)

p1 <- ggplot(summary_df %>%
        filter(metric %in% c("n_words", "n_nonstop", "n_distinct")),
    aes(x = release_date,
        y = value,
        color = metric_cat,
        group = metric_cat)
) + 
    geom_line(size = 1, alpha = .75) + 
    geom_point(color = "white",
               size = 3.5) + 
    geom_point(size = 2,
               alpha = .75) + 
    mk_nytimes(axis.text.x = element_text(angle = 45, hjust = c(rep(1, 8), .5), vjust = 1)) + 
    scale_color_brewer(NULL, palette = "Dark2") + 
    scale_y_continuous("Word frequency") + 
    scale_x_date(NULL,
                 breaks = taytaframe$release_date,
                 labels = taytaframe$label) + 
    labs(title = "Total words by type")
ggsave(here("plots", "p1_words_over_time.jpg"), 
       p1, 
       width = 6, 
       height = 3.5, 
       scale = 1.5)

p2 <- ggplot(summary_df %>%
                 filter(!(metric %in% c("n_words", "n_nonstop", "n_distinct", "n_tracks"))),
             aes(x = release_date,
                 y = value,
                 color = metric_cat,
                 group = metric_cat)
) + 
    geom_line(size = 1, alpha = 1) + 
    geom_point(color = "white",
               size = 3.5) + 
    geom_point(size = 2,
               alpha = 1) + 
    mk_nytimes(axis.text.x = element_text(angle = 45, hjust = c(rep(1, 8), .5), vjust = 1)) + 
    scale_color_brewer(NULL, type = "qual") + 
    scale_y_continuous("Word frequency") + 
    scale_x_date(NULL,
                 breaks = taytaframe$release_date,
                 labels = taytaframe$label) + 
    labs(title = "Average words per track")
ggsave(here("plots", "p2_avg_words_over_time.jpg"), 
       p2, 
       width = 6, 
       height = 3.5, 
       scale = 1.5)
