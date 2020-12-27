## Imports ----
library(tidyverse)
library(here)
library(ggwordcloud)
library(tidytext)
library(SnowballC)
library(sentimentr)
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

sentences <- swiftframe %>% 
    unnest_tokens(sentence, lyric, token = "sentences", drop = FALSE) %>% 
    mutate(sentence = gsub('\"', " ", sentence)) %>% 
    mutate(sentence = gsub("...", " ", sentence, fixed = TRUE))
sentiment_raw <- sentiment(sentences$sentence, 
                           missing_value = NA)
profanity_raw <- profanity(sentences$sentence)
sentences$sentiment <- sentiment_raw$sentiment
sentences$profanity <- profanity_raw$profanity_count

sentences <- sentences %>% 
    group_by(artist, album) %>% 
    mutate(track_norm = track_n / max(track_n)) %>% 
    ungroup() %>% 
    left_join(taytaframe)

track_sentiment <- sentences %>% 
    group_by(artist, album, album_cat, track_n, track_norm) %>% 
    summarize(mean_sentiment = mean(sentiment, na.rm = TRUE),
              total_sentiment = sum(sentiment, na.rm = TRUE))

p1 <- ggplot(track_sentiment, 
       aes(x = track_norm, y = mean_sentiment)) + 
    geom_hline(yintercept = 0, alpha = .75, linetype = "dotted") + 
    geom_point(alpha = .5) + 
    facet_wrap(~ album_cat) + 
    geom_line(alpha = .7, 
              stat = "smooth",
              method = "gam",
              se = TRUE) + 
    mk_nytimes(panel.border = element_rect(color = "grey70"),
               axis.text.x = element_blank(),
               panel.grid.major = element_blank()) +
    scale_x_continuous("Track position",
                       expand = c(.02, 0)) + 
    scale_y_continuous("Average track sentiment") + 
    labs(title = "Average track sentiment by album")
ggsave(here("plots", "p3_track_sentiment.jpg"), p1, 
       width = 6, 
       height = 3.5, 
       scale = 1.5)

p2 <- ggplot(sentences, 
       aes(x = sentiment)) + 
    geom_vline(xintercept = 0, 
               linetype = "dotted",
               alpha = .75) + 
    geom_density() + 
    facet_wrap(~ album_cat,
               scales = "free_y") + 
    mk_nytimes(panel.border = element_rect(color = "grey70"),
               axis.text.y = element_blank(),
               panel.grid.major = element_blank()) +
    scale_y_continuous(NULL, expand = c(.01, 0)) + 
    scale_x_continuous("Lyric sentiment", 
                       expand = c(.025, 0),
                       breaks = c(-1, 0, 1),
                       labels = c("-1\n(Negative)", "0\n(Neutral)", "+1\n(Positive)")) + 
    labs(title = "Distribution of lyric sentiment by album")
ggsave(here("plots", "p4_distribution_of_sentiment.jpg"), p2, 
       width = 6, 
       height = 3.5, 
       scale = 1.5)
