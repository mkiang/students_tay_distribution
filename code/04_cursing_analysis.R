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
    mutate(stopword = (word %in% stop_words$word) + 0,
           shit = grepl("\\<shit", word) + 0,
           fuck = grepl("\\<fuck", word) + 0,
           damn = grepl("damn", word) + 0,
           # crazy = grepl("\\<crazy", word) + 0, 
           bitch = grepl("\\<bitch", word) + 0,
           hell = grepl("\\<hell\\>", word) + 0,)

curses <- words %>%
    filter(stopword == 0) %>%
    group_by(artist, album) %>%
    summarize(
        n_words = n(),
        shit = sum(shit),
        fuck = sum(fuck),
        damn = sum(damn),
        bitch = sum(bitch),
        hell = sum(hell),
        # crazy = sum(crazy)
    ) %>%
    gather(curse, n_curse, shit:hell) %>%
    ungroup() %>%
    left_join(taytaframe) %>%
    mutate(
        rate_per_5k = n_curse / n_words * 1000,
        curse_cat = factor(
            curse,
            levels = c("bitch", "damn", "fuck", "hell", "shit"),
            labels = c("bitch", "[god]damn", "f*ck[ed]", "hell", "sh*t"),
            ordered = TRUE
        )
    ) %>% 
    arrange(album_cat, curse_cat)

p1 <- ggplot(curses,
       aes(x = release_date,
           y = rate_per_5k,
           color = curse_cat,
           group = curse_cat)) + 
    geom_line(size = 1, alpha = .6) + 
    geom_point(color = "white",
               size = 3.5) + 
    geom_point(size = 2,
               alpha = .6) + 
    mk_nytimes(axis.text.x = element_text(angle = 45, hjust = c(rep(1, 8), .5), vjust = 1)) + 
    scale_color_brewer(NULL, palette = "Set1") + 
    scale_y_continuous("Profanity rate (curse word per 1,000 words*)") + 
    scale_x_date(NULL,
                 breaks = taytaframe$release_date,
                 labels = taytaframe$label) + 
    labs(title = "Taylor Swift's Profanity Rate, 2006—2020",
         caption = "*After removing stop words. ")
ggsave(here("plots", "p5_cursing_over_time.jpg"), p1, 
       width = 6, 
       height = 3.5, 
       scale = 1.5)

