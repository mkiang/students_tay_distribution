## Imports ----
library(tidyverse)
library(genius)
library(here)
library(fs)

dir_create(here("data"))

## Set up a skeleton df ----
taytaframe <- tribble(
    ~ artist,      ~ album,          ~ release_date,
    "Taylor Swift", "Taylor Swift",  "2006-10-24",
    "Taylor Swift", "Fearless",      "2008-11-11",
    "Taylor Swift", "Speak Now",     "2010-10-25",
    "Taylor Swift", "Red",           "2012-10-22",
    "Taylor Swift", "1989",          "2014-10-27",
    "Taylor Swift", "Reputation",    "2017-11-10",
    "Taylor Swift", "Lover",         "2019-08-23",
    "Taylor Swift", "Folklore",      "2020-07-24",
    "Taylor Swift", "Evermore",      "2020-12-11" 
)
write_csv(taytaframe, here("data", "disco_info.csv"))

## Add lyrics ----
if (!file_exists(here("data", "working_data.RDS"))) {
    swiftframe <- taytaframe %>%
        select(-release_date) %>% 
        add_genius(artist, album, "album")
    
    ## Save them ----
    write_csv(swiftframe, here("data", "lyrics_by_line.csv"))
    saveRDS(list(taytaframe = taytaframe,
                 swiftframe = swiftframe),
            here("data", "working_data.RDS"))
}
