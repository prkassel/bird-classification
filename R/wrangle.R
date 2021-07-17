library(tidyverse)
library(lubridate)
library(stringr)

df <- read_csv('./data/xeno-canto-bird-recordings-extended-a-m/train_extended.csv')
df <- df %>% filter(substring(ebird_code, 1, 1) %in% letters[1:13])

df <- df %>% filter(country == "United States", type %in% c("song", "call"))


### Convert Time to the hour (0 - 23) that it was recorded

convert_time_to_numeric <- function(time) {
  converted <- hm(time)
  if (is.na(converted)) {
    converted <- hms(time)
  }
  hour(converted)
}

df$time_as_numeric <- convert_time_to_numeric(df$time)

### Convert date to month for seasonality
df$month_as_numeric <- month(df$date)


clean_elevation <- function(elev) {
  if (str_detect(elev, "\\?")) {
    elev <- NA
  }
  else {
    elev <- str_extract(elev, "\\d+\\.*\\d*")
  }
  as.numeric(elev)
}

df$elevation <- clean_elevation(df$elevation)

df <- df %>% select(species, filename, latitude, longitude, time_as_numeric, month_as_numeric, elevation, type, ebird_code, filename)

write_csv(df, file='./output/data.csv')

