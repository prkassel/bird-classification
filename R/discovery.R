library(tidyverse)
library(caret)



df <- read_csv('./output/data.csv') %>% as.data.frame()

top_5 <- df %>% group_by(species) %>% summarise(n=n()) %>% arrange(desc(n)) %>% slice(1:5) %>% select(species)


df <- df %>% filter(species %in% top_5$species)

### remove extreme outlier in longitude
df <- df[-which.max(df$longitude),]
df %>% ggplot(aes(longitude, latitude, color=species)) + geom_point()

spring <- df %>% filter(month_as_numeric %in% seq(3,6, 1))
spring %>% ggplot(aes(longitude, latitude, color=species)) + geom_point()

winter <- df %>% filter(month_as_numeric %in% seq(12,3, 1))
winter %>%  ggplot(aes(longitude, latitude, color=species)) + geom_point()

summer <- df %>% filter(month_as_numeric %in% seq(6,9, 1))
summer %>%  ggplot(aes(longitude, latitude, color=species)) + geom_point()

fall <- df %>% filter(month_as_numeric %in% seq(9,12, 1))
fall %>%  ggplot(aes(longitude, latitude, color=species)) + geom_point()
