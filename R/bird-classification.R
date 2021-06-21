library(tidyverse)
library(caret)


df <- read_csv('./output/data.csv') %>% as.data.frame()

top_5 <- df %>% group_by(species) %>% summarise(n=n()) %>% arrange(desc(n)) %>% slice(1:5) %>% select(species)


df <- df %>% filter(species %in% top_5$species)

### checkout how many missing values we still have:
sapply(df,function(x)sum(is.na(x)))

### there aren't too many for lat/long/month, so we can just remove those
### removing time as well for now cause it might not make a difference
df <- na.omit(df)

set.seed(11, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(11)`
ind <- createDataPartition(df$species, times=1, p=0.2, list=FALSE)
train_set <- df[-ind,] %>% select(-filename)
test_set <- df[ind,] %>% select(-filename)

train_knn <- train(species ~ ., method = "knn", 
                   data = train_set, tuneGrid = data.frame(k = seq(3, 15, 2)))

predictions <- predict(train_knn, test_set[,-1])

mean(predictions == test_set$species)

### ~ 57% accuracy
confusionMatrix(predictions, as.factor(test_set$species))$table

