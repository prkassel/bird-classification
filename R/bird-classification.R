library(tidyverse)
library(caret)
library(randomForest)


df <- read_csv('./output/data.csv') %>% as.data.frame()
df <- df %>% filter(substring(ebird_code, 1, 1) %in% letters[1:13])

top_5 <- df %>% group_by(species) %>% summarise(n=n()) %>% arrange(desc(n)) %>% slice(1:5) %>% select(species)


df <- df %>% filter(species %in% top_5$species)

### checkout how many missing values we still have:
sapply(df,function(x)sum(is.na(x)))

### there aren't too many for lat/long/month, so we can just remove those
### removing time as well for now cause it might not make a difference
df <- na.omit(df)

set.seed(11, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(11)`
ind <- createDataPartition(df$ebird_code, times=1, p=0.2, list=FALSE)
train_set <- df[-ind,] %>% select(-filename, -species)
test_set <- df[ind,] %>% select(-filename, -species)

train_knn <- train(ebird_code ~ ., method = "knn", data=train_set, 
                   tuneGrid = data.frame(k = seq(1, 15, 2)), )


predictions <- predict(train_knn, test_set[,-7])

mean(predictions == test_set$ebird_code)

confusionMatrix(predictions, as.factor(test_set$ebird_code))$table

