# K nearest neighbours
library(caret)

train_set_knn <- train_set %>%
  na.omit() %>%
  select(-creationDate, -closeDate, -account, -amount, -director, -manager)

test_set_knn <- train_set %>%
  na.omit() %>%
  select(-creationDate, -closeDate, -account, -amount, -director, -manager)


train_knn <- train(stage ~ ., method = "knn", 
                   data = train_set_knn,
                   na.action = na.pass,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))

# best tune
train_knn$bestTune

confusionMatrix(predict(train_knn, test_set_knn, type = "raw"),
                test_set_knn$stage)$overall["Accuracy"]



# Compare pairs of (director, manager) on result
