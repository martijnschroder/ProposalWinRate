# K nearest neighbours

train_knn <- train(stage ~ ., method = "knn", 
                   data = proposals_clean,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))

# best tune
train_knn$bestTune

confusionMatrix(predict(train_knn, proposals_clean, type = "raw"),
                proposals_clean$stage)$overall["Accuracy"]

