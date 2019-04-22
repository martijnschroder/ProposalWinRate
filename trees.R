# Trees require categorisation and factorisation of features
train_set_tree <- train_set %>%
  na.omit() %>%
  select(-creationDate, -closeDate, -account, -amount, -director, -manager)

test_set_tree <- train_set %>%
  na.omit() %>%
  select(-creationDate, -closeDate, -account, -amount, -director, -manager)



# Fitting with ctree
library(party)
proposals_clean2 <- proposals_clean %>% select(-account)
fit <- ctree(stage ~ ., data = proposals_clean2)
plot(fit) # learning: do we need to categorise amount?

# Fitting random forest
library(randomForest)
fit_with_account <- randomForest(stage ~ ., data = train_set_tree)

plot(fit)

proposals_clean %>%
  mutate(y_hat = predict(fit, newdata = proposals_clean)) %>% 
  ggplot() +
  geom_point(aes(amount, practice, colour = y_hat))






# fitting with caret rpart
library(caret)

fit <- train(stage ~ .,
             method = "rpart",
             tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
             # na.action = na.pass,
             data = train_set_tree)
ggplot(fit)

plot(fit$finalModel, margin = 0.1)
text(fit$finalModel, cex = 0.75)

fit$results
fit$coefnames
fit$bestTune

test_set %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(stage, amount, colour = practice)) #+
#geom_step(aes(stage, y_hat), col="red")


# Tree with rpart
train_set <- train_set %>% select(-director, -manager, -account, -name, -source, -creationDate, -closeDate)
test_set <- test_set %>% select(-director, -manager, -account, -name, -source, -creationDate, -closeDate)
library(rpart)
fit <- rpart(stage ~ .,
             method = "class",
             data = train_set)

fit <- rpart(stage ~ ., 
             data = train_set, 
             method = "anova", 
             control=rpart.control(minsplit=30, cp=0.001))

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

pred_test <- predict(fit, test_set, type = "class")

conf <- table(test$Severity, _test)

acc <- sum(diag(conf))/sum(conf)




# Ranger
train_set$name <- factor(train_set$name)
train_set$stage <- factor(train_set$stage)
train_set$practice <- factor(train_set$practice)
train_set$offer <- factor(train_set$offer)
train_set$sector <- factor(train_set$sector)
train_set$segment <- factor(train_set$segment)
train_set$director <- factor(train_set$director)
train_set$manager <- factor(train_set$manager)

test_set$name <- factor(test_set$name)
test_set$stage <- factor(test_set$stage)
test_set$practice <- factor(test_set$practice)
test_set$offer <- factor(test_set$offer)
test_set$sector <- factor(test_set$sector)
test_set$segment <- factor(test_set$segment)
test_set$director <- factor(test_set$director)
test_set$manager <- factor(test_set$manager)


train_set <- train_set %>%
  select(-name, -currency, -creationDate, -closeDate, -source)

test_set <- test_set %>%
  select(-name, -currency, -creationDate, -closeDate, -source)


# classification forest
ranger(stage ~., data = train_set, num.trees = 70, mtry=3)

# prediction
train.idx <- sample(nrow(proposals_clean), 2/3 * nrow(proposals_clean))
proposals.train <- iris[train.idx, ]
proposals.test <- iris[-train.idx, ]

rg.proposals <- ranger(stage ~ ., data = train_set)
pred.proposals <- predict(rg.proposals, data = test_set)

#Build a confusion matrix
table(test_set$stage, pred.proposals$predictions)

