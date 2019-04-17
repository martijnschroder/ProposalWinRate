# Trees require categorisation and factorisation of features

# We won't be needing "Nous or client not proceeding" in the analysis
proposals_clean <- proposals %>%
  filter(!(stage == "Client not pursuing" | stage == "Nous not pursuing"))

nrow(proposals_clean) # 3008 observations remaining

# convert practice to an integer
# TODO: find a smarter method
proposals_clean %>% distinct(practice)

proposals_clean$practice <- as.factor(proposals_clean$practice)
proposals_clean$stage <- as.factor(proposals_clean$stage)
proposals_clean$account <- as.factor(proposals_clean$account)
proposals_clean$offer <- as.factor(proposals_clean$offer)
proposals_clean$sector <- as.factor(proposals_clean$sector)
proposals_clean$segment <- as.factor(proposals_clean$segment)
proposals_clean$director <- as.factor(proposals_clean$director)
proposals_clean$manager <- as.factor(proposals_clean$manager)
proposals_clean$source <- as.factor(proposals_clean$source)


# Categorise features
levels(proposals_clean$practice) <- 1:length(levels(proposals_clean$practice))
levels(proposals_clean$account) <- 1:length(levels(proposals_clean$account))
levels(proposals_clean$offer) <- 1:length(levels(proposals_clean$offer))
levels(proposals_clean$sector) <- 1:length(levels(proposals_clean$sector))
levels(proposals_clean$segment) <- 1:length(levels(proposals_clean$segment))
levels(proposals_clean$director) <- 1:length(levels(proposals_clean$director))
levels(proposals_clean$manager) <- 1:length(levels(proposals_clean$manager))
levels(proposals_clean$source) <- 1:length(levels(proposals_clean$source))

proposals_clean <- proposals_clean %>% filter(currency == "AUD") %>% select(-currency)

proposals_clean <- proposals_clean %>% select(-name, -creationDate, -closeDate)
str(proposals_clean)

# Fitting with ctree
library(party)
proposals_clean2 <- proposals_clean %>% select(-account)
fit <- ctree(stage ~ ., data = proposals_clean2)
plot(fit) # learning: do we need to categorise amount?



# Fitting random forest
library(randomForest)
fit <- randomForest(stage ~ ., data = proposals_clean)
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
             data = proposals_clean)
ggplot(fit)

plot(fit$finalModel, margin = 0.1)
text(fit$finalModel, cex = 0.75)

fit$results
fit$coefnames
fit$bestTune

proposals_clean %>% 
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
train.idx <- sample(nrow(iris), 2/3 * nrow(iris))
iris.train <- iris[train.idx, ]
iris.test <- iris[-train.idx, ]

rg.proposals <- ranger(stage ~ ., data = train_set)
pred.proposals <- predict(rg.proposals, data = test_set)

#Build a confusion matrix
table(test_set$stage, pred.proposals$predictions)

