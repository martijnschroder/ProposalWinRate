library(neuralnet)
library(InformationValue) # used for WOE function

train_set <- train_set_orig
test_set <- test_set_orig

# for WOE to work, the data needs to be factorised
# WOE train_set
train_set$account <- factor(train_set$account)
train_set$practice <- factor(train_set$practice)
train_set$offer <- factor(train_set$offer)
train_set$sector <- factor(train_set$sector)
train_set$segment <- factor(train_set$segment)
train_set$director <- factor(train_set$director)
train_set$manager <- factor(train_set$manager)
train_set$source <- factor(train_set$source)
train_set$code <- factor(train_set$code)

# WOE test_set
test_set$account <- factor(test_set$account)
test_set$practice <- factor(test_set$practice)
test_set$offer <- factor(test_set$offer)
test_set$sector <- factor(test_set$sector)
test_set$segment <- factor(test_set$segment)
test_set$director <- factor(test_set$director)
test_set$manager <- factor(test_set$manager)
test_set$source <- factor(test_set$source)
test_set$code <- factor(test_set$code)


# WOE the train_set
# don't WOE stage column as that is the target
train_set$account <- WOE(X=train_set$account, Y=train_set$stage)
train_set$practice <- WOE(X=train_set$practice, Y=train_set$stage)
train_set$offer <- WOE(X=train_set$offer, Y=train_set$stage)
train_set$sector <- WOE(X=train_set$sector, Y=train_set$stage)
train_set$segment <- WOE(X=train_set$segment, Y=train_set$stage)
train_set$director <- WOE(X=train_set$director, Y=train_set$stage)
train_set$manager <- WOE(X=train_set$manager, Y=train_set$stage)
train_set$source <- WOE(X=train_set$source, Y=train_set$stage)
train_set$code <- WOE(X=train_set$code, Y=train_set$stage)

# WOE test_set
# don't WOE stage column as that is the target
test_set$account <- WOE(X=test_set$account, Y=test_set$stage)
test_set$practice <- WOE(X=test_set$practice, Y=test_set$stage)
test_set$offer <- WOE(X=test_set$offer, Y=test_set$stage)
test_set$sector <- WOE(X=test_set$sector, Y=test_set$stage)
test_set$segment <- WOE(X=test_set$segment, Y=test_set$stage)
test_set$director <- WOE(X=test_set$director, Y=test_set$stage)
test_set$manager <- WOE(X=test_set$manager, Y=test_set$stage)
test_set$source <- WOE(X=test_set$source, Y=test_set$stage)
test_set$code <- WOE(X=test_set$code, Y=test_set$stage)


nn=neuralnet(stage ~ account + practice + offer + sector + segment + director + manager + source,
             data=train_set, hidden=3,act.fct = "logistic",
             linear.output = FALSE)

plot(nn)
Predict=compute(nn,test_set)
Predict$net.result

# Converting probabilities into binary classes setting threshold level 0.5
prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)
pred

# accuracy of predictions
sum(pred == test_set$stage) / nrow(pred)

# 0.7553191

# playing with layers
# 0 hidden layers:  0.7624113
# 1 hidden layer:   0.7730496
# 2 hidden layers:  0.7659574
# 5 hidden layers:  0.7056738
# 10 hidden layers: 0.6312057

# try multiple hidden layers and backpropogation
nn=neuralnet(stage ~ account + practice + offer + sector + segment + director + manager + source,
             data=train_set, hidden=c(2,1,2), act.fct = "logistic",
             linear.output = FALSE)

plot(nn)
Predict=compute(nn,test_set)
Predict$net.result

# Converting probabilities into binary classes setting threshold level 0.5
prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)

# accuracy of predictions
sum(pred == test_set$stage) / nrow(pred)
# 0.7446809

nn=neuralnet(stage ~ account + practice + offer + sector + segment + director + manager + source,
             data=train_set, hidden=c(2,1,2), algorith="rprop+", act.fct = "logistic",
             linear.output = FALSE)

plot(nn)
Predict=compute(nn,test_set)
Predict$net.result

# Converting probabilities into binary classes setting threshold level 0.5
prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)

# accuracy of predictions
sum(pred == test_set$stage) / nrow(pred)
# 0.748227
