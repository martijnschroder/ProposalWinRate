library(Matrix)
sparse_matrix <- sparse.model.matrix(stage ~.-1, train_set)
output_vector = train_set[,"stage"] == 1

library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)

