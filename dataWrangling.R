# Read the dataset
library(tidyverse)
proposals <- read_csv("data/data.csv", col_names = TRUE)

# Change data types for amount, creationDate and closeDate
proposals$amount <- as.double(proposals$amount)
proposals$creationDate <- as.Date(proposals$creationDate, "%d/%m/%y")
proposals$closeDate <- as.Date(proposals$closeDate, "%d/%m/%y")

# drop competitiveness column as it contains too many NA values
proposals <- proposals %>% select(-competitiveness)

# replace missing amount values with overall group mean
amounts <- as.double(proposals$amount[!is.na(proposals$amount)])
avg_amount <- mean(amounts[amounts > 999]) # calculate overall avg with values greater than 999
proposals$amount[is.na(proposals$amount)] <- avg_amount # replace NAs with avg amount
proposals$amount[proposals$amount <= 999] <- avg_amount # replace amounts < 999 with avg_amount
rm(amounts, avg_amount)

proposals <- proposals[!(is.na(proposals$director) | is.na(proposals$manager)), ]

proposals$sector[is.na(proposals$sector)] <- "unknown"

# Fix the offending entries
proposals$segment[is.na(proposals$segment)] <- "unknown"
proposals$source[is.na(proposals$source)] <- "unknown"
proposals$code[is.na(proposals$code)] <- "unknown"

# Create train and test set with standard seed for reproducibiliy
set.seed(1)

# Get a random sample of 90% for the train set and 10% for validation
train_index <- sample(1:nrow(proposals), round(0.9*nrow(proposals)))
train_set_orig <- proposals[train_index, ]
test_set_orig <- proposals[-train_index, ]

# clean up environment
rm(train_index) # clean up

