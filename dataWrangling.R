# Read the dataset
proposals <- read_csv("data/data.csv", col_names = TRUE)

colSums(is.na(proposals))

# Step 1: create a tibble with column names and proportions of NA values per column
n <- nrow(proposals) # number of rows in dataset
sumNAbefore <- tibble(colSums(is.na(proposals)/n)) # proportions of NA values per column
sumNAbefore <- cbind(colnames(proposals), sumNAbefore) # concatenate the vectors
names(sumNAbefore) <- c("feature", "proportion") # rename the columns to human readible

sumNAbefore %>% ggplot(aes(reorder(feature, -proportion), proportion)) +
  geom_bar(stat = "Identity") +
  coord_flip() +
  xlab("Dataset column") +
  ylab("Proportion of NA values") +
  ggtitle("Proportion of NA values in the dataset columns") +
  theme_light()

rm(n) # clean up

# drop the competitiveness column. It's too broken to be useful
proposals <- select(proposals,-competitiveness)


# address NA values
# replace NAs with "Unknown"
proposals$source[is.na(proposals$source)] <- "Unknown"
proposals$sector[is.na(proposals$sector)] <- "Unknown"
proposals$segment[is.na(proposals$segment)] <- "Unknown"
proposals$practice[is.na(proposals$practice)] <- "Unknown"
proposals$offer[is.na(proposals$offer)] <- "Unknown"
proposals$director[is.na(proposals$director)] <- "Unknown"
proposals$manager[is.na(proposals$manager)] <- "Unknown"

# drop practice = corporate
proposals <- proposals %>% filter(practice != "Corporate")

proposals$account[is.na(proposals$account)] <- 
proposals <- proposals %>% filter(!is.na(account))

# amount
proposals[is.na(proposals$amount),]
nrow(proposals[is.na(proposals$amount),])
# 42 observations are returned. We can substitute NAs
# with group means for account/stage/practice/offer/sector if possible
# TODO: fix amounts with group means through some method
# the following is a tempory fix - set the amount to the overall mean for amount

amounts <- as.double(proposals$amount[!is.na(proposals$amount)])
avg_amount <- mean(amounts[amounts >= 1000]) # ignore amounts smaller than $1000 as they won't represent real values in our type business
proposals$amount[is.na(proposals$amount)] <- avg_amount # replace missing values with avg amount
proposals$amount[proposals$amount < 1000] <- avg_amount # replace values that are too low to be real with avg amount
rm(amounts, avg_amount)



# Create train and test set with standard seed for reproducibiliy
set.seed(1)

# Get a random sample of 90% for the train set and 10% for validation
train_index <- sample(1:nrow(proposals), round(0.9*nrow(proposals)))
train_set <- proposals[train_index, ]
test_set <- proposals[-train_index, ]

# clean up environment
rm(train_index) # clean up
