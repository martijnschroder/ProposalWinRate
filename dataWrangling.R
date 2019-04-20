# Read the dataset
# Change to de identified dataset: "data/data.csv"
proposals <- read_csv("data/data_real.csv", col_names = TRUE)

# Rename columns to increase readibility and facilitate analysis
proposals <- proposals %>%
  rename(
    name = `Opportunity Name`,
    account = `Account Name`,
    stage = Stage,
    currency = `Amount Currency`,
    amount = Amount,
    practice = `Primary Practice`,
    offer = `Business Offer`,
    sector = Sector,
    director = `Proposal director`,
    manager = `Proposal manager`,
    source = Source,
    competitiveness = `Competitive or sole sourced (compulsory)`,
    amount = Amount,
    segment = Segment,
    creationDate = `Created Date`,
    closeDate = `Close Date`
  )

# Convert amount column from char to integer
proposals$amount <- as.double(proposals$amount)

# Convert date columns from char to date types
proposals$creationDate <- as.Date(proposals$creationDate, "%d/%m/%Y")
proposals$closeDate <- as.Date(proposals$closeDate, "%d/%m/%Y")

# Show a table summary of the NA values in the dataset
colSums(is.na(proposals))

# Drop "Competitive or sole sourced (compulsory)" column
proposals <- select(proposals,-competitiveness, -source)


# Check how NA values are potentially impacting the data
nrow(proposals) # Number of observations prior to cleaning
# we have 3562 observations in the dataset. See how many we end up with once we're done cleaning

# account
proposals[is.na(proposals$account),]
# 7 observations are returned, with most columns populated with NA values. We need to drop those records
proposals <- proposals %>% filter(!is.na(account)) # remove the offending observations

# stage
proposals[is.na(proposals$stage),]
# same 7 - now empty

# currency
proposals[is.na(proposals$currency),]
# same 7 - now empty

# amount
proposals[is.na(proposals$amount),]
nrow(proposals[is.na(proposals$amount),])
# 82 observations are returned. If other columsn don't feature too many NA values, we can substitute NAs
# with group means for account/stage/practice/offer/sector if possible
# TODO: fix amounts with group means through some method
# the following is a tempory fix - set the amount to the overall mean for amount

amounts <- as.double(proposals$amount[!is.na(proposals$amount)])
avg_amount <- mean(amounts[amounts >= 1000])
proposals$amount[is.na(proposals$amount)] <- avg_amount
proposals$amount[proposals$amount < 1000] <- avg_amount
rm(amounts, avg_amount)

# practice
proposals[is.na(proposals$practice),]
nrow(proposals[is.na(proposals$practice),])
# 118 NA values for practice. Offer is mostly known and tied to practice. We can retrieve practice values.
# TODO: retrieve practice values based on offer
# the following is a temp solution and not desirable
proposals$practice[is.na(proposals$practice)] <- "Unknown"

# offer
proposals[is.na(proposals$offer),]
# Only 2 NAs for offer. Let's drop those as the impact of missing data is small and it makes our set better
proposals <- proposals %>% filter(!is.na(offer)) # remove the offending observations
# now empty


# sector
proposals[is.na(proposals$sector),]
nrow(proposals[is.na(proposals$sector),])
nrow(proposals[is.na(proposals$sector),]) / nrow(proposals)
# 1693 results returned. That is a lot (47.5% of all records)
# Potentially sector information can be retrieved from account. Mechnasim would be to 
# 1. Find list of clients with missing sectors
# 2. Find list of those same clients with sectors populated
# 3. Match the lists and see how well we do
# TODO: fix
# the following is a temp fix

proposals$sector[is.na(proposals$sector)] <- "Unknown"

# segment
nrow(proposals[is.na(proposals$segment),])
# 604 observations have missing segment. Either follow procedure above or forget as it seems that 
# sector may not be too important
# TODO: fix
# the following is a temp fix
proposals$segment[is.na(proposals$segment)] <- "Unknown"

# director
proposals[is.na(proposals$director),]
# 12 observations have NA for director. We can substitute with "unknown" so we don't lose data
proposals$director[is.na(proposals$director)] <- "Unknown"

# manager
proposals[is.na(proposals$manager),]
# 14 observations have NA for manager We can substitute with "unknown" so we don't lose data
proposals$manager[is.na(proposals$manager)] <- "Unknown"

# Code target variable "stage" as follows: 1 = "Opp successful", 0 = "Opp unsuccessful"
proposals$stage[proposals$stage == "Opp successful"] <- 1
proposals$stage[proposals$stage == "Opp unsuccessful"] <- 0

proposals$stage <- as.integer(proposals$stage)

proposals <- proposals %>% filter(stage == 0 | stage == 1) # use only win and loss data
proposals <- proposals %>% filter(currency == "AUD") # user only Australian data
proposals <- proposals %>% select(-currency) # drop currency column as we don't need it anymore
proposals <- proposals %>% filter(segment != "Internal")



# Create train and test set with standard seed for reproducibiliy
set.seed(1)

# Get a random sample of 90% for the train set and 10% for validation
train_index <- sample(1:nrow(proposals), round(0.9*nrow(proposals)))
train_set <- proposals[train_index, ]
test_set <- proposals[-train_index, ]

# clean up environment
rm(train_index)
