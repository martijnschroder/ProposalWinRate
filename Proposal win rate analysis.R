library(tidyverse)
library(dplyr)
library(lubridate)
library(caret)
library(gridExtra)
library(rpart)

proposals <- read_csv("data/data.csv")

names(proposals) # obtain the column names
tibble(proposals) # show structure of data

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

names(proposals) # much better


# Factorise culumns to facilitate certain type analyses (with exception of regression)
proposals$account <- factor(proposals$account)
proposals$stage <- factor(proposals$stage)
proposals$currency <- factor(proposals$currency)
proposals$practice <- factor(proposals$practice)
proposals$offer <- factor(proposals$offer)
proposals$sector <- factor(proposals$sector)
proposals$director <- factor(proposals$director)
proposals$manager <- factor(proposals$manager)
proposals$source <- factor(proposals$source)
proposals$competitiveness <- factor(proposals$competitiveness)
proposals$segment <- factor(proposals$segment)

# Convert amount column from char to integer
proposals$amount <- as.integer(proposals$amount)

# Convert date columns from char to date types
proposals$creationDate <- as.Date(proposals$creationDate, "%d/%m/%Y")
proposals$closeDate <- as.Date(proposals$closeDate, "%d/%m/%Y")

# Show a table summary of the NA values in the dataset
colSums(is.na(proposals))

# Drop "Competitive or sole sourced (compulsory)" column
proposals <- select(proposals,-competitiveness)

# Data exploration

# Get total number of valid observations for currency, stage and amount
nrProposals <- proposals %>%
  select(currency, stage, amount) %>%
  na.omit() %>%
  filter(currency == "AUD") %>%
  summarise(count = n()) %>%
  pull(count)

# Number of opportunities and value over stage
p1 <- proposals %>%
  select(currency, stage, amount) %>%
  na.omit() %>%
  filter(currency == "AUD") %>%
  group_by(stage) %>%
  summarise(n = n(), value = sum(amount), perc = n() / nrProposals) %>%
  ggplot(aes(x = stage, y = n)) +
  geom_bar(stat = "Identity")

p2 <- proposals %>%
  select(currency, stage, amount) %>%
  na.omit() %>%
  filter(currency == "AUD") %>%
  group_by(stage) %>%
  summarise(n = n(), value = sum(amount), perc = n() / nrProposals) %>%
  ggplot(aes(x = stage, y = value)) +
  geom_bar(stat = "Identity")

grid.arrange(p1, p2, ncol = 2)

# Observations: wins and losses in numbers seem at odds with wins and losses in total value
# It looks like we win smaller bids and loose larger ones. This means that value of the proposal
# is a significant contributer


# Explore how many bids we won / lose per client, practice, offer, sector, amount

# Winning proposals over time and coloured by practice
# As can be seen from the graph, there are some big outliers that may distort later model fitting
proposals %>% filter(currency == "AUD", stage == "Opp successful") %>%
  na.omit() %>%
  ggplot(aes(creationDate, amount, colour = practice, alpha(0.4))) +
  geom_point()

# Check how NA values are potentially impacting the data
nrow(proposals)
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

# practice
proposals[is.na(proposals$practice),]
nrow(proposals[is.na(proposals$practice),])
# 118 NA values for practice. Offer is mostly known and tied to practice. We can retrieve practice values.
# TODO: retrieve practice values based on offer


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


# segment
nrow(proposals[is.na(proposals$segment),])
# 604 observations have missing segment. Either follow procedure above or forget as it seems that 
# sector may not be too important
# TODO: fix


# director
proposals[is.na(proposals$director),]
# 12 observations have NA for director. We can substitute with "unknown" so we don't lose data
proposals[is.na(proposals$director),]$director <- "Unknown"

# manager
proposals[is.na(proposals$manager),]
# 14 observations have NA for manager We can substitute with "unknown" so we don't lose data



# Early test with random forests - the data has not been cleaned enough yet
proposals_clean <- proposals %>%
  na.omit()  %>%
  filter(!(stage == "Client not pursuing" | stage == "Nous not pursuing"))

# convert practice to an integer
# TODO: find a smarter method
proposals_clean %>% distinct(practice)
proposals_clean$practice[proposals_clean$practice == "Public policy"] <- 1
proposals_clean$practice[proposals_clean$practice == "Business & Digital Strategy"] <- 2
proposals_clean$practice[proposals_clean$practice == "Org Performance and Leadership"] <- 3
proposals_clean$practice[proposals_clean$practice == "Executive and talent development"] <- 4

proposals_clean$practice <- as.factor(proposals_clean$practice)
proposals_clean$stage <- as.factor(proposals_clean$stage)
proposals_clean$account <- as.factor(proposals_clean$account)
proposals_clean$offer <- as.factor(proposals_clean$offer)
proposals_clean$sector <- as.factor(proposals_clean$sector)
proposals_clean$segment <- as.factor(proposals_clean$segment)
proposals_clean$director <- as.factor(proposals_clean$director)
proposals_clean$manager <- as.factor(proposals_clean$manager)
proposals_clean$source <- as.factor(proposals_clean$source)


levels(proposals_clean$practice) <- 1:length(levels(proposals_clean$practice))
levels(proposals_clean$account) <- 1:length(levels(proposals_clean$account))
levels(proposals_clean$offer) <- 1:length(levels(proposals_clean$offer))
levels(proposals_clean$sector) <- 1:length(levels(proposals_clean$sector))
levels(proposals_clean$segment) <- 1:length(levels(proposals_clean$segment))
levels(proposals_clean$director) <- 1:length(levels(proposals_clean$director))
levels(proposals_clean$manager) <- 1:length(levels(proposals_clean$manager))
levels(proposals_clean$source) <- 1:length(levels(proposals_clean$source))

proposals_clean <- proposals_clean %>% select(-currency, -name, -creationDate, -closeDate)
str(proposals_clean)

# Fitting with ctree
library(party)
fit <- ctree(stage ~ ., data = proposals_clean)
plot(fit)

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
library(rpart)
fit <= rpart(stage ~ ., 
             data = proposals_clean)

fit <= rpart(stage ~ ., 
             data = proposals_clean, 
             method = "anova", 
             control=rpart.control(minsplit=30, cp=0.001))
