library(tidyverse)
library(dplyr)
library(lubridate)
library(caret)

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
proposals %>%
  select(currency, stage, amount) %>%
  na.omit() %>%
  filter(currency == "AUD") %>%
  group_by(stage) %>%
  summarise(n = n(), value = sum(amount), perc = n() / nrProposals)

# 