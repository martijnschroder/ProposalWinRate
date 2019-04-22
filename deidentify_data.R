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

# remove practice "Corporate"
proposals <- proposals %>% filter(practice != "Corporate")

# split name column into name and description. We don't need to reference number
proposals$code <-proposals$name %>% str_extract("^[A-Z]{3}") # 3 letter identifyer of opportunity
proposals$description <-sub("^[A-Z]{3}\\s\\d{4}\\s*[-]*\\s*", "", proposals$name) # name of opportunity
proposals <- proposals %>% select(-name) # name column now redundant. Can be dropped

# de identify the dataset
proposals$account <- as.factor(proposals$account)
proposals$practice <- as.factor(proposals$practice)
proposals$offer <- as.factor(proposals$offer)
proposals$sector <- as.factor(proposals$sector)
proposals$segment <- as.factor(proposals$segment)
proposals$director <- as.factor(proposals$director)
proposals$manager <- as.factor(proposals$manager)
proposals$source <- as.factor(proposals$source)

# Categorise features
levels(proposals$account) <- paste("account", 1:length(levels(proposals$account)), sep="")
levels(proposals$practice) <- paste("practice", 1:length(levels(proposals$practice)), sep="")
levels(proposals$offer) <- paste("offer", 1:length(levels(proposals$offer)), sep="")
levels(proposals$sector) <- paste("sector", 1:length(levels(proposals$sector)), sep="")
levels(proposals$segment) <- paste("segment", 1:length(levels(proposals$segment)), sep="")
levels(proposals$director) <- paste("director", 1:length(levels(proposals$director)), sep="")
levels(proposals$manager) <- paste("manager", 1:length(levels(proposals$manager)), sep="")
levels(proposals$source) <- paste("source", 1:length(levels(proposals$source)), sep="")

# select only "AUD" values and drop currency
proposals <- proposals %>% filter(currency == "AUD") %>% select(-currency)
proposals <- proposals %>% select(-description)

# Code target variable "stage" as follows: 1 = "Opp successful", 0 = "Opp unsuccessful"
proposals$stage[proposals$stage == "Opp successful"] <- 1
proposals$stage[proposals$stage == "Opp unsuccessful"] <- 0
proposals <- proposals %>% filter(stage == 0 | stage == 1) # use only win and loss data
proposals$stage <- as.integer(proposals$stage)

write_csv(proposals, "data/data.csv")
