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
rm(p1, p2, nrProposals)

# Observations: wins and losses in numbers seem at odds with wins and losses in total value
# It looks like we win smaller bids and loose larger ones. This means that value of the proposal
# is a significant contributer


# Explore how many bids we won / lose per client, practice, offer, sector, amount

# Winning proposals over time and coloured by practice
# As can be seen from the graph, there are some big outliers that may distort later model fitting
proposals %>% filter(currency == "AUD", stage == "Opp successful") %>%
  ggplot(aes(creationDate, amount, colour = practice, alpha(0.4))) +
  geom_point()

# For analysis below, only look at win / loss
proposals <- proposals %>%
  filter(currency == "AUD",
         stage == "Opp successful" | stage == "Opp unsuccessful",
         !(practice == "Corporate"))

# success rates for practice, offer, sector, segment, director, manager
practice_wins_rate <- proposals %>%
  filter(currency == "AUD") %>%
  group_by(practice, stage) %>%
  summarise(wins = n())

offer_wins_rate <- proposals %>%
  filter(currency == "AUD") %>%
  group_by(offer, stage) %>%
  summarise(wins = n())

sector_wins_rate <- proposals %>%
  filter(currency == "AUD") %>%
  group_by(sector, stage) %>%
  summarise(wins = n())

segment_wins_rate <- proposals %>%
  filter(currency == "AUD") %>%
  group_by(segment, stage) %>%
  summarise(wins = n())

director_wins_rate <- proposals %>%
  filter(currency == "AUD") %>%
  group_by(director, stage) %>%
  summarise(wins = n())

manager_wins_rate <- proposals %>%
  filter(currency == "AUD") %>%
  group_by(manager, stage) %>%
  summarise(wins = n())

practice_wins_rate
offer_wins_rate
sector_wins_rate
segment_wins_rate
director_wins_rate
manager_wins_rate

# Create some pretty graphs
# It might be more informative to weigh win rates with amounts
practice_wins_rate %>%
  ggplot(aes(x = practice, y = wins)) +
  geom_bar(stat = "Identity")

offer_wins_rate %>%
  ggplot(aes(x = practice, y = wins)) +
  geom_bar(stat = "Identity")

sector_wins_rate %>%
  ggplot(aes(x = practice, y = wins)) +
  geom_bar(stat = "Identity")

segment_wins_rate %>%
  ggplot(aes(x = practice, y = wins)) +
  geom_bar(stat = "Identity")

director_wins_rate %>%
  ggplot(aes(x = practice, y = wins)) +
  geom_bar(stat = "Identity")

manager_wins_rate %>%
  ggplot(aes(x = practice, y = wins)) +
  geom_bar(stat = "Identity")

practice_wins_rate %>%
  ggplot(aes(x = practice, y = wins)) +
  geom_bar(stat = "Identity")

