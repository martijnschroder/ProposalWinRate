# Data exploration

# Get total number of valid observations for currency, stage and amount
nrProposals <- proposals %>%
  select(currency, stage, amount) %>%
  na.omit() %>%
  filter(currency == "AUD") %>%
  summarise(count = n()) %>%
  pull(count)

# Average amount per sector and win rate


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
  summarise(n = n())

offer_wins_rate <- proposals %>%
  filter(currency == "AUD") %>%
  group_by(offer, stage) %>%
  summarise(n = n())

sector_wins_rate <- proposals %>%
  filter(currency == "AUD") %>%
  group_by(sector, stage) %>%
  summarise(n = n())

segment_wins_rate <- proposals %>%
  filter(currency == "AUD") %>%
  group_by(segment, stage) %>%
  summarise(n = n())

# Opp directors' win rates and values
director_wins_rate <- proposals %>%
  filter(currency == "AUD") %>%
  group_by(director, stage) %>%
  summarise(n = n(), value = sum(amount), avg_value = value/n) %>%
  arrange(desc(avg_value))

director_wins_rate <- director_wins_rate %>%
  group_by(director) %>%
  mutate(ratio = n / sum(n)) %>%
  filter(stage == "Opp successful") %>%
  filter(!str_detect(director, "nactive")) %>%
  filter(n > 5)

director_wins_rate %>%
  ggplot(aes(x=avg_value,y=ratio, size=n),alpha(0.4)) +
  geom_point()


# Opp managers' win rates and values
manager_wins_rate <- proposals %>%
  filter(currency == "AUD") %>%
  group_by(manager, stage) %>%
  summarise(n = n(), value = sum(amount), avg_value = value/n) %>%
  arrange(desc(avg_value))

manager_wins_rate <- manager_wins_rate %>%
  group_by(manager) %>%
  mutate(ratio = n / sum(n)) %>%
  filter(stage == "Opp successful") %>%
  filter(!str_detect(manager, "nactive")) %>%
  filter(n > 5)

manager_wins_rate %>%
  ggplot(aes(x=avg_value,y=ratio, size=n),alpha(0.4)) +
  geom_point()

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
  ggplot(aes(x = offer, y = wins)) +
  geom_bar(stat = "Identity")

sector_wins_rate %>%
  ggplot(aes(x = sector, y = wins)) +
  geom_bar(stat = "Identity") +
  coord_flip()

segment_wins_rate %>%
  ggplot(aes(x = segment, y = wins)) +
  geom_bar(stat = "Identity")

director_wins_rate %>%
  ggplot(aes(x = director, y = n)) +
  geom_bar(stat = "Identity") +
  coord_flip()

director_wins_rate %>% filter(stage == "Opp successful") %>%
  ggplot(aes(x=director, y=ratio, colour=n)) +
  geom_point() +
  coord_flip()

manager_wins_rate %>%
  filter(stage == "Opp successful", n >= 5) %>%
  filter(!str_detect(manager, 'nactive')) %>%
  arrange(desc(ratio)) %>%
  ggplot(aes(x=manager, y=ratio, size=n)) +
  geom_point() +
  coord_flip()

manager_wins_rate %>%
  ggplot(aes(x = practice, y = wins)) +
  geom_bar(stat = "Identity")


