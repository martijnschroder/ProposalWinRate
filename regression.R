# logistic regression fit

train_set <- train_set %>% na.omit() %>% select(-account)
test_set <- test_set %>% na.omit() %>% select(-account)


model <- glm(stage ~.,family=binomial(link='logit'),data=train_set)

summary(model)
anova(model, test="Chisq")

offer_rates <- proposals %>%
  filter(amount > 99999) %>%
  group_by(offer, stage) %>%
  summarise(n = n())

# Win ratios for proposals with value >= 100k
offer_rates %>%
  group_by(offer) %>%
  mutate(winRatio = n / sum(n)) %>%
  arrange(desc(winRatio)) %>%
  filter(stage == 1, n >10) %>%
  ggplot(aes(offer, winRatio, size=n)) +
  geom_point() +
  coord_flip() +
  ylab(label = "Win ratios for proposals with value >= 100k") +
  xlab("Business offer") +
  theme_light()

# Win ratios given amount
win_ratios <- proposals %>%
  group_by(amount, stage) %>%
  mutate(n = n(), winRatio = n / sum(n))
  
proposals %>%
  filter(stage == 1, amount > 49999) %>%
  group_by(amount) %>%
  mutate(n = n()) %>%
  ggplot(aes(amount, n, colour= practice)) +
  geom_point() +
  theme_light() +
  ylab("Number of successful proposals") +
  xlab("Amount")

# Experimentation with bins and averages to study relationship amount and win rate
bins <- tibble(seq(0, 1000000 - 50000, by=50000),
               tapply(proposals$amount, cut(proposals$amount, seq(0, 1000000, by=50000)), sum))

plot(bins)

proposals %>%
  filter(amount > 49999) %>%
  group_by(amount) %>%
  mutate(n = n())
