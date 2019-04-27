proposals %>%
  group_by(account, result) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  group_by(account) %>%
  mutate(p = n / sum(n)) %>%
  filter(result == 1) %>%
  select(-amount, -offer, -sector, -segment, -director, -manager) %>%
  arrange(desc(p)) %>%
  ggplot(aes(reorder(creationDate, p), p, colour=n, alpha = 0.4)) +
  geom_point() +
  facet_wrap(~ practice) +
  theme_minimal() +
  xlab("Date") +
  ylab("Proposal win rate")

proposals %>%
  group_by(account, result) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  group_by(account) %>%
  mutate(p = n / sum(n)) %>%
  filter(result == 1) %>%
  select(-amount, -offer, -sector, -segment, -director, -manager) %>%
  arrange(desc(p)) %>%
  ggplot(aes(reorder(creationDate, p), p, colour=n, alpha = 0.4)) +
  geom_point() +
  facet_wrap(~ practice) +
  theme_minimal() +
  xlab("Date") +
  ylab("Proposal win rate") +
  scale_x_date(date_labels = "%Y")


# exploring ggplot date axis functionality
set.seed(1234)
last_month <- Sys.Date() - 0:29
df <- data.frame(
  date = last_month,
  price = runif(30)
)
head(df)

p <- ggplot(data=df, aes(x = date, y = price)) +
  geom_line()
p

# Format : month/day
p + scale_x_date(date_labels = "%b/%d")

# Format : Week
p + scale_x_date(date_labels = "%U")

# Months only
p + scale_x_date(date_labels = "%B")+
  theme(axis.text.x = element_text(angle=45, hjust = 1))

