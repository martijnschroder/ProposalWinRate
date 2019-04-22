# proportion of winning proposals given source
library(gridExtra)

sum_tab <- proposals %>%
  filter(!is.na(source)) %>%
  group_by(source, stage) %>%
  summarise(n = n(), value = sum(amount)) %>%
  ungroup() %>%
  group_by(source) %>%
  mutate(p = n / sum(n))

p1 <- sum_tab %>%
  filter(stage == 1) %>%
  ggplot(aes(reorder(source, p), p)) +
  coord_flip() +
  geom_bar(stat = "Identity") +
  xlab("Source of proposal") +
  ylab("Proportion of proposals won")
  
p2 <- sum_tab %>%
  filter(stage == 1) %>%
  ggplot(aes(n, p, colour=source)) +
  geom_point() +
  xlab("Number of proposals") +
  ylab("Win rate")

plot(p2)

grid.arrange(p1, p2, ncol = 2)

# table of win rates given number of observations
sum_tab %>%
  filter(stage == 1, n > 9) %>%
  arrange(desc(n)) %>%
  select(n, p, source, value)

          