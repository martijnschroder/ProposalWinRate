# analysis of proposal data and outcomes
#
# proposals$x <- predictive factors
# proposals$y <- outcomes (win/loose)
#
library(tidyverse)
library(lubridate)
library(caret)

proposals <- read_csv("data/data.csv")

as_tibble(proposals)
str(proposals)

proposals %>% distinct(Lead.client.manager)
