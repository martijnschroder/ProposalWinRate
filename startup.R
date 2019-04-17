# global startup file
# load libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(caret) ## loads ggplot as a dependency
library(ggthemes)
library(gridExtra)
library(rpart)

# load and wrangle data
source("dataWrangling.R")

# conduct data exploration
source("data_exploration.R")

# tree analysis
source("trees.R")

# K nearest neighbours
source("KnearestNeighbours.R")

