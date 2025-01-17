## Vince Weber
## BIOE 286 - Lab B

library(here)
library(tidyverse)

urlRemote = "https://raw.githubusercontent.com/"
pathGithub = "calvin-munson/R-DataScience-workshops/master/workshop2_enterthetidyverse/"
fileName = "cereal.csv"
cereal=read.csv(paste0(urlRemote, pathGithub, fileName))

# slow way
fruits = c("apple", "apple", "orange", "orange", "banana")
fruits
length(fruits)
unique(fruits)
fruits2 = unique(fruits)
length(fruits2)

# tasty pipeline
fruits %>%
  unique() %>%
  length()

cereal %>%
  # This line takes column names
  colnames() %>%
  # This line calculates the number of column names
  length()

# C. Introducing select()
head(
  cereal %>%
    dplyr::select(name, calories, fiber)
)

cereal2 = cereal %>%
  dplyr::select(name, calories, fiber)
head(cereal2)

head(
  cereal %>%
    dplyr::select(-name, -mfr)
)

# D. Introducing filter()
cereal %>%
  filter(sugars > 12) %>% 
  nrow()

cereal %>%
  filter(mfr == "Kelloggs")

# yucky way
cereal2 = cereal %>%
  dplyr::select(name, sugars, protein)
cereal3 = cereal2 %>%
  filter(sugars > 12)

# pretty way
cereal %>%
  dplyr::select(name, sugars, protein) %>%
  filter(sugars > 12)

cereal %>%
  filter(calories > 120) %>%
  dplyr::select(name, fiber, fat, sodium)

# E. mutate()
cereal_carbs = cereal %>%
  dplyr::select(name, carbo, sugars)
head(cereal_carbs)

head(
  cereal_carbs %>%
    mutate(sugars_total = sugars*2)
)

head(
  cereal_carbs %>%
    mutate(sugars = sugars*2) #overwrites sugars!
)

head(
  cereal_carbs %>%
    mutate(sugars_with_milk = sugars + 5)
)

head(
  cereal_carbs %>%
    mutate(total_carbs = carbo + sugars)
)

head(
  cereal_carbs %>%
    mutate(total_carbs = carbo + sugars,
           total_with_milk = total_carbs + 5)
)

# F. group_by() & summarize()
head(
  cereal %>%
    group_by(mfr)
)

