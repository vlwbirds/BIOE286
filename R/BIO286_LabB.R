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

cereal %>%
  group_by(mfr) %>%
  summarise(mean_sugars = mean(sugars))

mean(c(2,5,900,NA,60))
mean(c(2,5,900,NA,60),na.rm=TRUE)
str(cereal$mfr)
str(cereal$sugars)

out=aggregate(sugars~mfr,data=cereal,FUN="mean")
out

cereal %>%
  group_by(mfr) %>%
  summarise(mean_sugars = mean(sugars),
            mean_calories = mean(calories))

# G pivot_longer() & pivot_wider
cereal_long = cereal %>%
  pivot_longer(cols = c(-name, -mfr, -type),
               names_to = "nutrient",
               values_to = "value")
head(cereal_long)

# As a tiny exercise, try to write out in plain English what the chunk of code above does.
# The code above takes the larger cereal df and pivots the names, mfr, and type into a single column for each and repeats the values for each other lines.

#THis code takes the unique individual names form the nutrient column and makes a column for each one, then also takes the values that were previously associated with them on on a row, but now they are in the column with the name as a header
cereal_wide = cereal_long %>%
  pivot_wider(names_from = "nutrient",
              values_from = "value")
head(cereal_wide)

# 2) Basic plotting

rm(list=ls()) #fresh start!
dat=read.csv(here("data/LabB/ourworld.csv"))

colnames(dat)

ggplot(data=dat,aes(x=Urban.1,y=Birth_Rt))+
  geom_boxplot()+geom_jitter(width=0.2)+
  labs(x="Urban characteristics",y="Birth Rate")

out=dat %>%
  group_by(Urban.1) %>%
  summarize(mean=mean(Birth_Rt),
            sd=sd(Birth_Rt))
out
