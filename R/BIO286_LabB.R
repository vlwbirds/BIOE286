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

#make a bar graph with error bars
ggplot(data=out, aes(x=Urban.1, y=mean)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_jitter(data=dat,aes(x=Urban.1,y=Birth_Rt),width=0.2)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2)+
  labs(x="community type",y="mean birth rate (+/- SD)")

#first calculate the sample size of each group
out=dat %>%
  group_by(Urban.1) %>%
  summarize(mean=mean(Birth_Rt),
            sd=sd(Birth_Rt),
            n=n(),
            se=sd/sqrt(n))
out

#make a bar graph with error bars
ggplot(data=out, aes(x=Urban.1, y=mean)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2)+
  geom_jitter(data=dat,aes(x=Urban.1,y=Birth_Rt),width=0.2)+
  labs(y=expression("Mean Birth Rate" %+-% "SE"))

ggplot(data=dat,aes(x=Urban.1,y=Birth_Rt,fill=Group))+
  geom_boxplot()+
  geom_point(aes(col=Group),position=position_jitterdodge(jitter.width = .2)) +
  labs(x="Urban characteristics",
     y="Birth Rate",
     col="Country Group",
     fill="Country Group")

p1=ggplot(data=dat,aes(x=Group,y=Birth_Rt,fill=Urban.1))+
  geom_boxplot()+
  geom_point(aes(col=Urban.1),position=position_jitterdodge(jitter.width=.2))+
  labs(x="Country Group",y="Birth Rate",
       col="Urban Characteristics",
       fill="Urban Characteristics")
p1

p2=ggplot(data=dat,aes(x=Gdp_Cap))+
  geom_histogram(binwidth=2000,col="black")+
  stat_bin(binwidth=2000, geom="text", aes(label=after_stat(count)), vjust=-1.5)+
  ylim(0,20)
p2

library(patchwork)
p1+p2

ggplot(dat, aes(x=Urban.1, y=Birth_Rt,fill=Urban.1))+
  geom_boxplot()+geom_jitter()+facet_wrap(.~Group)+
  theme(legend.position="none")

p2nums = ggplot_build(p2)
head(p2nums$data[[1]])

subset(dat,Gdp_Cap>-1000&Gdp_Cap<1000)

dat %>% filter(Gdp_Cap>-1000&Gdp_Cap<1000)

p3=ggplot(data=dat,aes(x=Gdp_Cap))+
  geom_histogram(aes(y = after_stat(count/sum(count))), binwidth=2000,col="black")+
  labs(y="Frequency")+scale_y_continuous(labels = scales::percent)
p3

p3nums = ggplot_build(p3)
head(p3nums$data[[1]])

# 3) Try out data manipulation and plotting on your own!