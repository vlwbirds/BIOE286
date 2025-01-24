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


# 1. Load the tidyverse packages from the library and read in the .csv file called “animal_weight.csv”. Store it in as a data object called animal_weights.
## Call to tidyverse packages
library(tidyverse)
urlRemote = "https://raw.githubusercontent.com/"
pathGithub = "calvin-munson/R-DataScience-workshops/master/practice_worksheets/animal_weights/"
fileName = "animal_weight.csv"
animal_weights=read.csv(paste0(urlRemote, pathGithub, fileName))

# 2. Write code to: a) Look at the header of the data, b) find the number of columns in the
# data, and c) find the number of rows in the data.
head(animal_weights) # species sex age_class individual weight_kg height_m
ncol(animal_weights) # 6 cols
nrow(animal_weights) # 24 rows

# 3. Identify the unique species and unique age classes present in the dataset
unique(animal_weights$species) # "Elephant" "Hippo"    "Rhino"   
unique(animal_weights$age_class) # [1] "Child" "Adult"

# 4. Write code to calculate the mean weight of each species in the dataset. Hint: group_by()
# and summarise() are your friends

spp_mean_weight <- animal_weights %>% 
  group_by(species) %>% 
  summarise(mean(weight_kg))
# species  `mean(weight_kg)`
# <chr>                <dbl>
# 1 Elephant             7875 
# 2 Hippo                1888.
# 3 Rhino                1781.

# 5. Calculate the same average weight, but this time for each species/age class combination
# in the dataset (for instance, what does the average elephant child weight? Elephant adult?
#                   Hippo child? etc)


spp_class_kg <- animal_weights %>% 
  group_by(species, age_class) %>% 
  summarise(mean(weight_kg))
# species  age_class `mean(weight_kg)`
# <chr>    <chr>                 <dbl>
# 1 Elephant Adult                 9125 
# 2 Elephant Child                 6625 
# 3 Hippo    Adult                 2300 
# 4 Hippo    Child                 1475 
# 5 Rhino    Adult                 2225 
# 6 Rhino    Child                 1338.


# 6. Next, calculate both mean weight AND mean height in the same data frame for each
# species/age class combination

spp_class_kg_ht <- animal_weights %>% 
  group_by(species, age_class) %>% 
  summarise(mean(weight_kg),mean(height_m))
# species  age_class `mean(weight_kg)` `mean(height_m)`
# <chr>    <chr>                 <dbl>            <dbl>
# 1 Elephant Adult                 9125              6.6 
# 2 Elephant Child                 6625              3.5 
# 3 Hippo    Adult                 2300              1.48
# 4 Hippo    Child                 1475              0.9 
# 5 Rhino    Adult                 2225              1.62
# 6 Rhino    Child                 1338.             1.12


# 7. You may have noticed that there is also a column for the sex of the animal, but that the
# data only exists for the Hippos in our dataset. Create a new dataframe called hippo_stats,
# which a) only includes data from Hippos, and b) has the mean weight and height for each
# species, sex, and age combination in the dataset.

hippo_stats <- animal_weights %>% 
  subset(species %in% "Hippo") %>% 
  group_by(species, age_class, sex) %>% 
  summarise(mean(weight_kg), mean(height_m))

# # A tibble: 4 × 5
# # Groups:   species, age_class [2]
# species age_class sex    `mean(weight_kg)` `mean(height_m)`
# <chr>   <chr>     <chr>              <dbl>            <dbl>
# 1 Hippo   Adult     Female              1500             1.4 
# 2 Hippo   Adult     Male                3100             1.55
# 3 Hippo   Child     Female              1200             0.8 
# 4 Hippo   Child     Male                1750             1  


# 8. Calculate Body Mass Index (BMI) for each individual animal. In humans, BMI is calculated
# as:
#   BMI = kg/m2
# Where BMI is body mass index, kg is mass in kilograms, and m is height in meters.
# Taking the original dataframe, create a new column that contains a calculated BMI for each individual
# animal (this is obviously totally meaningless in terms of actual data on these species!)
# Hint: mutate() is your friend! Also, if you are stuck on how to square a value in R, check out our even better
# friend, Google!

bmi <- animal_weights %>% 
  mutate(BMI = weight_kg/height_m^2)

# species    sex age_class individual weight_kg height_m       BMI
# 1  Elephant            Child          A      8000      4.0  500.0000
# 2  Elephant            Child          B      6000      3.0  666.6667
# 3  Elephant            Adult          C     10000      7.0  204.0816
# 4  Elephant            Adult          D      9000      6.5  213.0178
# 5  Elephant            Child          E      7000      4.0  437.5000
# 6  Elephant            Child          F      5500      3.0  611.1111
# 7  Elephant            Adult          G      9000      6.4  219.7266
# 8  Elephant            Adult          H      8500      6.5  201.1834
# 9     Hippo Female     Adult          A      1400      1.4  714.2857
# 10    Hippo Female     Adult          B      1600      1.4  816.3265
# 11    Hippo   Male     Adult          C      3000      1.5 1333.3333

#   9. Using ggplot, create a set of boxplots showing the distribution of weights for each species
# Hint: Think about what value you want on which axis

weight_plot <- ggplot(animal_weights, aes(x=species, y=weight_kg, fill = species)) +
  geom_boxplot() +
  geom_point(aes(col=species, color = "black"),position=position_jitterdodge(jitter.width=.2)) #atrocious without chaning the points to black

# 10. Create a more detailed boxplot, this time including the fill of the boxplot as the age class
# of the species Additionally, change a few of the features of the plot:
# a) Add neat x and y axis labels
# b) Add a plot title
# c) Change the y-axis limits to include 0 as a minimum
# d) Jitter the raw data points on top
# Hint: Again, use Google if you’re stuck on how to change these features. The website Stack Overflow is a
# great place to go.

age_plot <- ggplot(animal_weights, aes(age_class, weight_kg, fill = species)) +
  geom_boxplot() +
  geom_point(aes(col=species, color = "black"), position=position_jitterdodge(jitter.width=.2)) +
  ylim(0,10000) +
  labs(title = "Animal Weight by Species and Age Class", x = "Age Class", y = "Weight (kg)")
age_plot
