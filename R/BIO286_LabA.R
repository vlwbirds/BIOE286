######################################################
########## BIOE 286 - Lab A ###########################
########### Vince Weber ##############################

library(here)
library(tidyverse)
library(readxl)

A <- 1:10
A <- c(1,1,3,5,10)
sum(A)
length(A)
mean(A)
sd(A)
max(A)
min(A)

rand <- rnorm(n=100, mean = 0, sd = .1)
ggplot() +
  aes(rand) +
  geom_histogram(bins = 20, color = "black", fill = "white")

d <- read.csv(here("data/LabA/example_data.csv"))
head(d)
tail(d)
str(d)
summary(d)
nrow(d)
ncol(d)
colnames(d)
d$total=d$mussels + d$snails + d$barnacles
d$mussels
d[, "mussels"]
d[,3]

subset(d, tideheight=="low")
subset(d, tideheight=="low" & site=="A")
subset(d, mussels>6)
subset(d, snails<=4)
subset(d, tideheight!="low")

ggplot(d, aes(x = mussels, y = snails)) +
  geom_point()

ggplot(d, aes(x = mussels, y = snails)) +
  geom_point(pch = 21,
             fill = "lightblue", col="darkblue",
             size = 2) +
  labs(x = "# of Mussels", y = "# of Snails", title = "Snails~Mussels")

ggplot(d, aes(x = mussels, y = snails)) +
  geom_point(pch = 21,
             fill = rgb(1,0,0,.5),
             col = rgb(0,0,1,.5),
             size = 2) +
  labs(x = "# of Mussels", y = "# of Snails", title = "Snails~Mussels")

ggplot(d, aes(x = mussels, y = snails)) +
  geom_point() +
  annotate("text", x=7, y=15, label = "Marm is Gr8")

res <- summary(lm(snails~mussels, data = d))
ggplot(d, aes(x = mussels, y = snails)) +
  geom_point() +
  annotate("text", x=7, y=15, label=paste0("Snails=", round(coef(res) [1], digits = 2), "+",
                                           round(coef(res) [2], digits = 2), "*Mussels"))

ggplot(data = d,mapping = aes(x = mussels, y = snails)) +
  geom_point(aes(color=tideheight,
                 size=tideheight))

ggplot(data=d, aes(x=tideheight, y=mussels)) +
  geom_boxplot(fill="light blue")+
  labs(title="Plot of mussels across tide heights",
       x="Tide Height",
       y = "Mussels Per Square Meter")

# According to Marm, “boxplots are one of the dumbest things ever invented”. Any idea why he thinks  that?
# Well they certainly are ugly
# Whiskers represent the min and max values
# Boxes represent the inner quartile from the median. So, 50% of the value between the min and the median and the max and the median.
# The center line is the median

ggplot(data=d, aes(x=tideheight, y=mussels)) +
  geom_boxplot(fill="light blue")+
  geom_jitter()+
  labs(title="Mussels across tide heights",
       x="Tide Height",
       y = "Mussels Per Square Meter")

# 7) Data aggregation

aggregate(mussels~tideheight, data=d, FUN=median)

aggregate(mussels~tideheight+site, data=d, FUN=median)

agg <- aggregate(mussels~tideheight+site, data=d, FUN=median)

# 8) Fancy plotting

agg = d %>% #take the d dataset
  group_by(tideheight) %>% #for each tideheight category
  summarise(mean = mean(mussels), #calculate the mean # of mussels
            sd = sd(mussels), #and the sd of mussels
            se=sd/sqrt(n())) #and the se of mussels

ggplot(data=agg, aes(x=tideheight, y=mean)) +
  geom_bar(stat="identity", fill="light blue") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.1)+
  labs(title="Mussels across tide heights",
       x="Tide Height",
       y = "Mussels Per Square Meter (±SE)")

aggbysite = d %>% #take the d dataset
  group_by(tideheight,site) %>% #for each tideheight category AND site
  summarise(mean = mean(mussels), #calculate the mean # of mussels
            sd = sd(mussels), #and the sd of mussels
            se=sd/sqrt(n())) #and the se of mussels

ggplot(data=aggbysite, aes(x=tideheight, y=mean,fill=site)) +
  geom_bar(stat="identity",
           position=position_dodge(width=.9)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.1,
                position=position_dodge(width=.9))+
  labs(title="Mussels across tide heights",
       x="Tide Height",
       y = "Mussels Per Square Meter (±SE)")

# 9) EXAMPLE: sea star

dat=read.csv(here("data/LabA/Size and age of seastars.csv"))
head(dat)
colnames(dat) = c("Age","Diameter")
head(dat)

ggplot(dat, aes(Age, Diameter)) +
  geom_point()+
  labs(x="Age (years)", y="Diameter (mm)")
ggsave(filename = "DiameterAge.png", path = here("output/"), width = 4, height = 4, units = "in", dpi = 300)

# 10) EXAMPLE: Abalone Landings

df <- read_csv(here("data/LabA/abalonelandings.csv"))
head(df)
tail(df)
view(df)
# What is the relationship you would like to depict? I want to plot the pounds for each species of abalone by year
# What are the x and y axes? Pounds is Y, Year is X
# What sort of graph type is most appropriate? Scatterplot or bar charts

aba_spp <- df %>% 
  group_by(Species) 
  
ggplot(aba_spp, aes(Year, Pounds, fill = Species, color = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("black", "green", "pink", "red", "blue", "purple", "white")) +
  scale_color_manual(values = c("black", "green", "pink", "red", "blue", "purple", "white")) +
  labs(x = "Year", y = "Pounds of Abalone")
# ouchie eyeballs


ggplot(aba_spp, aes(Year, Pounds, color = Species, group = Species)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("black", "lightgreen", "hotpink", "red", "blue", "purple", "white")) +
  labs(x = "Year", y = "Pounds of Abalone")
ggsave(filename = "AbalonePounds_Line.png", path = here("output/"), width = 4, height = 4, units = "in", dpi = 300)

# 11) EXAMPLE: VO2 max
#Now try to create the appropriate graph for the relationship between time to run a mile and oxygen consumption, using the dataset VO2 max vs runtime.csv.
#What type of graph would you use? Scatterplot with a fitted line and standard error buffer

df <- read_csv(here("data/LabA/VO2 max vs runtime.csv"))
head(df)
view(df)

colnames(df) = c("VO2 max", "Runtime")

ggplot(df, aes(Runtime, `VO2 max`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Runtime (minutes per mile)", y = "VO2 Max (oxygen consumption, ml/(kg x min)") +
  theme_minimal()
ggsave(filename = "VO2_Runtime_Line.png", path = here("output/"), width = 4, height = 4, units = "in", dpi = 300)

# 12) Limpet Size

df <- read_csv(here("data/LabA/Limpet size.csv"))
head(df)

ggplot(df, aes(x=limpetsize))+
  geom_histogram() +
  labs(x = "Limpet Size")

# 13) Whales

df <- read_csv(here("data/LabA/Blue whale abundance.csv"))
head(df)
str(df)

ggplot(df, aes(x = Location, y = `Number of Whales`, fill = Period)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Location", y = "Number of Whales") +
  theme_minimal()

df$Period = factor(df$Period,levels=c("Pre-whaling","Current"))

ggplot(df,aes(x=Location, y=`Number of Whales`, fill=Period))+
  geom_bar(stat="identity",position=position_dodge())+
  theme(axis.text.x=element_text(angle=90))
ggsave(filename = "WhalesPreCurrent.png", path = here("output/", width = 4, height = 4))

# 14) Urchin/Kelp Map

library(ggmap)
library(mapdata)

dat <- read_csv(here("data/LabA/Urchin kelp Lat Long.csv"))
head(dat)

ylim=range(dat$LAT) #find latitude limits of data to inform map
xlim=range(dat$LONG) #find longitude limits of data to inform map

#world map data
w=map_data("worldHires",ylim=ylim,xlim=xlim) #extract map data

#make the urchin plot
z=ggplot()+ #make an empty plot
  labs(y= "Latitude (deg)", x = "Longitude (deg)")+
  geom_polygon(data=w,aes(x=long,y=lat,group=group),fill="grey30")+
  coord_fixed(1.5,xlim=xlim,ylim=ylim)+
  geom_point(dat,mapping=aes(x=LONG,y=LAT,size=Urchins))+
  scale_size(range = c(0, 3))+
  theme(axis.text.x=element_text(angle=90))

#display the plot
z

ggsave(filename = "SitkaUrchinsAllYears.png", path = here("output/"), width = 4, height = 5)

# creating facet grid map
z+facet_grid(.~Year)
ggsave(filename = "SitkaUrchinsSplitYears.png", path = here("output/"), width = 4, height = 5)

z+facet_grid(Year~.)

# What pattern do you see? There were more Urchins in 1988, but after they declined, their population leveled out to relatively stable level

#plot Kelp
z=ggplot()+
  labs(y= "Latitude (deg)", x = "Longitude (deg)")+
  geom_polygon(data=w,aes(x=long,y=lat,group=group),fill="grey30")+
  coord_fixed(1.5,xlim=xlim,ylim=ylim)+
  geom_point(dat,mapping=aes(x=LONG,y=LAT,size=Kelp))+
  scale_size(range = c(0, 3))+
  theme(axis.text.x=element_text(angle=90))
z+facet_grid(.~ Year)

# What is the pattern? > How do the graphs help you understand the patterns?
# The pattern here is that Kelp were less prevalent in the 1988 plot compared to the others, which may suggest that there is.a relationship between urchin populations and kelp abundance.