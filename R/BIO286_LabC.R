########################################################################################################
########################################################################################################
############################# BIOE 286 - Lab C #########################################################
########################################################################################################
########################################################################################################

library(here)
library(tidyverse)
library(ggpubr)

#############################################
# 1) Optimizing sample size
#############################################

# abalone data
dat <- read_csv(here("data/LabC/AbaloneSizes2.csv"))
#view(dat)
#str(dat)

table(dat$SampleSize,dat$Group)

# fake data generation using rep()
r=data.frame(colony=rep(c("Ano Nuevo","San Simeon","Farallones"),each=10),
             sex=(rep(c("Male","Female"),times=15)))
print(r)

table(r$colony) #how many seals are there at each colony, across sexes?

# Ano Nuevo Farallones San Simeon 
# 10         10         10 

table(r$colony,r$sex) #how many seals are there in each colony and sex?

#            Female Male
# Ano Nuevo       5    5
# Farallones      5    5
# San Simeon      5    5

# Back to abalone
out=aggregate(AbaloneSize~Group+SampleSize,data=dat,FUN="mean") # just MEAN by group and sample size
#head(out)
#str(out)

out=dat %>% #do the following things to the data called dat
  group_by(SampleSize,Group) %>% #group by the sample size factor, and also group
  summarise(MEAN=mean(AbaloneSize),
            MEDIAN=median(AbaloneSize),
            SD=sd(AbaloneSize),
            N=n(),
            SE=SD/sqrt(N))
#summarize mean, median, standard deviation, number of cases, and SE
str(out)

#out$SE=out$SD/sqrt(out$N) # another way to make a new column and populate with SE
#out$SE=with(out,SD/sqrt(N)) # same as previous, but with(df,function)
out

# plotting samplesize and Abalone size with mean, median, SD, SE
figa=ggplot(data=out,aes(x=SampleSize,y=MEAN))+
  geom_point()+
  labs(x="Sample Size",y="Mean Ab Size (mm)")
figb=ggplot(data=out,aes(x=SampleSize,y=MEDIAN))+
  geom_point()+
  labs(x="Sample Size",y="Median Ab Size (mm)")
figc=ggplot(data=out,aes(x=SampleSize,y=SD))+
  geom_point()+
  labs(x="Sample Size",y="SD Ab Size (mm)")
figd=ggplot(data=out,aes(x=SampleSize,y=SE))+
  geom_point()+
  labs(x="Sample Size",y="SE Ab Size (mm)")

# combine plots to one patchwork figure
ggarrange(figa,figb,figc,figd, #good thing we named them!
          nrow=2,ncol=2,
          labels=c("A","B","C","D"))

#log transform plots
ggplot(data=out,aes(x=log10(SampleSize),y=MEAN))+
  geom_point()+
  labs(x="Log10(Sample Size)",y="Mean Ab Size (mm)")

#fixing x axis have data on log scale
ggplot(data=out,aes(x=SampleSize,y=MEAN))+
  geom_point()+
  scale_x_log10()+ #this makes a log scale x axis!
  labs(x="Sample Size, on a log scale",y="Mean Ab Size (mm)")

# log axis all data from A B C D
figalog=ggplot(data=out,aes(x=SampleSize,y=MEAN))+
  geom_point()+
  scale_x_log10()+ #this makes a log scale x axis!
  labs(x="Sample Size, on a log scale",y="Mean Ab Size (mm)")
figblog=ggplot(data=out,aes(x=SampleSize,y=MEDIAN))+
  geom_point()+
  scale_x_log10()+ #this makes a log scale x axis!
  labs(x="Sample Size, on a log scale",y="Median Ab Size (mm)")
figclog=ggplot(data=out,aes(x=SampleSize,y=SD))+
  geom_point()+
  scale_x_log10()+ #this makes a log scale x axis!
  labs(x="Sample Size, on a log scale",y="SD Ab Size (mm)")
figdlog=ggplot(data=out,aes(x=SampleSize,y=SE))+
  geom_point()+
  scale_x_log10()+ #this makes a log scale x axis!
  labs(x="Sample Size, on a log scale",y="SE Ab Size (mm)")
ggarrange(figalog,figblog,figclog,figdlog,
          nrow=2,ncol=2,
          labels=c("A","B","C","D"))

