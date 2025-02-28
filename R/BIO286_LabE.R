########################################################################################################
########################################################################################################
############################# BIOE 286 - Lab E #########################################################
#############################   Vince Weber    #########################################################
########################################################################################################
########################################################################################################

library(here)

#note: i am turning off warning messages because they are obnoxious in rmarkdown
#but you'll probably see warning messages when you write your code. don't panic!
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
lapply(c("ggthemes","tidyverse","viridis","ggpmisc",
         "ggplot2","geomtextpath", "ggh4x","cowplot","lme4",
         "grid","geomtextpath","ggrepel","multcomp","emmeans"),require,character.only=T)

#read data
dat=read.csv(here('data/LabE/MoltData286.csv'))
Points=read.csv(here('data/LabE/MoltPoints286.csv'))
Model=read.csv(here('data/LabE/MoltModel286.csv'))

#Color palette for time points
fig2_palette = c(
  "arrival" = "#559B9D",
  "moltstart" = "#235D72",
  "moltend" = "#A4DAC1",
  "departure" = "#123F5A"
)
# show color palette
library(scales)
show_col(fig2_palette)

# color palette for age and sex
figmain_palette = c(
  "Juvenile Male" = "grey70",
  "Juvenile Female" = "#3F141F",
  "Repro Adult Female" = "#E39AAD",
  "Skip Adult Female" = "grey20"
)

# plot
f1=ggplot(data=Points,aes(x=yday,y=molt))+
  geom_vline(xintercept = seq(110,165,10),col="grey95",size=.2)+
  geom_line(data=Model,aes(x=graphx,y=pred1),col="grey10",size=1)+
  geom_point(size=2)+
  scale_y_continuous(labels = scales::percent,expand=c(.03,.03))+
  scale_x_continuous(expand=c(0,0))+
  xlab("Date (day of year)")+ylab("Percent Molt")+
  theme_few()
f1

#set up the annotations
datay=subset(dat,ayr=="2022-38369") #subset metadata for this one seal
annot=data.frame(x=c(datay$arrival, #x location for arrival
                     datay$moltstart, #x location for molt start
                     datay$moltend, #x location for molt end
                     datay$departure), #x location for departure
                 y=-0.05) #all life history events have the same y location
#now add a point and label for each life history event
f1+geom_point(data=annot,aes(x=x,y=y),
              size=3,shape=21:24,
              fill=fig2_palette,col="black",stroke=.5)+
  #note we need to shift the x location of two annotations so they don't overlap
  annotate("text",
           x = annot$x-c(0,1,-2,0),
           y = annot$y-.05,
           label = c("Arrival Date", "Molt Start","Molt End", "Departure Date"),
           color = fig2_palette, #using the palette!
           size=2.5,
           hjust=0.5,
           vjust=0.5) +
  annotate("text",
           x=125,y=1,hjust=0.5,size=2.5,
           label=paste("Adult Female Seal","2022-38369","\n",
                       "Molt Duration = 7 days","\n",
                       "Haul-out Duration = 41 days"))

ggsave(here("output/LabE/MoltModelIndividual.png"),height=6,width=6) #save the plot

# starting to set up data for all seals
dat=dat[order(dat$moltstart),] #order by molt start date so the plot looks pretty.
dat$i=1:nrow(dat) #assign row names so ggplot can easily use individual as row
head(dat) #look at the data before the pivot
dat_long=dat %>%
  pivot_longer(cols=c("arrival", "moltstart","moltend","departure"),
               names_to = "phenology",
               values_to = "date")
head(dat_long) #look at the data after the pivot

fig2=ggplot(dat_long, aes(x=date,y=i)) +
  geom_segment(aes(yend = i, x = arrival, xend = departure),
               size=0.2, data = dat,col="grey60") +
  geom_point(aes(fill = phenology, shape = phenology), color = "black", stroke = 0.5) +
  scale_fill_manual(values = fig2_palette) +
  scale_y_continuous("Seals", breaks = NULL,limits=c(1,640)) +
  scale_x_continuous("Date (day of year)",
                     breaks=seq(50,250,by=25),limits=c(65,235))+
  scale_shape_manual(values = 21:24) +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text( hjust = 0.5),
        legend.position = "none")+
  annotate("text",
           x = c(150,160,170,180),
           y = 550,angle=45,
           label = c("Arrival Date", "Molt Start","Molt End", "Departure Date"),
           color = fig2_palette,
           hjust = 0)
fig2
