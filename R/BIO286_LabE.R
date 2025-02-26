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
