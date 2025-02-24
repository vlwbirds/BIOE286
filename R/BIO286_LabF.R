########################################################################################################
########################################################################################################
############################# BIOE 286 - Lab F #########################################################
#############################   Vince Weber    #########################################################
########################################################################################################
########################################################################################################

library(here)
lapply(c("tidyverse","ggthemes","GGally","car"),require,character.only=T) #load packages

# read limpt data
dat=read.csv(here("data/LabF/Limpet_MR.csv"))
str(dat)
colnames(dat)=c("Diatoms","FocalLimpets","OtherLimpets","TideHeight","Predators") # simplify col names

# check correlations
ggpairs(dat[,c("Diatoms","OtherLimpets","TideHeight","Predators")],switch="both")+
  theme_few()+theme(strip.background = element_rect(fill = "white"),
                    strip.placement = "outside")
# 1a. Which pairs of the predictors have the highest correlations? How strong are the
# correlations? Are the correlations positive or negative? Keep these in mind as you
# do the analyses below!
# 1a. Diatoms and TideHeight positive Corr 0.497*, others alright

# 1b. Can you create hypotheses for each of the 4 relationships before you fit the
# models? Would you hypothesize the response variable (Focal Limpets) to increase
# or decrease with each of the four predictors?

# Diatoms - Positive relationship, more diatoms = more Limpets, with narrow CI Ribbon, nom nom
p1 <- ggplot(dat, aes(x=Diatoms, y=FocalLimpets)) +
               geom_point() +
               stat_smooth() +
               theme_few() +
               labs(x="Diatom Abundance", Y="Focal Limpets")
p1

# OtherLimpets - Kinda chaotic, seemingly flat, but there's a dip in focal abundance once others get higher, 
# so there may me a negative relationship after a certain point, 
# which makes sense because of competition for similar resources
p2 <- ggplot(dat, aes(OtherLimpets, FocalLimpets)) +
  geom_point() +
  stat_smooth() +
  theme_few() +
  labs(x="Other Limpet Abundance", Y="Focal Limpets")
p2# 

# TideHeight - There are more focal limpets as tide height increases. Wider CI Ribbon
p3 <- ggplot(dat, aes(x=TideHeight, y=FocalLimpets)) +
  geom_point() +
  stat_smooth() +
  theme_few() +
  labs(x="Tide Height", Y="Focal Limpets")
p3# 

# Predators - Limpet abundance decreases as predators increase. Interesting that the lm shows limpets increasing as predators increase, which is contrary to what I hypothesized.
p4 <- ggplot(dat, aes(x=Predators, y=FocalLimpets)) +
  geom_point() +
  stat_smooth() +
  theme_few() +
  labs(x="Predator Abundance", Y="Focal Limpets")
p4

# long data and facet grid plots
dat_long=pivot_longer(dat,cols=c("Diatoms","OtherLimpets","TideHeight","Predators"),
                      names_to="Variable",values_to="Value")
ggplot(data=dat_long,aes(y=FocalLimpets,x=Value))+
  geom_point()+facet_grid(.~Variable,scales = "free_x",switch="x")+
  theme_few()+theme(strip.placement = "outside")
ggplot(data=dat_long,aes(y=FocalLimpets,x=Value))+
  geom_point()+facet_grid(.~Variable,scales = "free_x",switch="x")+
  theme_few()+theme(strip.placement = "outside")+
  geom_hline(aes(yintercept=mean(FocalLimpets)),col="blue")
#make two text annotations (blue for Null and red for Best Fit)
#Value is the x location, FocalLimpets is the y location, Variable is the facet panel
ann_text = data.frame(Value=c(75,75), FocalLimpets=c(280,300),Variable=rep("Diatoms",2),
                      color=c("blue","red"),label = c("Null","Best Fit"))
#make the plots
ggplot(data=dat_long,aes(y=FocalLimpets,x=Value))+
  geom_point()+
  facet_grid(.~Variable,scales = "free_x",switch="x")+
  geom_hline(aes(yintercept=mean(FocalLimpets)),col="blue")+
  geom_smooth(method="lm",col="red")+
  geom_text(data = ann_text,aes(label = label,col=color))+
  scale_color_manual(values=c("blue","red"))+
  theme_few()+theme(strip.placement = "outside",legend.pos="none")

# the positive relationship with diatom abundance, as well as tide height appear the most significant by eyeballing

DiatomsModel=lm(FocalLimpets~Diatoms,data=dat);summary(DiatomsModel)
OtherLimpetsModel=lm(FocalLimpets~OtherLimpets,data=dat);summary(OtherLimpetsModel)
TideHeightModel=lm(FocalLimpets~TideHeight,data=dat);summary(TideHeightModel)
PredatorsModel=lm(FocalLimpets~Predators,data=dat);summary(PredatorsModel)

# Independent Variable   Slope    p-value      R^2
# Diatom Abundance       1.052   2.08e-14    0.968
# Other Limpets         -1.497      0.367   -0.008
# Tide Height            8.034      0.013    0.273
# Predators              4.483      0.580   -0.039

# my eyeball predictions were correct. Diatom abundance is highly significant, and tide height is also significant

FullModel=lm(FocalLimpets~Diatoms+OtherLimpets+TideHeight+Predators,data=dat);summary(FullModel)
#FullModel=lm(FocalLimpets~.,data=dat) #fun fact, this is the same as the line above...
#the period says "all the things", but it only works because
#there are no other columns in the dat dataframe!

# Call:
#   lm(formula = FocalLimpets ~ Diatoms + OtherLimpets + TideHeight + 
#        Predators, data = dat)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.4825 -1.2175 -0.1803  1.3116  2.2677 
# 
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  -23.395114   3.311395  -7.065 5.64e-06 ***
#   Diatoms        1.006054   0.004829 208.353  < 2e-16 ***
#   OtherLimpets  -1.053614   0.028305 -37.223 2.11e-15 ***
#   TideHeight     0.977851   0.065442  14.942 5.36e-10 ***
#   Predators     -0.073184   0.136844  -0.535    0.601    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.63 on 14 degrees of freedom
# Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9997 
# F-statistic: 1.655e+04 on 4 and 14 DF,  p-value: < 2.2e-16

# 4. Compare the output of this multiple regression to the table of univariate regression results you made above. 
# Which variables are significant? 
# Diatoms, OtherLimpets, TideHeight are all significant

# Are they the same ones that were significant in analyses by themselves and are the P-values similar?
# The only p-values that are similar from the univariate lm are Diatoms, and Predators. 
# Other Limpets and Tide Height come out to be highly significant in the additive model.

# Are the slopes similar to or very different from the univariate analyses? 
# The slopes for Diatoms and Other Limpets relatively similar to the univariate lm. 
# Predators get flipped to negative, which supports my initial hyposthesis, although it's not significant 
# The slope of Tide Height remains positive, but is much flatter in the additive model.

# For any predictors with different slopes or P-values, why do you think the results are different in the multiple regression?
