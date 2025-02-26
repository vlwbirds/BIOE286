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
## Which variables are significant? 
# Diatoms, OtherLimpets, TideHeight are all significant.

## Are they the same ones that were significant in analyses by themselves and are the P-values similar?
# The only p-values that are similar from the univariate lm are Diatoms, and Predators. 
# Other Limpets and Tide Height come out to be highly significant in the additive model.

## Are the slopes similar to or very different from the univariate analyses? 
# The slopes for Diatoms and Other Limpets relatively similar to the univariate lm. 
# Predators get flipped to negative, which supports my initial hyposthesis, although it's not significant 
# The slope of Tide Height remains positive, but is much flatter in the additive model.

## For any predictors with different slopes or P-values, why do you think the results are different in the multiple regression?
# The p-values are likely different because Diatoms, Other Limpets, and Tide Height all share a relationship with one another within an aquatic system.
# Thus, looking at each one individually doesn't describe the system as well.

# 5. Which predictor in the FullModel model has the largest slope (by “large” we mean the absolute value of the slope)?
# The largest slope is for Other Limpets, but it isn't sizably that different from the other significant parameters.

## Do several predictors have similar size slopes? 
# relatively yes

## Are any much smaller than others?
# the smallest slope is for predators, which I mentioned earlier.

summary(dat)
# Diatoms       FocalLimpets    OtherLimpets     TideHeight      Predators    
# Min.   : 10.0   Min.   :  0.0   Min.   : 0.00   Min.   :39.00   Min.   :1.000  
# 1st Qu.: 80.0   1st Qu.: 90.0   1st Qu.: 9.00   1st Qu.:43.50   1st Qu.:1.750  
# Median :140.0   Median :150.0   Median :26.00   Median :50.00   Median :4.400  
# Mean   :157.4   Mean   :158.9   Mean   :23.11   Mean   :49.80   Mean   :4.532  
# 3rd Qu.:240.0   3rd Qu.:230.0   3rd Qu.:35.00   3rd Qu.:55.35   3rd Qu.:6.750  
# Max.   :300.0   Max.   :330.0   Max.   :46.00   Max.   :60.00   Max.   :9.900 

# Calculate standard deviation across specified columns
apply(dat[, c("Diatoms", "FocalLimpets", "OtherLimpets", "TideHeight", "Predators")], 2, sd, na.rm = TRUE)
# Standard Deviations from dat
#   Diatoms FocalLimpets OtherLimpets   TideHeight    Predators 
# 92.546811    98.820530    14.490015     6.885492     2.986461

## 6. Do all predictors have approximately the same mean? 
# not at all. Diatoms and Focal Limpets are very close, but the other means are highly variable.

## Do they have similar ranges? 
# Again, no. Diatoms and Focal Limpets have similar ranges, but the others have much smaller ranges for the max value. With the exception of tide height, the other variables start close to zero, however.
## Do they have similar standard deviations?

# z-transformation
dat=dat %>% mutate(Diatoms_Z=(Diatoms-mean(Diatoms))/sd(Diatoms),
                   OtherLimpets_Z= (OtherLimpets-mean(OtherLimpets))/sd(OtherLimpets),
                   TideHeight_Z= (TideHeight-mean(TideHeight))/sd(TideHeight),
                   Predators_Z= (Predators-mean(Predators))/sd(Predators))

summary(dat)

# scale (z-transformation)
dat=dat %>% mutate(Diatoms_Z2=scale(Diatoms),
                   OtherLimpets_Z2= scale(OtherLimpets),
                   TideHeight_Z2= scale(TideHeight),
                   Predators_Z2= scale(Predators))
summary(dat)

# delete extra Z2 columns
dat=dat %>% dplyr::select(-Diatoms_Z2,-OtherLimpets_Z2,-TideHeight_Z2,-Predators_Z2)

# model z-transform
FullModel_Z=lm(FocalLimpets~Diatoms_Z+OtherLimpets_Z+TideHeight_Z+Predators_Z,data=dat);summary(FullModel_Z)
# Call:
#   lm(formula = FocalLimpets ~ Diatoms_Z + OtherLimpets_Z + TideHeight_Z + 
#        Predators_Z, data = dat)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.4825 -1.2175 -0.1803  1.3116  2.2677 
# 
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)    158.9474     0.3738 425.179  < 2e-16 ***
#   Diatoms_Z       93.1071     0.4469 208.353  < 2e-16 ***
#   OtherLimpets_Z -15.2669     0.4101 -37.223 2.11e-15 ***
#   TideHeight_Z     6.7330     0.4506  14.942 5.36e-10 ***
#   Predators_Z     -0.2186     0.4087  -0.535    0.601    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.63 on 14 degrees of freedom
# Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9997 
# F-statistic: 1.655e+04 on 4 and 14 DF,  p-value: < 2.2e-16

## 7. Which predictor in the FullModel_Z model has the largest slope (by “large” we mean the absolute value of the slope)? 
# The predictor is with the largest slope is not Diatoms, which appears to better describe the data

## Do several predictors have similar size slopes?
# No, the slopes are all very different now.

## Are any much smaller than others? Remember that the slopes in this model describe how much the response variable (Focal Limpets) increase when each predictor
## increases by 1 standard deviation (sd) in that predictor, where sd is measured with the original dataset.
# The smallest slope is Predators by far.

# Full Model iterations lacking one predictor
FullModel_Z_NoDiatom=lm(FocalLimpets~OtherLimpets_Z+TideHeight_Z+Predators_Z,data=dat)
FullModel_Z_NoOtherLimpets=lm(FocalLimpets~Diatoms_Z+TideHeight_Z+Predators_Z,data=dat)
FullModel_Z_NoTideHeight=lm(FocalLimpets~Diatoms_Z+OtherLimpets_Z+Predators_Z,data=dat)
FullModel_Z_NoPredators=lm(FocalLimpets~Diatoms_Z+OtherLimpets_Z+TideHeight_Z,data=dat)

# R^2 values
summary(FullModel_Z)$r.squared #full model
# 0.9997885
summary(FullModel_Z_NoDiatom)$r.squared #No diatoms
# 0.3440203
summary(FullModel_Z_NoOtherLimpets)$r.squared #No other limpets
# 0.9788583
summary(FullModel_Z_NoTideHeight)$r.squared #No Tide height
# 0.9964158
summary(FullModel_Z_NoPredators)$r.squared #No Predators
# 0.9997842
summary(DiatomsModel)$r.squared #diatoms only
# 0.9701916
summary(OtherLimpetsModel)$r.squared #Other limpets only
# 0.04818731
summary(TideHeightModel)$r.squared #Tide Height only
# 0.3133519
summary(PredatorsModel)$r.squared #Predators only
# 0.01835133

# plotting r^2 values for each model
library(broom)

#make a vector with model names
models=c("DiatomsModel","OtherLimpetsModel","TideHeightModel","PredatorsModel",
         "FullModel_Z","FullModel_Z_NoDiatom","FullModel_Z_NoOtherLimpets",
         "FullModel_Z_NoTideHeight","FullModel_Z_NoPredators")
#pre-allocate space for the rˆ2 values that you will extract
#(repeat NA 9 times and call it "out")
out=rep(NA,length(models))

#for loop - for each model, extract the rˆ2 value and store it in "out".
#don't worry too much about this, but the `get()` function statement is used to retrieve the model
for(i in 1:length(models)){
  out[i]=glance(get(models[i]))$r.squared
}

#make a dataframe with the names of the models and the r2 values of the models
modelr2=data.frame(modelname=models,r2=out)

#make the model name into a factor
modelr2$modelname=factor(modelr2$modelname)

#make a barplot of rˆ2 values for each model
#make sure the bars are ordered from high to low rˆ2 value
ggplot(data=modelr2,aes(x=reorder(models,-r2), y=r2)) +
  geom_bar(stat="identity", fill="darkblue") +
  xlab("Model")+ylab(expression(Rˆ2))+theme_few()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## 8. Which predictor is most important in explaining unique variation in the response
## variable (compare the difference in R2 between the full model and each of the reduced models)? 
# Diatom Abundance by far

## Does this comparison give you a similar answer to the comparison of the
## slopes in the FullModel_Z model? 
# Absolutely. With diatoms included the r2 values are close to 1, but as soon as they are removed the r2 values drop precipitously. Go figure, things gotta eat.

## Do the predictors that are most important in
## explaining unique variation also have the highest variation explained by themselves
## in the univariate models (DiatomsModel, etc)? 
# Yes, this appears to be true for diatom abundance especially, but tide height also has a significant value in the univariate model. Though, when it is removed from the larger additive model there isn't much difference.

## Is the amount of variation explained by each variable alone the same as 
## the drop in R2 when you leave that predictor out?
# No, these models seem to support the effect of diatoms as the most important parameter over all others.

#All models AIC
AIC(DiatomsModel,OtherLimpetsModel,TideHeightModel,PredatorsModel,
    FullModel,FullModel_Z,FullModel_Z_NoDiatom,FullModel_Z_NoOtherLimpets,
    FullModel_Z_NoTideHeight,FullModel_Z_NoPredators)

#                            df       AIC
# DiatomsModel                3 166.69163
# OtherLimpetsModel           3 232.49964
# TideHeightModel             3 226.29526
# PredatorsModel              3 233.08608
# FullModel                   6  78.67214
# FullModel_Z                 6  78.67214
# FullModel_Z_NoDiatom        5 229.42711
# FullModel_Z_NoOtherLimpets  5 164.16431
# FullModel_Z_NoTideHeight    5 130.44479
# FullModel_Z_NoPredators     5  77.05638

#All models delta AIC
AICcmodavg::aictab(cand.set=list(DiatomsModel,OtherLimpetsModel,TideHeightModel,
                                 PredatorsModel,FullModel,
                                 FullModel_Z,FullModel_Z_NoDiatom,
                                 FullModel_Z_NoOtherLimpets,FullModel_Z_NoTideHeight,FullModel_Z_NoPredators),
                   modnames=(c("diatoms only","other limpets only","tide height only","predators only",
                               "full model","full model Z transformed","no diatom","no other limpet",
                               "no tide height","no predators")),second.ord=F)

# Model selection based on AIC:
#   
#                          K    AIC Delta_AIC AICWt Cum.Wt      LL
# no predators             5  77.06      0.00  0.53   0.53  -33.53
# full model Z transformed 6  78.67      1.62  0.24   0.76  -33.34
# full model               6  78.67      1.62  0.24   1.00  -33.34
# no tide height           5 130.44     53.39  0.00   1.00  -60.22
# no other limpet          5 164.16     87.11  0.00   1.00  -77.08
# diatoms only             3 166.69     89.64  0.00   1.00  -80.35
# tide height only         3 226.30    149.24  0.00   1.00 -110.15
# no diatom                5 229.43    152.37  0.00   1.00 -109.71
# other limpets only       3 232.50    155.44  0.00   1.00 -113.25
# predators only           3 233.09    156.03  0.00   1.00 -113.54

#Only full models and leave one out models
AICcmodavg::aictab(cand.set=list(FullModel,FullModel_Z,FullModel_Z_NoDiatom,
                                 FullModel_Z_NoOtherLimpets,FullModel_Z_NoTideHeight,
                                 FullModel_Z_NoPredators),
                   modnames=(c("full model","full model Z transformed","no diatom","no other limpet",
                               "no tide height","no predators")),second.ord=F)

# Model selection based on AIC:
#   
#                          K    AIC Delta_AIC AICWt Cum.Wt      LL
# no predators             5  77.06      0.00  0.53   0.53  -33.53
# full model Z transformed 6  78.67      1.62  0.24   0.76  -33.34
# full model               6  78.67      1.62  0.24   1.00  -33.34
# no tide height           5 130.44     53.39  0.00   1.00  -60.22
# no other limpet          5 164.16     87.11  0.00   1.00  -77.08
# no diatom                5 229.43    152.37  0.00   1.00 -109.71

## 9. Which predictors are most/least important by AIC 
## (which is essentially a measurement of information lost or “badness of fit”, with lower AIC values having less badness and being better!)?
# Least ~predators | Most ~Diatoms + OtherLimpets + TideHeight

## Models without important predictors will have much worse AIC scores (i.e. higher AIC scores). 
## Are the importance of predictors using AIC the same as using R2? 
# AIC versus R2 in this case swaps the importance of the full model with predators being higher R2 and without predators having the lowest delta AIC.
# Taking diatoms out of the model has a very large jump in AIC values.

## Are there variables that weren’t very important in increasing R2 that nonetheless make a big difference by AIC (hint, there is at least one!)?
# It appears that including predators into the full model had a large influence on the AIC scores, but alone not so much. 
# THe same can be said of including other limpets, alone they had little influence, but when incorporated in the full model the AIC went down.

# residuals for each parameter as a response
Diatoms_Resid=resid(lm(Diatoms~OtherLimpets+TideHeight+Predators,data=dat))
OtherLimpets_Resid=resid(lm(OtherLimpets~Diatoms+TideHeight+Predators,data=dat))
TideHeight_Resid=resid(lm(TideHeight~Diatoms+OtherLimpets+Predators,data=dat))
Predators_Resid=resid(lm(Predators~Diatoms+OtherLimpets+TideHeight,data=dat))

# Residuals with focal limpet as response
FocalLimpets_Resid_Diatoms=resid(lm(FocalLimpets~OtherLimpets+TideHeight+Predators,
                                    data=dat))
FocalLimpets_Resid_OtherLimpets=resid(lm(FocalLimpets~Diatoms+TideHeight+Predators,
                                         data=dat))
FocalLimpets_Resid_TideHeight=resid(lm(FocalLimpets~Diatoms+OtherLimpets+Predators,
                                       data=dat))
FocalLimpets_Resid_Predators=resid(lm(FocalLimpets~Diatoms+OtherLimpets+TideHeight,
                                      data=dat))

# residual plots
par(mfrow=c(2,2),mar=c(3.2,3.2,1,1),mgp=c(2.1,1,0)) #create a 2x2 set of plots & shrink margins
plot(FocalLimpets_Resid_Diatoms~Diatoms_Resid, ylab="Focal Limpet residuals")
plot(FocalLimpets_Resid_OtherLimpets~OtherLimpets_Resid)
plot(FocalLimpets_Resid_TideHeight~TideHeight_Resid)
plot(FocalLimpets_Resid_Predators~Predators_Resid)

# 10. Compare these plots to those you made at the very beginning. 
# How are they different? 
# the linear distributions are much more tightly packed together and are easy to interpret.

# How are they similar?
#  Predators are still rather chaotic. Diatoms is about the same.

# fake field visit to new site
DA=125 #diatom abundance
TH=55 #tide height
OL=43 #other limpets
PR=5.3 #predators
-23.395+(1.006*DA)+(-1.054*OL)+(0.978*TH)+(-0.073*PR) # 110.4361

predict(FullModel,newdata=data.frame(Diatoms=DA,OtherLimpets=OL,TideHeight=TH,Predators=PR),
        se.fit=T, interval = "confidence") #SE & CI

# $fit
#        fit      lwr      upr
# 1 110.4502 108.5455 112.3549
# 
# $se.fit
# [1] 0.8880558
# 
# $df
# [1] 14
# 
# $residual.scale
# [1] 1.629515

predict(FullModel,newdata=data.frame(Diatoms=DA,OtherLimpets=OL,TideHeight=TH,Predators=PR),
        interval = "prediction") #PI

#        fit      lwr      upr
# 1 110.4502 106.4699 114.4305

# 11. 
# What is the predicted mean, 110.4502
# SE, 0.8880558
# 95% Confidence Interval, 108.5455 112.3549
# 95% Prediction Interval, 106.4699 114.4305
# number of Focal Limpets for these values of the predictor? Not sure.. 110.45 +- 4

#Make a dataframe of the response and predictor residuals in long format
rdat=data.frame(Focal_Limpet_residuals=c(FocalLimpets_Resid_Diatoms,
                                         FocalLimpets_Resid_OtherLimpets,
                                         FocalLimpets_Resid_TideHeight,
                                         FocalLimpets_Resid_Predators),
                Predictor_residuals=c(Diatoms_Resid,OtherLimpets_Resid,
                                      TideHeight_Resid,Predators_Resid),
                Predictor=rep(c("Diatoms","OtherLimpets","TideHeight","Predators"),
                              each=length(FocalLimpets_Resid_Diatoms)))

#Add the means of the predictor and response to the residuals
rdat$Focal_Limpet_residualsT=rdat$Focal_Limpet_residuals+mean(dat$FocalLimpets)
pred_means=apply(dat[,c("Diatoms","OtherLimpets","TideHeight","Predators")],
                 2,FUN=mean)
rdat$Predictor_residualsT=rdat$Predictor_residuals+rep(pred_means,
                                                       each=length(FocalLimpets_Resid_Diatoms))
ggplot(rdat,aes(x=Predictor_residualsT,y=Focal_Limpet_residualsT))+geom_point()+
  facet_wrap(~Predictor,scales="free",strip.position ="bottom")+theme_few()+
  theme(strip.placement = "outside")+labs(x="Predictor residuals + mean",
                                          y="Focal Limpet residuals + mean")
