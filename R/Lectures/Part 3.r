#Part 3 General linear models comparing groups
lapply(c("tidyverse","ggthemes","multcomp","emmeans","Hmisc","car"),require,character.only=T) #load packages

setwd("c:/marm/teaching/286/lecture R code/")
m=read.csv("MoltDataForMarm.csv")
m=m[order(m$MoltStartDate),]
head(m)
table(m$AgeSexCat)

ggplot(m,aes(x=AgeSexCat,y=MoltDuration))+geom_boxplot()+geom_jitter(height=0)+theme_few()
f1=lm(MoltDuration~AgeSexCat,data=m);summary(f1)

#relevel
m$AgeSexCat=relevel(factor(m$AgeSexCat), ref="JuvenileMale") 
f1=lm(MoltDuration~AgeSexCat,data=m);summary(f1)

#Under the hood
m=read.csv("MoltDataForMarm.csv") #re-read in to remove releveling
m$dummyAF_Int=1 #Intercept for reference group
m$dummyJF=0;m$dummyJF[m$AgeSexCat=="JuvenileFemale"]=1
m$dummyJM=0;m$dummyJM[m$AgeSexCat=="JuvenileMale"]=1
m2=m[order(m$MoltDuration),c("MoltDuration","AgeSexCat","dummyJF","dummyJM")]
m2[395:400,] #for showing snippet of data

f2=lm(MoltDuration~-1+dummyAF_Int+dummyJF+dummyJM,data=m);summary(f2)
f1=lm(MoltDuration~AgeSexCat,data=m);summary(f1) #compare w/ original model
f3=lm(MoltDuration~dummyJF+dummyJM,data=m);summary(f3) #same as f2 w/out manual intercept

#pairwise comparisons w/ emmeans
emmeans(f1, pairwise ~ AgeSexCat) #similar but not identical to glht despite both being Tukey adjusted
E1=emmeans(f1, pairwise ~ AgeSexCat)
multcomp::cld(E1)

#pairwise comparisons w/ multcomp::glht; similar to emmeans
m$AgeSexCat=factor(m$AgeSexCat) #needed for glht()
f1=lm(MoltDuration~AgeSexCat,data=m);summary(f1)
summary(glht(f1,linfct = mcp(AgeSexCat = "Tukey")))

#Eyeball comparisons, overlapping SE, CI error bars
ants = data.frame(place=rep(c("field","forest"),c(4,6)),
                  colonies=c(c(12,9,12,10),9,6,4,6,7,10)) 
ggplot(data=ants, aes(x=place, y=colonies))+ 
  stat_summary(fun.data = "mean_cl_normal", 
               colour="red", size=1)+  geom_jitter()+theme_few()


#lm Diagnostics and assumptions
par(mfrow=c(2,2), mar = c(3.5, 3.5, 1.5, 1.5),mgp=c(1.5,.5,0))#b,l,t,r
plot(f1)

#Proper tests
#Normality
shapiro.test(resid(f1)) #fails - data not normal
#Constant variance
leveneTest(f1) #fails - variance not constant
f1=lm(MoltDuration~AgeSexCat,data=m);summary(f1) #compare w/ original model
#fligner.test(f1)#fails
fligner.test(MoltDuration~AgeSexCat,data=m) #works, but variance not constant
bartlett.test(MoltDuration~AgeSexCat,data=m) #works, but variance not constant

