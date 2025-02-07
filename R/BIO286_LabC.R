########################################################################################################
########################################################################################################
############################# BIOE 286 - Lab C #########################################################
#############################   Vince Weber    #########################################################
########################################################################################################
########################################################################################################

library(here)
library(tidyverse)
library(ggpubr)
library(lmtest)

#############################################
# 1) Optimizing sample size
#############################################

# abalone data
dat <- read_csv(here("data/LabC/AbaloneSizes2.csv"))
#view(dat)
#str(dat)

table(dat$SampleSize,dat$Group)

#       a   b   c   d   e   f   g   h   i   j
# 5     5   5   5   5   5   5   5   5   5   5
# 10   10  10  10  10  10  10  10  10  10  10
# 25   25  25  25  25  25  25  25  25  25  25
# 50   50  50  50  50  50  50  50  50  50  50
# 100 100 100 100 100 100 100 100 100 100 100
# 500 500 500 500 500 500 500 500 500 500 500

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

# At what sample size does the mean abalone size appear to stabilize (i.e. all 10 values
# are close to each other)? Does this match with another panel of the figure?
#
# They converge at the 500 sample size, and it's similar to the SD (Panel C)
#
#   
# Although a larger sample size is always better, it costs money and time to collect
# more data. Is there a sample size that might be a decent trade-off between maximize
# sampling efficiency and to minimizing error?
#
# a sample size of 50 appears sufficient
#

##################################################################
########### 2) Extracting model components ###################### 
##################################################################

set.seed(1)
FakeData=data.frame(Time=rnorm(n=1000,mean=50,sd=5))
FakeData$ConfidenceBeg=0+1.5*FakeData$Time+rnorm(n=1000,mean=0,sd=5)
head(FakeData)

#       Time ConfidenceBeg
# 1 46.86773      75.97642
# 2 50.91822      81.93698
# 3 45.82186      64.37890
# 4 57.97640      88.01826
# 5 51.64754      77.81829
# 6 45.89766      60.53324

ggplot(data=FakeData,aes(x=Time,y=ConfidenceBeg))+
  geom_point()

fit=lm(ConfidenceBeg~Time,data=FakeData); summary(fit)

# Call:
#   lm(formula = ConfidenceBeg ~ Time, data = FakeData)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -16.2422  -3.3598  -0.0688   3.7770  18.2216 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.40256    1.59708  -0.252    0.801    
# Time         1.50643    0.03181  47.359   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.202 on 998 degrees of freedom
# Multiple R-squared:  0.6921,	Adjusted R-squared:  0.6918 
# F-statistic:  2243 on 1 and 998 DF,  p-value: < 2.2e-16

## Testing for normality ##
par(mfrow=c(2,2)) #make a 2x2 matrix of plotsd
plot(fit) #make 4 diagnostic plots

#Normality of residuals:
shapiro.test(resid(fit))

# Shapiro-Wilk normality test
# 
# data:  resid(fit)
# W = 0.99932, p-value = 0.9824

#Linearity of the relationship:
summary(lm(resid(fit)~poly(predict(fit),2)))

# Call:
#   lm(formula = resid(fit) ~ poly(predict(fit), 2))
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -16.4471  -3.3990  -0.0769   3.7820  18.0147 
# 
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)
# (Intercept)            -4.463e-16  1.645e-01   0.000    1.000
# poly(predict(fit), 2)1  3.529e-14  5.203e+00   0.000    1.000
# poly(predict(fit), 2)2  4.561e+00  5.203e+00   0.877    0.381
# 
# Residual standard error: 5.203 on 997 degrees of freedom
# Multiple R-squared:  0.0007702,	Adjusted R-squared:  -0.001234 
# F-statistic: 0.3842 on 2 and 997 DF,  p-value: 0.6811
#
#This fits the residuals to a quadratic polynomial model.
#If the 2nd term of the polynomial (the ,2) is significant,
#there is non-linearity in the residuals

#Homoscedasticity (constant variance):
bptest(fit)

# studentized Breusch-Pagan test
# 
# data:  fit
# BP = 0.16709, df = 1, p-value = 0.6827
#
#P-values <0.05 indicate non-constant variance (heteroskedasticity)

#Independence of residuals for temporal data
#(i.e. data points taken on successive days/weeks/etc.);
#your response variable needs to be sorted by date:

pacf(resid(fit)) 
#This makes a plot of the partial autocorrelation of the residuals,
#which is the correlation between the residuals.
#If the bars extend above or below the blue dashed lines,
#this indicates significant correlation at that timestep.

coef(fit) #gives intercept and slope
# (Intercept)        Time 
# -0.4025621   1.5064325

coef(summary(fit)) # yay more data
#               Estimate Std. Error    t value      Pr(>|t|)
# (Intercept) -0.4025621 1.59708010 -0.2520613  8.010456e-01
# Time         1.5064325 0.03180874 47.3590828 1.687125e-257

coef(summary(fit))[2] # time slope
coef(summary(fit))["Time","Estimate"] #same thing, more specific
## [1] 1.506433



############################################################
############ 3) Stats annotations, fitted lines, and CIs
############################################################

# plot lm to FakeData
ggplot(data=FakeData,aes(x=Time,y=ConfidenceBeg))+geom_point()+
  geom_smooth(method="lm",colour="purple")+
  stat_cor(label.y = c(100),color="purple")+
  stat_regline_equation(label.y = c(105),color="purple")

# adding a group for advanced coders
FakeData$ConfidenceAdv=75+0.5*FakeData$Time+rnorm(n=1000,mean=0,sd=5)

# pivot to long form taking confidence differences Beg to Avg into a single column
FakeDataLong=pivot_longer(FakeData,cols=c("ConfidenceBeg","ConfidenceAdv"),
                          names_to="Group",values_to="Confidence")

# plot showing confidence difference in beginner and advance groups over time
ggplot(data=FakeDataLong,aes(x=Time,y=Confidence,fill=Group))+
  geom_point(aes(color=Group))+
  geom_smooth(aes(color=Group,fill=Group),method="lm")+
  stat_cor(label.x=c(55,55),label.y = c(50,60),aes(color=Group))+
  stat_regline_equation(label.x=c(55,55),label.y = c(55,65),aes(color=Group))

# fit for advanced
fit2=lm(Confidence~Time,subset(FakeDataLong,Group=="ConfidenceAdv"))
summary(fit2)
# fit for time:group interaction
fit3=lm(Confidence~Time*Group,FakeDataLong)
summary(fit3)
