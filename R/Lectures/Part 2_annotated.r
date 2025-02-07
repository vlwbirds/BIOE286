# Part 2

# 1. Set up ---------------------------------------------------------------

# Read in packages
lapply(c("tidyverse","ggthemes","lme4","ggpubr","here"),require,character.only=T) #load packages
#setwd("c:/marm/teaching/286/lecture R code/")
# Set working directory
# setwd("~/Documents/U.C. Santa Cruz/TAing/BIOE 286 - Experimental Design and Data Analysis/Winter 2025/Annotated lecture code")


# CRICKET EXAMPLE ------------------------------------------------------


# 2. Set up and explore the relationship ----------------------------------


## SLIDE 6
m=read.csv(here("data/Lectures/RegressionDataCrickets.csv")) #read in the data

# Plot the data with a regression line (stat_smooth())
ggplot(m,aes(x=TempF,y=ChirpsPerMin))+
  theme_few()+
  geom_point(size=3)+
  stat_smooth(method="lm")+
  xlab("Temperature(F)")+
  ylab("Chirps per minute")

# Run a model with lm(y~x), store it as f1, then display the summary of f1
f1=lm(ChirpsPerMin~TempF,data=m);summary(f1)

## SLIDE 7
# Same thing, but add on the equation using stat_regline_equation (in the ggpubr package)
ggplot(m,aes(x=TempF,y=ChirpsPerMin))+theme_few()+
  geom_point(size=3)+stat_smooth(method="lm")+
  xlab("Temperature(F)")+ylab("Chirps per minute")+
  ggpubr::stat_regline_equation()


# 3. Solve for slope and intercept in a couple ways -----------------------

## SLIDE 13
#Solve for slope and intercept using matrix expression
X=as.matrix(data.frame(1,m$TempF)) #Make X matrix
(solve(t(X)%*%X))%*%(t(X)%*%m$ChirpsPerMin) #(X'X)^-1 * (X'Y)


## SLIDE 17
# Run a model with lm(y~x), store it as f1, then display the summary of f1
f1=lm(ChirpsPerMin~TempF,data=m);summary(f1)

#Put regression coefs on plot with predictor and response names
ggplot(m,aes(x=TempF,y=ChirpsPerMin))+ # Plot with dataset m, make the x axis Temp and y Chirps
  theme_few()+ # add one of the preset themes
  geom_point(size=3)+ # plot the data as points
  stat_smooth(method="lm",se=F)+ # Add a line with the lm method
  labs(x="Temperature(F)",y="Chirps per minute")+ # Add labels for the axes
  # Instead of the stat_regline_equation, let's extract the coefficients "by hand" by pulling out the coefficients from the model (coef(f1))[1]
  # the function annotate puts words on the plot. Nice! 
  # You need to tell it what to annotate ("text"), 
  # where to put it, (x = ..., y = ...),
  # and the label to put on.
  annotate(geom="text",x=75,y=18,
           label=paste("Chirps=",round(coef(f1)[1],2),"+",
                       round(coef(f1)[2],2),"*Temp(F)"))


# 4. How do SD and SE differ? ------------------------------------------------

## SLIDE 23

#SE vs SD
set.seed(2) #make draws repeatable
# Get the standard deviation of a distribution of 500 values that we draw randomly from a normal distribution with a mean of 25 and sd of 30
# And then repeat with different means!
sd(rnorm(n=500,mean=25,sd=30))
sd(rnorm(n=5000,mean=25,sd=30))
sd(rnorm(n=5000,mean=125,sd=30)) #sd of 5000 numbers w/ different mean

sd(colMeans(matrix(rnorm(100),ncol=10)))
sd(colMeans(matrix(rnorm(1000),ncol=10)))
sd(colMeans(matrix(rnorm(1000),ncol=100)))
10^.5


# 5. Calculate residuals -----------------------------------------------------


## SLIDE 28
#SE of regression coefs (intercept & slope)
f1=(lm(ChirpsPerMin~TempF,data=m)); summary(f1) # Run the model, look at it
X=as.matrix(data.frame(1,m$TempF));X #Make X matrix

# Calculate the residuals by hand; take the actual data (m$ChirpsPerMin), then subtract the estimated value of the relationship (aka subtract the line from the data)
# We "subtract the line" by writing out the equation with the coefficients we calculated
eps=m$ChirpsPerMin-
  (coef(f1)["(Intercept)"]+
     coef(f1)["TempF"]*m$TempF);eps #residuals by hand!
resid(f1) #same as line above!
sqrt(sum(resid(f1)^2)/(nrow(m)-length(f1$coefficients)))
rse=sigma(f1);rse #same as above!
sqrt(rse^2*diag(solve(t(X)%*%X))) #SEs using our equation!


# 6. Adding in new variables -------------------------------------------------


# * 6.1 Set up new simulated data -----------------------------------------

## SLIDE 35
#Reducing SE of slope by measuring extra variables
set.seed(1) #make example repeatable
dat=data.frame(Temp=rnorm(50,65,1), #create Temp data - draw random numbers from a normal distribution
               FemDens=rnorm(50,30,1)) #create Female Density data - random #s from normal dist

# Run a correlation test to ensure that randomly generated data isn't correlated
cor.test(~Temp+FemDens,data=dat) #predictors aren't correlated

# This creates new data, called SingRate, that is a function of Temp and FemDens plus a little bit of random jittering
dat$SingRate = 1*dat$Temp + 2*dat$FemDens + rnorm(50,2,2.5) #SingRate is influenced by both predictors


# * 6.2 Look at just Temp -------------------------------------------------

## SLIDE 36
# Plot the simulated data! PLot SingRate as a function of Temp Add on the RSE, slope, and SE of the slope
# New - color the points by female density to get a way to visualize the TWO predictor variables against the one response variable
ggplot(dat,aes(y=SingRate,x=Temp))+theme_few()+
  geom_point(aes(col=FemDens),size=3)+
  scale_colour_gradientn(colours = rainbow(4))+
  stat_smooth(method="lm",se=F)+
  annotate(geom="text",x=63.3,y=134,label=paste("RSE=",round(sigma(M_Temp),3)))+
  annotate(geom="text",x=63.3,132,label=paste("Slope=",round(coef(summary(M_Temp))[2,1],3)))+
  annotate(geom="text",x=63.3,130,label=paste("Slope SE=",round(coef(summary(M_Temp))[2,2],3)))

## SLIDE 37

# Now run a linear model on our data with known predictive power with JUST Temp
M_Temp=lm(SingRate~Temp,data=dat);summary(M_Temp) #Model with just Temp
coef(summary(M_Temp))
coef(summary(M_Temp))[2,2] #lazy syntax
coef(summary(M_Temp))["Temp","Std. Error"] #Same thing, better coding practice


# * 6.3 Now incorporate Temp AND FemDens ----------------------------------

## SLIDE 38 & 42

#Model w both Temp AND FemDens (Temp_FemDens)
M_Temp_FemDen=lm(SingRate~Temp+FemDens,data=dat); summary(M_Temp_FemDen) 
# And a model that regresses the residuals of the relationship between SingRate and FemDens 
# against the residualts of the relationship between Temp and FemDens
summary(lm(resid(lm(SingRate~FemDens,data=dat))~ resid(lm(Temp~FemDens,data=dat))))

# Plot the residuals from the model of Singing Rate and Female Density on the y axis
# And plot the residuals from the model of Singing Rate and Temp on the x axis
plot(resid(lm(SingRate~FemDens,data=dat))~ resid(lm(Temp~FemDens,data=dat)),
     ylim=(range(dat$SingRate)-mean(dat$SingRate)), ylab="Resid (SingRate~FemDens)")
# Put a line through the scatterplot of residuals and add some text
abline(lm(resid(lm(SingRate~FemDens,data=dat))~ resid(lm(Temp~FemDens,data=dat))))
text(-1.5,6,paste("RSE=",round(sigma(M_Temp_FemDen),3)) )
text(-1.5,4,paste("Slope=",round( coef(summary(M_Temp_FemDen))[2,1],3)) )
text(-1.5,2,paste("Slope SE=",round( coef(summary(M_Temp_FemDen))[2,2],3)) )

## SLIDE 46

#Compare models w/ and w/out FemDens
M_Temp=lm(SingRate~Temp,data=dat);summary(M_Temp) #Model with just Temp
M_Temp_FemDen=lm(SingRate~Temp+FemDens,data=dat); summary(M_Temp_FemDen) #Model w both Temp FemDens
# 2 times the probability of the negative of the absolute value of the t value 2.714, with 47 degrees of freedom
2*pt(-abs(2.714),47)
2*pt(-abs(-.12),47)

# Two tailed for p = 0.05 and for a given degrees of freedom (df):
# qt(0.025, df)
qt(0.025, 47)



# New example!
set.seed(1) #make example repeatable
dat=data.frame(Temp=rnorm(50,65,1), #create Temp data
               FemDens=rnorm(50,30,1))#create Female Density data
dat$SingRate=1*dat$Temp+
  2*dat$FemDens+rnorm(50,2,2.5) #SingRate is influenced by both predictors
set.seed(3) #make example repeatable
dat$FireFlies=rnorm(50,15,1) #Make unrelated variable
cor.test(~Temp+FireFlies,data=dat) #Fireflies & Temp aren't correlated
cor.test(~FemDens+FireFlies,data=dat) #Fireflies & FemDens aren't correlated
M_Temp_FemDen=lm(SingRate~Temp+FemDens,data=dat); summary(M_Temp_FemDen) #Model w both Temp FemDens
M_Temp_FemDen_Firefly=lm(SingRate~Temp+FemDens+FireFlies,data=dat); summary(M_Temp_FemDen_Firefly) #Model w both Temp FemDens


## SLIDE 47

# Plot a normal distribution with a line at -1.96
curve(dnorm(x),xlim=c(-4,4));abline(h=0,v=0);abline(v=-1.96)

# dnorm- the probability (height) for a given value
# pnorm- the cumulative probability from -∞ to that value
# qnorm- the inverse of pnorm
# rnorm- generate a normally distributed random number

# For a normal distribution (_norm, which uses x,mean, sd), try these:
dnorm(0,mean=0,sd=1)
pnorm(-1.96,0,1)
qnorm(0.025,0,1)
rnorm(10,0,1)
rnorm(10,100,20)

# 7. Simulate how increasing variance of X reduces SE ---------------------

## SLIDE 52

#Reduce SE by increasing variance of X (Temperature)
# Generate two datasets based on different temp variables - one with a narrow range (low sd) and one with a wide range (large sd)
set.seed(1) #make example repeatable
dat2=data.frame(TempN=rnorm(50,65,1), #create narrow Temp range - ~63-67
                TempW=rnorm(50,65,5)) #create wide Temp range - ~55-75
dat2$SingRateN=1*dat2$TempN+rnorm(50,2,2.5) #SingRate w/ narrow Temp
dat2$SingRateW=1*dat2$TempW+rnorm(50,2,2.5) #SingRate w/ wide Temp

## SLIDE 53

# Linear model of sing rate N as a func. of Temp
M_TempN=lm(SingRateN~TempN,data=dat2);summary(M_TempN)
plot(SingRateN~TempN,data=dat2,pch=19);abline(M_TempN);
text(63.3,72,paste("RSE=",round(sigma(M_TempN),3)) )
text(63.3,70,paste("Slope=", round(coef(summary(M_TempN))[2,1],3)) )
text(63.3,68,paste("Slope SE=", round(coef(summary(M_TempN))[2,2],3)) )
M_TempW=lm(SingRateW~TempW,data=dat2);summary(M_TempW)
plot(SingRateW~TempW,data=dat2,pch=19);abline(M_TempW);
text(59,79,paste("RSE=",round(sigma(M_TempW),3)) )
text(59,75,paste("Slope=",round(coef(summary(M_TempW))[2,1],3)) )
text(59.3,71,paste("Slope SE=",round(coef(summary(M_TempW))[2,2],3)) )


# 8. Making new predictions -----------------------------------------------



#Making new predictions
f1=(lm(ChirpsPerMin~TempF,data=m)); summary(f1) #Model
#Predict values of Y (chirps) for all Temp data points, by hand
ChirpsPerMin_pred=coef(f1)["(Intercept)"]+coef(f1)["TempF"]*m$TempF
ChirpsPerMin_pred

#Predict 20 new points at 1 temp
set.seed(99) #Make repeatable
par(mfrow=c(1,1),mar=c(3.5,3.5,0.5,0.5),mgp=c(2.5,1,0))#Plotting params b,l,t,r
plot(ChirpsPerMin~TempF,data=m, xlab="Temperature, degrees F",ylab="Chirps per minute",
     ylim=c(13,20),pch=19)
abline(f1)
text(76,20,paste("Chirps=", round(coef(f1)[1],2),"+",round(coef(f1)[2],2),"*Temp(F)"))

## SLIDE 59
SE_ci=sigma(f1)*sqrt(1/nrow(m)+(90-mean(m$TempF))^2/(var(m$TempF)*(nrow(m)-1)) );SE_ci
#Same as line above
predict(f1,newdata=data.frame(TempF=90),se=T,interval="confidence")$se.fit
Ynew=rnorm(20,predict(f1,newdata=data.frame(TempF=90)),SE_ci)
points(rep(90,length(Ynew)),Ynew,col="red",pch=1) #Add 20 new predicted points
points(90,mean(Ynew),col="red",pch=19) #Add mean of new predicted points


## SLIDE 61
#Add Confidence and Prediction interval lines/curves
nd1=data.frame(TempF=seq(min(m$TempF),max(m$TempF),by=0.1))
nd1$CI_l=predict(f1,newdata=nd1,se=T,interval="confidence")$fit[,"lwr"]
nd1$CI_u=predict(f1,newdata=nd1,se=T,interval="confidence")$fit[,"upr"]
nd1$PI_l=predict(f1,newdata=nd1,se=T,interval="prediction")$fit[,"lwr"]
nd1$PI_u=predict(f1,newdata=nd1,se=T,interval="prediction")$fit[,"upr"]
lines(CI_l~TempF,data=nd1,col="red",lty=1)
lines(CI_u~TempF,data=nd1,col="red",lty=1)
lines(PI_u~TempF,data=nd1,col="red",lty=2)
lines(PI_l~TempF,data=nd1,col="red",lty=2)


# 9. Extrapolate beyond x-axis --------------------------------------------

## SLIDE 63/64

#Crazy extrapolation
#Make N=50 new predictions at 25, 110 F
T_extrapL=25;T_extrapH=110;newP=50
SE_ci_L=predict(f1,newdata=data.frame(TempF=T_extrapL),se=T,interval="prediction")$se.fit
SE_ci_H=predict(f1,newdata=data.frame(TempF=T_extrapH),se=T,interval="prediction")$se.fit
set.seed(9)
Ynew=data.frame(TempF=rep(c(T_extrapL,T_extrapH),each=newP),
                ChirpsPerMin=c(rnorm(newP,predict(f1,newdata=data.frame(TempF=T_extrapL)),SE_ci_L),
                               rnorm(newP,predict(f1,newdata=data.frame(TempF=T_extrapH)),SE_ci_H) ) )
#Make confidence and prediction intervals for full range of temps
nd1=data.frame(TempF=seq(T_extrapL,T_extrapH,by=0.1))
nd1$CI_l=predict(f1,newdata=nd1,se=T,interval="confidence")$fit[,"lwr"]
nd1$CI_u=predict(f1,newdata=nd1,se=T,interval="confidence")$fit[,"upr"]
nd1$PI_l=predict(f1,newdata=nd1,se=T,interval="prediction")$fit[,"lwr"]
nd1$PI_u=predict(f1,newdata=nd1,se=T,interval="prediction")$fit[,"upr"]
par(mfrow=c(1,1),mar=c(3.5,3.5,0.5,0.5),mgp=c(2.5,1,0))#Plotting params b,l,t,r
plot(ChirpsPerMin~TempF,data=m, xlab="Temperature, degrees F",ylab="Chirps per minute",
     xlim=c(T_extrapL,T_extrapH+1),ylim=c(min(nd1$PI_l),max(nd1$PI_u)),pch=19)
abline(f1)
text(45,20,paste("Chirps=", round(coef(f1)[1],2),"+",round(coef(f1)[2],2),"*Temp(F)"))
points(Ynew$TempF,Ynew$ChirpsPerMin,col="red",pch=1)
lines(CI_l~TempF,data=nd1,col="red",lty=1)
lines(CI_u~TempF,data=nd1,col="red",lty=1)
lines(PI_u~TempF,data=nd1,col="red",lty=2)
lines(PI_l~TempF,data=nd1,col="red",lty=2)

## SLIDE 67

#Predict a new mean at 90F
predict(f1,newdata=data.frame(TempF=90),se=T,interval="confidence")
#make new individual point predictions at 90F
predict(f1,newdata=data.frame(TempF=90),interval="prediction")
#If you want the SE for new predicted points, add se=T (also see code above)
predict(f1,newdata=data.frame(TempF=90),se=T,interval="prediction")

## SLIDE 68

#GGplot does confidence intervals with one line!
ggplot(m,aes(x=TempF,y=ChirpsPerMin))+theme_few()+
  geom_point(size=3)+
  stat_smooth(method="lm")+
  labs(x="Temperature(F)",y="Chirps per minute")


## SLIDE 77

#How good is our analysis?
#Plot w/ red point and arrows
par(mfrow=c(1,1),mar=c(3.5,3.5,0.5,0.5),mgp=c(2.5,1,0))#Plotting params b,l,t,r
plot(ChirpsPerMin~TempF,data=m,xlab="Temperature, degrees F",ylab="Chirps per minute",pch=19)
abline(h=mean(m$ChirpsPerMin),lty=2);abline(f1)
points(ChirpsPerMin~TempF,data=m[4,],col="red",pch=19)
m$pred_Chirps=predict(f1)
arrows(x0=84.3,y0=mean(m$ChirpsPerMin),y1=m$pred_Chirps[4])
arrows(x0=84.3,y1=m$ChirpsPerMin[4],y0=m$pred_Chirps[4])


## SLIDE 70
#Plot with segments from points to mean
plot(ChirpsPerMin~TempF,data=m,xlab="Temperature, degrees F",ylab="Chirps per minute")
abline(h=mean(m$ChirpsPerMin))#;abline(v=mean(m$TempF))
segments(x0=m$TempF,x1=m$TempF,y0=m$ChirpsPerMin,y1=mean(m$ChirpsPerMin))

## SLIDE 70
#Plot with segments from points to predicted line
plot(ChirpsPerMin~TempF,data=m,xlab="Temperature, degrees F",ylab="Chirps per minute",pch=19)
abline(f1)
segments(x0=m$TempF,x1=m$TempF,y0=m$ChirpsPerMin,y1=m$pred_Chirps)

## SLIDE 71

#What do different values of R2 look like?
set.seed(11)
xq=0.08
par(mfrow=c(3,2),mar=c(.5,.5,0.5,0.5),mgp=c(.5,.5,0),cex=1.25)#Plotting params b,l,t,r
x=rnorm(50);y=rnorm(50,0,2)
g1=lm(y~x);summary(g1)
plot(y~x,xaxt="n",yaxt="n",xlab="",ylab="");text(quantile(x,xq),quantile(y,.975),bquote(R^2 == .(round(summary(g1)$r.squared, digits = 2))))
x=rnorm(50);y=x+rnorm(50,0,2)
g1=lm(y~x);summary(g1)
plot(y~x,xaxt="n",yaxt="n",xlab="",ylab="");text(quantile(x,xq),quantile(y,.975),bquote(R^2 == .(round(summary(g1)$r.squared, digits = 2))))
x=rnorm(50);y=x+rnorm(50)
g1=lm(y~x)
plot(y~x,xaxt="n",yaxt="n",xlab="",ylab="");text(quantile(x,xq),quantile(y,.975),bquote(R^2 == .(round(summary(g1)$r.squared, digits = 2))))
x=rnorm(50);y=-x-rnorm(50,0,.5) #negative relatinship
g1=lm(y~x);summary(g1)
plot(y~x,xaxt="n",yaxt="n",xlab="",ylab="");text(quantile(x,xq),quantile(y,xq),bquote(R^2 == .(round(summary(g1)$r.squared, digits = 2))))
x=rnorm(50);y=x+rnorm(50,0,.25)
g1=lm(y~x)
plot(y~x,xaxt="n",yaxt="n",xlab="",ylab="");text(quantile(x,xq),quantile(y,.975),bquote(R^2 == .(round(summary(g1)$r.squared, digits = 2))))
x=rnorm(50);y=x+rnorm(50,0,0.01)
g1=lm(y~x);summary(g1)
plot(y~x,xaxt="n",yaxt="n",xlab="",ylab="");text(quantile(x,xq),quantile(y,.975),bquote(R^2 == .(round(summary(g1)$r.squared, digits = 2))))

## SLIDE 74

#R2 slopes and P-values
set.seed(2)
xq=0.02
par(mfrow=c(2,1),mar=c(.5,.5,0.5,0.5),mgp=c(.5,.5,0),cex=1.25)#Plotting params b,l,t,r
n1=20;n2=300
x=rnorm(n1);y=1.*x+rnorm(n1,0,1)
g1=lm(y~x)
plot(y~x,xaxt="n",yaxt="n",xlab="",ylab="");text(quantile(x,4*xq),quantile(y,.99),bquote(R^2 == .(round(summary(g1)$r.squared, digits = 2))))
abline(g1)
text(quantile(x,4*xq),quantile(y,.92),paste("P =",format(coef(summary(g1))[2,4], digits = 2)))
x=rnorm(n2);y=x+rnorm(n2,0,3)
g1=lm(y~x)
plot(y~x,xaxt="n",yaxt="n",xlab="",ylab="");text(quantile(x,xq),quantile(y,.995),bquote(R^2 == .(round(summary(g1)$r.squared, digits = 2))))
text(quantile(x,xq),quantile(y,.98),paste("P =",format(coef(summary(g1))[2,4], digits = 2)))
abline(g1)

## SLIDE 78

#F-statistic for whole model by hand
set.seed(1) #make example repeatable
dat=data.frame(Temp=rnorm(50,65,1), #create Temp data
               FemDens=rnorm(50,30,1)) #create Female Density data
dat$SingRate=1*dat$Temp+
  2*dat$FemDens+rnorm(50,2,2.5) #SingRate is influenced by both predictors
M_Temp_FemDen=lm(SingRate~Temp+FemDens,data=dat); summary(M_Temp_FemDen) #Model w both Temp FemDens

## SLIDE 80
SSy=with(dat,sum((SingRate-mean(SingRate))**2))
SSE=sum(M_Temp_FemDen$residuals**2)
k=length(M_Temp_FemDen$coefficients);n=nrow(dat)
Fv=((SSy-SSE)/(k-1)) / (SSE/(n-k));Fv #F-value
1-pf(Fv,k-1,n-k) #P-value for this F value



## SLIDE 85

#Lm assumptions
#Independence of residuals example:
set.seed(1)
x=c(rnorm(50,5,3),rnorm(50,10,3))
d=data.frame(x=x,y=rep(c(1,1.5),each=50)+.1*(rnorm(100,0,3)),
             taxa=rep(c("Birds","Reptiles"),each=50))
ggplot(d,aes(x=x,y=y))+geom_point()+theme_few()+
  xlab("Body mass(kg)")+ylab("Brain Mass(kg)")+
  geom_smooth(method="lm")+stat_cor(label.y = 1.9)+ 
  stat_regline_equation(label.y = 2)

## SLIDE 86

ggplot(d,aes(x=x,y=y,color=taxa))+geom_point(size=2)+theme_few()+
  xlab("Body mass(kg)")+ylab("Brain Mass(kg)")+ 
  geom_smooth(method="lm")+stat_cor(label.y = c(1.7,1.9))+ 
  stat_regline_equation(label.y = c(1.8,2))+ theme(legend.position="top")


## SLIDE 90

#Linearity plots
npg=100
x = rep(runif(npg,1,100),4)
d=data.frame(x=x,y=c(.1*(.01*x[1:npg]^2)+rnorm(npg,0,1),
                     .1*x[1:npg]+rnorm(npg),
                     abs(x[1:npg])^.25+rnorm(npg,0,.1),
                     1/(1+exp(.25*x[1:npg]-2.5))+rnorm(npg,0,.1)),
             group=rep(LETTERS[1:4],each=npg))
ggplot(d,aes(x=x,y=y,group=group))+geom_point()+
  facet_wrap(~group,scales="free")+theme_few()+
  stat_smooth(method="lm")


## SLIDE 91

#Normality
set.seed(2)
x=rnorm(20,10,4)
d2=data.frame(y=round(x/4),x=x)
ggplot(d2,aes(x=x,y=y))+theme_few()+
  geom_point(size=3)+
  xlab("Fish size(cm)")+ylab("Number of matings")

fd2=lm(y~x,data=d2)
d2$resid=resid(fd2)
d2$predy=predict(fd2)
ggplot(d2,aes(x=predy,y=resid))+theme_few()+
  geom_point(size=3)+
  labs(x="Predicted # matings",y="Residuals")

qqnorm(resid(lm(y~x,data=d2)),ylab="Residuals",pch=19)
qqline(resid(lm(y~x,data=d2)))
ggplot(d2,aes(sample=resid))+theme_few()+
  stat_qq()+stat_qq_line()

## SLIDE 95
#R's built in diagnostics plots
set.seed(1);df=data.frame(x=rnorm(100),y=rnorm(100))
f1=lm(y~x,data=df)
par(mfrow=c(2,2));plot(f1)  


## SLIDE 96

#Anscombe's quartet
m=read.csv("AnscomeQuartet.csv")
summary(lm(Y~X,data=m[m$Group=="A",]))
ggplot(m,aes(x=X,y=Y,group=Group))+theme_few()+
  geom_point()+
  facet_wrap(~Group,scales="free")+
  geom_smooth(method="lm",linewidth=.5)

## SLIDE 98

#Centering, scaling - z-transforming
set.seed(1)
m=data.frame(x1=rnorm(100,10,1))
m = m %>% mutate(y=x1+rnorm(100),
                 x1_cent=x1-mean(x1),
                 x1_ztrans=x1_cent/sd(x1),
                 y_cent=y-mean(y),
                 y_ztrans=y_cent/sd(y))
f1=lm(y~x1,data=m);summary(f1)
f2=lm(y_cent~x1,data=m);summary(f2)
f3=lm(y_cent~x1_cent,data=m);summary(f3)
f4=lm(y_ztrans~x1_ztrans,data=m);summary(f4)
f5=lm(x1~y,data=m);summary(f5)

