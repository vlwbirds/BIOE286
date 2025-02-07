lapply(c("tidyverse","ggthemes","lme4","ggpubr","here"),require,character.only=T) #load packages

m=read.csv(here("data/Lectures/RegressionDataCrickets.csv")) #read in the data
ggplot(m,aes(x=TempF,y=ChirpsPerMin))+theme_few()+
  geom_point(size=3)+stat_smooth(method="lm")+
  xlab("Temperature(F)")+ylab("Chirps per minute")

f1=lm(ChirpsPerMin~TempF,data=m);summary(f1)

ggplot(m,aes(x=TempF,y=ChirpsPerMin))+theme_few()+
  geom_point(size=3)+stat_smooth(method="lm")+
  xlab("Temperature(F)")+ylab("Chirps per minute")+
  ggpubr::stat_regline_equation()

#Solve for slope and intercept using matrix expression
X=as.matrix(data.frame(1,m$TempF)) #Make X matrix
(solve(t(X)%*%X))%*%(t(X)%*%m$ChirpsPerMin) #(X'X)^-1 * (X'Y)

#Put regression coefs on plot with predictor and response names
ggplot(m,aes(x=TempF,y=ChirpsPerMin))+theme_few()+
  geom_point(size=3)+
  stat_smooth(method="lm",se=F)+
  labs(x="Temperature(F)",y="Chirps per minute")+
  annotate(geom="text",x=75,y=18,
           label=paste("Chirps=",round(coef(f1)[1],2),"+",
                       round(coef(f1)[2],2),"*Temp(F)"))

#SE vs SD
set.seed(2) #make draws repeatable
sd(rnorm(n=500,mean=25,sd=30))
sd(rnorm(n=5000,mean=25,sd=30))
sd(rnorm(n=5000,mean=125,sd=30)) #sd of 5000 numbers w/ different mean

sd(colMeans(matrix(rnorm(100),ncol=10)))
sd(colMeans(matrix(rnorm(1000),ncol=10)))
sd(colMeans(matrix(rnorm(1000),ncol=100)))
10^.5

#SE of regression coefs (intercept & slope)
f1=(lm(ChirpsPerMin~TempF,data=m)); summary(f1)
X=as.matrix(data.frame(1,m$TempF));X #Make X matrix

eps=m$ChirpsPerMin-
  (coef(f1)["(Intercept)"]+
     coef(f1)["TempF"]*m$TempF);eps #residuals by hand!
resid(f1) #same as line above!
sqrt(sum(resid(f1)^2)/(nrow(m)-length(f1$coefficients)))
rse=sigma(f1);rse #same as above!
sqrt(rse^2*diag(solve(t(X)%*%X))) #SEs using our equation!

#Reducing SE of slope by measuring extra variables
set.seed(1) #make example repeatable
dat=data.frame(Temp=rnorm(50,65,1), #create Temp data
               FemDens=rnorm(50,30,1)) #create Female Density data
cor.test(~Temp+FemDens,data=dat) #predictors aren't correlated
dat$SingRate=1*dat$Temp+
  2*dat$FemDens+rnorm(50,2,2.5) #SingRate is influenced by both predictors
M_Temp=lm(SingRate~Temp,data=dat);summary(M_Temp) #Model with just Temp
coef(summary(M_Temp))
coef(summary(M_Temp))[2,2] #lazy syntax
coef(summary(M_Temp))["Temp","Std. Error"] #Same thing, better coding practice

ggplot(dat,aes(y=SingRate,x=Temp))+theme_few()+
  geom_point(size=3)+
  stat_smooth(method="lm",se=F)+
  annotate(geom="text",x=63.3,y=134,label=paste("RSE=",round(sigma(M_Temp),3)))+
  annotate(geom="text",x=63.3,132,label=paste("Slope=",round(coef(summary(M_Temp))[2,1],3)))+
  annotate(geom="text",x=63.3,130,label=paste("Slope SE=",round(coef(summary(M_Temp))[2,2],3)))

M_Temp_FemDen=lm(SingRate~Temp+FemDens,data=dat); summary(M_Temp_FemDen) #Model w both Temp FemDens
summary(lm(resid(lm(SingRate~FemDens,data=dat))~ resid(lm(Temp~FemDens,data=dat))))

plot(resid(lm(SingRate~FemDens,data=dat))~ resid(lm(Temp~FemDens,data=dat)),
     ylim=(range(dat$SingRate)-mean(dat$SingRate)), ylab="Resid (SingRate~FemDens)")
abline(lm(resid(lm(SingRate~FemDens,data=dat))~ resid(lm(Temp~FemDens,data=dat))))
text(-1.5,6,paste("RSE=",round(sigma(M_Temp_FemDen),3)) )
text(-1.5,4,paste("Slope=",round( coef(summary(M_Temp_FemDen))[2,1],3)) )
text(-1.5,2,paste("Slope SE=",round( coef(summary(M_Temp_FemDen))[2,2],3)) )

#Compare models w/ and w/out FemDens
M_Temp=lm(SingRate~Temp,data=dat);summary(M_Temp) #Model with just Temp
M_Temp_FemDen=lm(SingRate~Temp+FemDens,data=dat); summary(M_Temp_FemDen) #Model w both Temp FemDens
2*pt(-abs(2.714),47)
2*pt(-abs(-.12),47)

#Reduce SE by increasing variance of X (Temperature)
set.seed(1) #make example repeatable
dat2=data.frame(TempN=rnorm(50,65,1), #create narrow Temp range
                TempW=rnorm(50,65,5)) #create wide Temp range
dat2$SingRateN=1*dat2$TempN+rnorm(50,2,2.5) #SingRate w/ narrow Temp
dat2$SingRateW=1*dat2$TempW+rnorm(50,2,2.5) #SingRate w/ wide Temp
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
SE_ci=sigma(f1)*sqrt(1/nrow(m)+(90-mean(m$TempF))^2/(var(m$TempF)*(nrow(m)-1)) );SE_ci
#Same as line above
predict(f1,newdata=data.frame(TempF=90),se=T,interval="confidence")$se.fit
Ynew=rnorm(20,predict(f1,newdata=data.frame(TempF=90)),SE_ci)
points(rep(90,length(Ynew)),Ynew,col="red",pch=1) #Add 20 new predicted points
points(90,mean(Ynew),col="red",pch=19) #Add mean of new predicted points

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

#Predict a new mean at 90F
predict(f1,newdata=data.frame(TempF=90),se=T,interval="confidence")
#make new individual point predictions at 90F
predict(f1,newdata=data.frame(TempF=90),interval="prediction")
#If you want the SE for new predicted points, add se=T (also see code above)
predict(f1,newdata=data.frame(TempF=90),se=T,interval="prediction")

#GGplot does confidence intervals with one line!
ggplot(m,aes(x=TempF,y=ChirpsPerMin))+theme_few()+
  geom_point(size=3)+
  stat_smooth(method="lm")+
  labs(x="Temperature(F)",y="Chirps per minute")

#How good is our analysis?
#Plot w/ red point and arrows
par(mfrow=c(1,1),mar=c(3.5,3.5,0.5,0.5),mgp=c(2.5,1,0))#Plotting params b,l,t,r
plot(ChirpsPerMin~TempF,data=m,xlab="Temperature, degrees F",ylab="Chirps per minute",pch=19)
abline(h=mean(m$ChirpsPerMin),lty=2);abline(f1)
points(ChirpsPerMin~TempF,data=m[4,],col="red",pch=19)
m$pred_Chirps=predict(f1)
arrows(x0=84.3,y0=mean(m$ChirpsPerMin),y1=m$pred_Chirps[4])
arrows(x0=84.3,y1=m$ChirpsPerMin[4],y0=m$pred_Chirps[4])

#Plot with segments from points to mean
plot(ChirpsPerMin~TempF,data=m,xlab="Temperature, degrees F",ylab="Chirps per minute")
abline(h=mean(m$ChirpsPerMin))#;abline(v=mean(m$TempF))
segments(x0=m$TempF,x1=m$TempF,y0=m$ChirpsPerMin,y1=mean(m$ChirpsPerMin))

#Plot with segments from points to predicted line
plot(ChirpsPerMin~TempF,data=m,xlab="Temperature, degrees F",ylab="Chirps per minute",pch=19)
abline(f1)
segments(x0=m$TempF,x1=m$TempF,y0=m$ChirpsPerMin,y1=m$pred_Chirps)

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

#F-statistic for whole model by hand
set.seed(1) #make example repeatable
dat=data.frame(Temp=rnorm(50,65,1), #create Temp data
               FemDens=rnorm(50,30,1)) #create Female Density data
dat$SingRate=1*dat$Temp+
  2*dat$FemDens+rnorm(50,2,2.5) #SingRate is influenced by both predictors
M_Temp_FemDen=lm(SingRate~Temp+FemDens,data=dat); summary(M_Temp_FemDen) #Model w both Temp FemDens
SSy=with(dat,sum((SingRate-mean(SingRate))**2))
SSE=sum(M_Temp_FemDen$residuals**2)
k=length(M_Temp_FemDen$coefficients);n=nrow(dat)
Fv=((SSy-SSE)/(k-1)) / (SSE/(n-k));Fv #F-value
1-pf(Fv,k-1,n-k) #P-value for this F value

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

ggplot(d,aes(x=x,y=y,color=taxa))+geom_point(size=2)+theme_few()+
  xlab("Body mass(kg)")+ylab("Brain Mass(kg)")+ 
  geom_smooth(method="lm")+stat_cor(label.y = c(1.7,1.9))+ 
  stat_regline_equation(label.y = c(1.8,2))+ theme(legend.position="top")

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

#R's built in diagnostics plots
set.seed(1);df=data.frame(x=rnorm(100),y=rnorm(100))
f1=lm(y~x,data=df)
par(mfrow=c(2,2));plot(f1)  

#Anscombe's quartet
m=read.csv("AnscomeQuartet.csv")
summary(lm(Y~X,data=m[m$Group=="A",]))
ggplot(m,aes(x=X,y=Y,group=Group))+theme_few()+
  geom_point()+
  facet_wrap(~Group,scales="free")+
  geom_smooth(method="lm",linewidth=.5)

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

