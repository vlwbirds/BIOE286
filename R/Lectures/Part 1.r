#Part 1 Part 1 Introduction Frequentist Theory Regression Comparing Groups
#Code for Intro Mosquitoes in Cameroon example is in later lecture (Part 6 GLMERs)

#Do research visits disturb bats example:
lapply(c("tidyverse","ggthemes"),require,character.only=T) #load packages
setwd("c:/marm/teaching/286/lecture R code")
m=read.csv("Bat Disturbance Data.csv")
head(m[m$Site.ID=="1"&m$Visits_t==1,],4)
head(m[m$Site.ID=="Bear Creek Cave"&m$Visits_t==2,],4)
x1=m[250:386,c("Visits_t","MYLU_lambda","MYLU_LLambda")] #for showing data on slide
x1=x1[!is.na(x1$MYLU_LLambda)&is.finite(x1$MYLU_LLambda)&x1$Visits_t<3,];x1
#Use subset of data for this example
x1=m[m$MYLU_LLambda>-1.5&m$Visits_t<3,c("Visits_t","MYLU_lambda","MYLU_LLambda")]
x1=x1[!is.na(x1$MYLU_LLambda)&is.finite(x1$MYLU_LLambda),]
nrow(x1)

x1m=aggregate(MYLU_LLambda~Visits_t,data=x1,FUN=mean);x1m
x1mT=x1 %>% group_by(Visits_t) %>% summarize(mMYLU_LLambda=mean(MYLU_LLambda));x1mT
meandiff=x1m[2,2]-x1m[1,2];meandiff
range(x1$MYLU_LLambda)
x1m$SD=c(aggregate(MYLU_LLambda~Visits_t,data=x1,FUN=sd)[,2]);x1m
ggplot(x1,aes(y=MYLU_LLambda,x=factor(Visits_t) ))+theme_few()+
  labs(x="# of visits",y="Log10 population growth rate, lambda")+
  geom_jitter(width=0.2,shape=1,size=3)+
  geom_point(data=x1m,size=5)

#Normal distribution
curve(dnorm(x,0,1),xlim=c(-2,2),xlab="Response variable",ylab="Probability",xaxt="n");abline(v=0)
arrows(0,.23,1,.23)

#Simple null hypothesis
curve(dnorm(x,3,1),xlim=c(1,5),xlab="Response variable",ylab="Probability");abline(v=3)
arrows(3,.23,4,.23)
set.seed(1) #make example repeatable
nh=rnorm(n=20,mean=3,sd=1);nh # 20 random numbers, mean 10
mean(nh[1:10]) #first 10 numbers
mean(nh[11:20]) #second 10 numbers
diff=mean(nh[1:10])-mean(nh[11:20]);diff

curve(dnorm(x,3,1),xlim=c(0,6),ylim=c(0,.5),xlab="Response variable",ylab="Probability");abline(v=3)
lines(density(nh[1:10]),lty=1,main="",col="blue");abline(v=mean(nh[1:10]),col="blue");
text(1.5,.3,"1st 10\n numbers",col="blue")
lines(density(nh[11:20]),lty=1,main="",col="red");abline(v=mean(nh[11:20]),col="red");
text(5,.3,"2nd 10\n numbers",col="red")

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
hist(x1$MYLU_LLambda,lty=1,main="",breaks=20,col=0,xlab="Log10 Pop growth rates")
hist(x1$MYLU_LLambda[x1$Visits_t==1],col=c1,lty=2,add=T,breaks=20)
hist(x1$MYLU_LLambda[x1$Visits_t==2],col=c2,lty=2,add=T,breaks=20)
abline(v=mean(x1$MYLU_LLambda))
abline(v=mean(x1$MYLU_LLambda[x1$Visits_t==1]),col="lightblue")
abline(v=mean(x1$MYLU_LLambda[x1$Visits_t==2]),col="pink")

plot(density(x1$MYLU_LLambda),col="black",lty=1,main="",ylim=c(0,1.5),lwd=2,
     xlab="Log10 Pop growth rates")
lines(density(x1$MYLU_LLambda[x1$Visits_t==1]),col="blue",lty=2)
lines(density(x1$MYLU_LLambda[x1$Visits_t==2]),col="pink",lty=2)
abline(v=mean(x1$MYLU_LLambda))
abline(v=mean(x1$MYLU_LLambda[x1$Visits_t==1]),col="lightblue")
abline(v=mean(x1$MYLU_LLambda[x1$Visits_t==2]),col="pink")
text(-.7,.9,"1 visit",col="blue");text(-.7,1.4,"2 visits",col="red");
text(-.7,1.2,"A single distribution",col="black")
text(-.7,1.1,"for all the data",col="black")

m_mll=mean(x1$MYLU_LLambda);sd_mll=sd(x1$MYLU_LLambda)
curve(dnorm(x,m_mll,sd_mll),xlim=c(-2,2),xlab="Log10 lambda",ylab="Probability");abline(v=m_mll)
arrows(m_mll,.5,m_mll+sd_mll,.5)
text(.85,.5,paste("SD=",round(sd_mll,3)))
text(-.75,.0,paste("mean=",round(m_mll,3)))
m_mll
meandiff
var(x1$MYLU_LLambda)
sd(x1$MYLU_LLambda)
nrow(x1)
aggregate(MYLU_LLambda~Visits_t,data=x1,FUN=length)

diff=NA #initialize variable
nsim=10000 # # of simulations
set.seed(1) #make random draws repeatable
for (i in 1:nsim) {
  draws=rnorm(89,mean=m_mll,sd=0.428) #draw from normal dist
  mean1_51=mean(draws[1:51]);mean52_89=mean(draws[52:89]) #calculate means of 1st 51 #s & other 38
  diff[i]=mean1_51-mean52_89 #store diff b/w means
}
draws[1:51]
draws[52:89]
mean(draws[1:51])
mean(draws[52:89])
diff[i]
diff[1:100]

par(mfrow=c(1,1),mgp=c(1.5,.5,0),cex=1.,mar=c(3, 3, 0.5, 0.5))#b,l,t,r
hist(diff,xlab="Difference between simulated log pop growth rates",main="",xlim=c(-.4,.4));
abline(v=meandiff,col="red")
text(-.24,1800,"Difference observed",col="red")
text(-.24,1600,round(meandiff,3),col="red")
hist(abs(diff),xlab="Difference between simulated log pop growth rates",main="");
abline(v=abs(meandiff),col="red")
text(.16,1600,"abs(Difference observed)",col="red")
text(.11,1500,abs(round(meandiff,3)),col="red")
sum((diff)>0.073)/nsim #frac of sims w/ bigger diff
sum(abs(diff)>0.073)/nsim #frac of sims w/ bigger abs diff

meandiff=x1m[2,2]-x1m[1,2];meandiff
f1=lm(MYLU_LLambda~as.factor(Visits_t),data=x1) #fit model to get SE of diff
meandiff+1.96*c(0.092,-0.092) #approx 95% CI for diff

#Correlation: Cricket example
m=read.csv("RegressionDataCrickets.csv");m
ggplot(m,aes(y=ChirpsPerMin,x=TempF))+theme_few()+
  geom_point(size=2)+labs(x="Temperature, degrees F",y="Chirps per minute")
cov(m$TempF,m$ChirpsPerMin)/sqrt(var(m$TempF)*var(m$ChirpsPerMin))
cor.test(m$TempF,m$ChirpsPerMin)$estimate

curve(dnorm(x,mean(m$ChirpsPerMin),sd(m$ChirpsPerMin)),xlab="Chirps per minute",ylab="Probability",
      xlim=c(mean(m$ChirpsPerMin)-4*sd(m$ChirpsPerMin),mean(m$ChirpsPerMin)+4*sd(m$ChirpsPerMin)),)
abline(v=mean(m$ChirpsPerMin))
text(19,.25,paste("Mean=",round(mean(m$ChirpsPerMin),3)) )
text(19,.22,paste("SD=",round(sd(m$ChirpsPerMin),3)) )

curve(dnorm(x,mean(m$TempF),sd(m$TempF)),xlab="Temperature, degrees F",ylab="Probability",
      xlim=c(mean(m$TempF)-4*sd(m$TempF),mean(m$TempF)+4*sd(m$TempF)),);
abline(v=mean(m$TempF));text(20,.2,paste("SD=",round(sd(m$TempF),3)) )
text(100,.035,paste("Mean=",round(mean(m$TempF),3)) )
text(100,.03,paste("SD=",round(sd(m$TempF),3)) )

nsim=10000 # of draws/simulations
rxyv=NA #initialize correlation value variable
set.seed(1) #make random draws repeatable
for (i in 1:nsim) {
  chirp=rnorm(15,mean=mean(m$ChirpsPerMin),sd=sd(m$ChirpsPerMin))
  tempF=rnorm(15,mean=mean(m$TempF),sd=sd(m$TempF))
  rxyv[i]=cor.test(tempF,chirp)$estimate #calculate correlation coef
}
cbind(chirp,tempF)
plot(chirp~tempF,pch=19)
rxyv[9990:10000]
cor.test(tempF,chirp)$estimate

hist(rxyv,main="",xlim=c(-1,1));abline(v=0.695,col="red",lwd=2);
text(0.35,1350,"Observed value\n 0.695",col="red")
hist(abs(rxyv),main="");abline(v=0.695,col="red",lwd=2)
text(0.55,1250,"Observed value\n 0.695",col="red")

sum(rxyv>0.695)/nsim #frac of draws w/ more positive correlation
sum(abs(rxyv)>0.695)/nsim #frac of draws w/ bigger correlation

ggplot(m,aes(x=TempF,y=ChirpsPerMin))+theme_few()+
  geom_point(size=3)+stat_smooth(method="lm")+
  xlab("Temperature(F)")+ylab("Chirps per minute")+
  ggpubr::stat_regline_equation()
