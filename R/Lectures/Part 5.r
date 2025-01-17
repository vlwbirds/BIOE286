#Part 5 Generalized linear models
lapply(c("tidyverse","ggthemes","fitdistrplus","lme4","car","emmeans","brglm2"),require,character.only=T) #load packages
setwd("c:/marm/teaching/286/lecture R code/")

#Snake length
curve(dnorm(x,1.2,.7),-2,4,xlab="length",ylab="Probability"); abline(v=0)

#Eseal reproduction
df=data.frame(age=rep(3:5,each=30),Reproduce=rbinom(90,1,rep(c(.2,.5,.9),each=30)))
plot(Reproduce~jitter(age),data=df,xlim=c(1,8),ylim=c(-.2,1.2));abline(lm(Reproduce~age,df))

#Eseal arrival time
m=read.csv("BirthTiming_ElephantSeal.csv",header=T)
m$diff=m$firstwith-m$aday
f_norm=fitdist(m$diff,dnorm)
f_gamma=fitdist(m$diff,dgamma)
hist(m$diff,xlim=c(-2,12),xlab="Time between arrival and birth, days",breaks=c(-2:12), right=F, main="")
points(x=c(-2:12+.5),y=nrow(m)*dnorm(c(-2:12+.5),mean=f_norm$estimate["mean"],sd=f_norm$estimate["sd"]),col="blue",pch=19)
points(x=c(-2:12+.5),y=nrow(m)*dgamma(c(-2:12+.5),shape=f_gamma$estimate["shape"],rate=f_gamma$estimate["rate"]),col="red",pch=19)
curve(nrow(m)*dnorm(x,mean=f_norm$estimate["mean"],sd=f_norm$estimate["sd"]),
      from=-2,to=12,add=T,col="blue")
curve(nrow(m)*dgamma(x,shape=f_gamma$estimate["shape"],rate=f_gamma$estimate["rate"]),
      from=-2,to=12,add=T,col="red")
legend(9,28,legend=c("Normal","gamma"),col=c("blue","red"),lwd=1,lty=1,bty="n",pch=19)

#Normal/Gaussian distribution
curve(dnorm(x,0.1,1),from=-10,to = 10,xname="x",col=1,xlim=c(-10,20),ylab="Probability",main="Normal")
curve(dnorm(x,0.1,2.25),from= -10,to = 20,xname="x",col=2,add=T)
curve(dnorm(x,5,1),from= -10,to = 20,xname="x",col=3,add=T)
curve(dnorm(x,5,2.25),from= -10,to = 20,xname="x",col=4,add=T)
legend(10,.35,col=1:4,legend = c("0.1,1","0.1,2.25","5,1","5,2.25"),lty=1,bty="n",title="Mean,SD")

#Gamma distribution
shape=function(mu,sig) mu^2/sig^2
scale = function(mu,sig) sig^2/mu
curve(dgamma(x,shape=shape(0.1,1),scale=scale(0.1,1)),from= 0,to = 20,xname="x",col=1,xlim=c(0,20),
      ylim=c(0,.4),ylab="Probability",main="Gamma")
curve(dgamma(x,shape=shape(0.1,2.25),scale=scale(0.1,2.25)),from= 0,to = 20,xname="x",col=2,add=T)
curve(dgamma(x,shape=shape(5,1)  ,scale=scale(5,1)),from= 0,to = 20,xname="x",col=3,add=T)
curve(dgamma(x,shape=shape(5,2.25)  ,scale=scale(5,2.25)),from= 0,to = 20,xname="x",col=4,add=T)
legend(10,.35,col=1:4,legend = c("0.1,1","0.1,2.25","5,1","5,2.25"),lty=1,bty="n",title="Mean,SD")
abline(v=0)

#Binomial
curve(dbinom(x,size=6,prob=0.4),from= 0,to = 20,n=21,xname="x",col=1,xlim=c(0,20),ylim=c(0,.4),
      type="b",ylab="Probability",main="Binomial")
curve(dbinom(x,size=155,prob=0.032),from= 0,to = 20,n=21,xname="x",col=2,add=T,type="b")
legend(10,.15,col=1:3,legend =c("2.4,1.09","4.96,2.2") ,lty=1,bty="n",title="Mean, sd")

#Bernoulli (Binomial)
par(mar=c(3.5,3.5,0.1,0.1),mgp=c(2.2,1,0))
curve(plogis(x),from=-7, to =7,ylab="Probability of success",xlab="x")
curve(plogis(x,location = 2),from=-7, to =7,lty=2,add=T)
curve(1-plogis(x),from=-7, to =7,add=T,lty=3)

#Bernoulli (Binomial) sample sizes, uncertainty
p=seq(0,1,.01);n=10
f_se=sqrt(p*(1-p)/n)
par(mar=c(3.5,3.5,0.1,0.1),mgp=c(2,1,0))
plot(f_se~p,type="l",ylab="SE")
n=50;f_se=sqrt(p*(1-p)/n)
lines(f_se~p,type="l",ylab="SE",yaxt="n",lty=2)
text(x=.5,y=.15,"N=10");text(x=.5,y=.06,"N=50")

p=seq(0.01,.2,.01);n=10
f_cv=(sqrt(p*(1-p)/n))/p
par(mar=c(3.5,3.5,0.1,0.1),mgp=c(2,1,0))
plot(f_cv~p,type="l",ylab="CV",ylim=c(0,max(f_cv)))
n=50;f_cv=(sqrt(p*(1-p)/n))/p
lines(f_cv~p,type="l",ylab="SE",yaxt="n",lty=2)
n=100;f_cv=(sqrt(p*(1-p)/n))/p
lines(f_cv~p,type="l",ylab="SE",yaxt="n",lty=3)
text(x=.05,y=2,"N=10");text(x=.05,y=.9,"N=50");text(x=.05,y=.3,"N=100")

#Poisson
curve(dpois(x,lambda=2),from= 0,to = 20,n=21,xname="x",col=1,xlim=c(0,20),ylim=c(0,.4),
      type="b",ylab="Probability",main="Poisson",pch=19)
curve(dpois(x,lambda=5),from= 0,to = 20,n=21,xname="x",col=2,add=T,type="b",pch=19)
curve(dpois(x,lambda=10),from= 0,to = 20,n=21,xname="x",col=3,add=T,type="b",pch=19)
legend(10,.35,col=1:3,legend = c("2,1.4","5,2.4","10,3.2"),lty=1,bty="n",title="Mean, sd")

#Negative binomial
k=function(mu,sig) mu^2/(sig^2-mu)
curve(dnbinom(x,mu=5,size=k(5,2.25)),from= 0,to = 20,n=21,xname="x",col=1,type="b",ylab="Prob",main="Neg binomial")
curve(dnbinom(x,mu=5,size=k(5,5)),from= 0,to = 20,n=21,xname="x",col=2,type="b",add=T)
legend(10,.15,col=1:2,legend =c("5,2.5","5,5") ,lty=1,bty="n")

#Beta distribution
curve(dbeta(x,shape1=2,shape2=2),col=1,ylab="Probability",main="Beta",ylim=c(0,2.5),lwd=2)
curve(dbeta(x,shape1=1,shape2=1),col=2,add=T,lwd=2)
curve(dbeta(x,shape1=5,shape2=2),col=3,add=T,lwd=2)
curve(dbeta(x,shape1=2,shape2=3),col=4,add=T,lwd=2)
curve(dbeta(x,shape1=.5,shape2=.5),col=5,add=T,lwd=2)
curve(dbeta(x,shape1=3,shape2=1),col=6,add=T,lwd=2)

#Link shapes
par(mfrow=c(4,1),mar=c(2,2,0,0))
x=1:10;y=x;plot(y~x,type="l",xaxt="n",yaxt="n")
x=c(1:7,seq(7.1,10,.1));y=exp(x);plot(y~x,type="l",xaxt="n",yaxt="n")
x=-5:5;y=plogis(x);plot(y~x,type="l",xaxt="n",yaxt="n")
x=c(seq(1,3,.1),4:10);y=1/x;plot(y~x,type="l",xaxt="n",yaxt="n")
par(mfrow=c(1,1))

#Bat fungal loads
m=read.csv("BatFungalLoads.csv")
m$date_sampled=as.Date(m$date_sampled,format="%Y-%m-%d")
head(m)
ggplot(m,aes(x=date_sampled,y=pd,color=species))+theme_few()+
  geom_point(size=3)+theme(text=element_text(size=20))

mg=m %>% group_by(date_sampled,species) %>%  #make grouped data frame
  summarize(Prevalence=sum(pd)/n(),N=n(),SE=(Prevalence*(1-Prevalence)/N)^.5)

ggplot(mg,aes(x=date_sampled,y=Prevalence,color=species,weight=N))+theme_few()+
  geom_point(size=3,position=position_dodge(width=2))+
  geom_errorbar(aes(x=date_sampled,ymin=Prevalence-SE,ymax=Prevalence+SE),width=0.1,
                position = position_dodge(width=2))+
  scale_x_date(date_breaks="1 month",date_labels = "%b")+
  theme(text=element_text(size=20))

f1a=glm(pd~date_sampled+species,family=binomial,data=m);summary(f1a)
#f1a2=glm(Prevalence~date_sampled+species,family=binomial,data=mg,weights=N);summary(f1a2)

dts=seq.Date(from=as.Date("2013-11-21"), to=as.Date("2014-04-15"), by="day")
nd1=data.frame(date_sampled=rep(dts,4),species=rep(c("epfu","mylu","myso","pesu"),each=length(dts)))
nd1$pred=predict(f1a,newdata=nd1,type="response")
ggplot(mg,aes(x=date_sampled,y=Prevalence,color=species))+theme_few()+
  geom_point(size=3,position=position_dodge(width=2))+
  geom_errorbar(aes(x=date_sampled,ymin=Prevalence-SE,ymax=Prevalence+SE),width=0.1,
                position = position_dodge(width=2))+
  scale_x_date(date_breaks="1 month",date_labels = "%b")+
  geom_line(data=nd1,aes(x=date_sampled,y=pred,color=species))+
  theme(text=element_text(size=20))

m$Jdate0=m$Jdate-324
f1c=glm(pd~Jdate0+species,family=binomial,data=m);summary(f1c)
1-pchisq(deviance(f1c),f1c$df.residual)

plogis(-0.297072)
plogis(-0.297072+0.894844)

#Understanding the model
dts=seq.Date(from=as.Date("2013-11-21"), to=as.Date("2014-04-15"), by="day")
nd1=data.frame(date_sampled=dts,species= rep(c("epfu","mylu","myso","pesu"),each=length(dts)))
nd1$pred=predict(f1a,newdata=nd1,type="response")

nd1$pred=predict(f1a,newdata=nd1) #default or:
nd1$pred=predict(f1a,newdata=nd1,type="link") 

ggplot(mg,aes(x=date_sampled,y=Prevalence,color=species))+theme_few()+
  geom_point(size=3,position=position_dodge(width=2))+
  geom_errorbar(aes(x=date_sampled,ymin=Prevalence-SE,ymax=Prevalence+SE),width=0.1,
                position = position_dodge(width=2))+
  scale_x_date(date_breaks="1 month",date_labels = "%b")+
  geom_line(data=nd1,aes(x=date_sampled,y=pred,color=species))+
  theme(text=element_text(size=20))

#Stats for “species” predictor:
car::Anova(f1a, type="III",test.statistic="Wald")
#Pairwise comparisons:
emmeans::emmeans(f1a, pairwise ~ species,type="response")

mg$Prevalence2=mg$Prevalence;mg$Prevalence2[mg$Prevalence2==1]=0.999
ggplot(mg,aes(x=date_sampled,y=Prevalence2,color=species))+theme_few()+
  geom_point(size=3,position=position_dodge(width=2))+
  geom_errorbar(aes(x=date_sampled,ymin=Prevalence2-SE,ymax=Prevalence2+SE),width=0.1,
                position = position_dodge(width=2))+
  scale_x_date(date_breaks="1 month",date_labels = "%b")+
  geom_line(data=nd1,aes(x=date_sampled,y=pred,color=species))+
  theme(text=element_text(size=20))+
  scale_y_continuous(trans="logit", limits=c(0.01,0.999), 
                     breaks=c(0.01,0.1,0.5,0.9,0.99,0.999))+
  xlab("")+ylab("Prevalence (logit scale)")

#Challenges of 0/1 data: Complete separation, brglm2
f3a=glm(Prevalence~species,data= mg[mg$date_sampled=="2013-11-21",], 
        family=binomial,weights=N);summary(f3a)
f3b=glm(Prevalence~species,data=mg[mg$date_sampled=="2013-11-21",],
        family=binomial,weights=N,method="brglmFit");summary(f3b)

#Complete separation w/ interaction model
f2a=glm(pd~date_sampled*species,family=binomial,data=m);summary(f2a)
f2b=glm(pd~date_sampled*species,family=binomial,data=m,method="brglmFit");summary(f2b)

nd1$pred=predict(f2b,newdata=nd1,type="response") #make lines for plot
ggplot(mg,aes(x=date_sampled,y=Prevalence,color=species))+theme_few()+
  geom_point(size=3,position=position_dodge(width=2))+
  geom_errorbar(aes(x=date_sampled,ymin=Prevalence-SE,ymax=Prevalence+SE),width=0.1,
                position = position_dodge(width=2))+
  scale_x_date(date_breaks="1 month",date_labels = "%b")+
  geom_line(data=nd1,aes(x=date_sampled,y=pred,color=species))+
  theme(axis.title=element_text(size=20),axis.text=element_text(size=20))+
  theme(legend.title=element_text(size=20), legend.text=element_text(size=20))

#GLM #2 Count data: Mosquitoes 
mL=read.csv("Mosquitoes_Yaounde_grouped.csv") 
head(mL)
#Plot the data (on a log scale)
ggplot(mL,aes(x=UI_500M,y=mosq,color=species,shape=species))+
  geom_point(size=3)+theme_few()+scale_y_continuous(trans='log10')+
  labs(x = "Urbanization w/in 500m",y="Mosquito abundance, log scale")+
  scale_color_manual(values=c("red","blue"))

#Analyze the data
g0=glm(mosq~UI_500M+species,family=poisson,data=mL);summary(g0)
#Is Residual deviance = Residual degrees of freedom?
AER::dispersiontest(g0) #over dispersion test

#Poisson vs quasipoisson
g0=glm(mosq~UI_500M+species,family=poisson,data=mL);summary(g0)
g1b=glm(mosq~UI_500M+species,data=mL,family=quasipoisson);summary(g1b)

#Negative binomial
#Use the fitted model to make graphical version of model across a range of urbanization
nd2=data.frame(UI_500M=rep(0:100,2),species=rep(c("A. albopictus","A. aegypti"),each=101)) #new data frame for predictions
nd2$mosq.Poi=predict(g0,newdata=nd2,type="response") #predicted model for Poisson (mean is same as QP)
nd2$SE.Poi=predict(g0,newdata=nd2,type="response",se.fit=T)$se.fit 
nd2$SE.QPoi=predict(g1b,newdata=nd2,type="response",se.fit=T)$se.fit 

#Poisson and QP
ggplot(mL,aes(x=UI_500M,y=mosq,group=species,color=species))+theme_few()+
  geom_point(size=3)+scale_y_continuous(trans='log10')+
  geom_line(data=nd2,aes(x=UI_500M,y=mosq.Poi,color=species))+scale_color_manual(values=c("red","blue"))+
  geom_ribbon(data=nd2,aes(x=UI_500M,ymin=mosq.Poi-SE.Poi,ymax=mosq.Poi+SE.Poi,y=mosq.Poi,
                           color=species,fill=species),alpha=0.5,linetype=0)+
  labs(x = "Urbanization w/in 500m",y="Mosquito abundance +/- 1 SE, log scale")+
  geom_ribbon(data=nd2,aes(x=UI_500M,ymin=mosq.Poi-SE.QPoi,ymax=mosq.Poi+SE.QPoi,y=mosq.Poi,
                           color=species,fill=species),alpha=0.3,linetype=0)

#Quasi-poisson vs Negative binomial
g1b=glm(mosq~UI_500M+species,data=mL,family=quasipoisson);summary(g1b)
g1a=glm.nb(mosq~UI_500M+species,data=mL);summary(g1a)
nd2$mosq.nb=predict(g1a,newdata=nd2,type="response")
nd2$SE.nb=predict(g1a,newdata=nd2,type="response",se.fit=T)$se.fit

#NB and QP
ggplot(mL,aes(x=UI_500M,y=mosq,group=species,color=species))+theme_few()+
  geom_ribbon(data=nd2,aes(x=UI_500M,ymin=mosq.nb-SE.nb,ymax=mosq.nb+SE.nb,y=mosq.nb,
                           color=species,fill=species),alpha=0.1,linetype=0)+
  geom_point(size=3)+scale_y_continuous(trans='log10')+
  geom_line(data=nd2,aes(x=UI_500M,y=mosq.nb,color=species))+scale_color_manual(values=c("red","blue"))+
  labs(x = "Urbanization w/in 500m",y="Mosquito abundance +/- 1 SE, log scale")+
  geom_line(data=nd2,aes(x=UI_500M,y=mosq.Poi,color=species),linetype="dashed")

#Show part of fitted model data frame - 2.3% increase/1 unit UI increase
nd2[nd2$UI_500M%in%c(0,1,100),]

#Trying to use GGplot stat_smooth for plot:
ggplot(mL,aes(x=UI_500M,y=mosq,fill=species,color=species))+theme_few()+
  geom_point(size=3)+scale_color_manual(values=c("red","blue"))+
  labs(x = "Urbanization w/in 500m",y="Mosquito abundance +/- 1 SE, log scale")+
  stat_smooth(method="glm.nb")

#Dispersion test on mosquito data - can't use except for Poisson GLMs
#AER::dispersiontest(f1c)

#Species effect on bat data model from above
m=read.csv("BatFungalLoads.csv")
m$date_sampled=as.Date(m$date_sampled,format="%Y-%m-%d")
f1a=glm(pd~date_sampled+species,family=binomial,data=m);summary(f1a)
car::Anova(f1a,type="III",test.statistic="Wald")
emmeans(f1a, pairwise ~ species)


#Pseudo-R2:
#https://rdrr.io/cran/DescTools/man/PseudoR2.html 
#https://en.wikipedia.org/wiki/Pseudo-R-squared
DescTools::PseudoR2(f1a, which = c("McFadden", "Nagel","McKelveyZavoina"))

