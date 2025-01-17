lapply(c("tidyverse","lme4","ggthemes","sjPlot","MASS"),require,character.only=T) #load packages
setwd("c:/marm/teaching/286/Lecture R code/") #set working directory
mL=read.csv("Mosquitoes_Yaounde.csv") #read in raw data file
head(mL)
table(mL$Neighborhood)/2 #how many data points/neighborhood (2 mosq spp/point)

#Plot the data
mLg=aggregate(mosq~species+Neighborhood+UI_500M,data=mL,FUN=mean) #mean per neighborhood
ggplot(mL,aes(x=UI_500M,y=mosq,color=Neighborhood,shape=species))+theme_few()+
  geom_point(size=3)+scale_y_continuous(trans='log10')+
#  geom_point(data=mLg,aes(x=UI_500M,y=mosq,color="black"),color="black",size=3)+ #add means
  labs(x = "Urbanization w/in 500m",y="Mosquito abundance, log scale")

#Analyze the data - are residuals correlated by Neighborhood?
g1a=glm.nb(mosq~UI_500M*species,data=mL);summary(g1a)
mL$resid1=resid(g1a,type="response")
ggplot(mL,aes(x=UI_500M,y=resid1,color=Neighborhood,shape=species))+geom_point(size=3)+
  theme_few()+labs(x = "Urbanization w/in 500m",y="glm resids")+
  geom_abline(intercept=0,slope=0)

#GLMER model but "nearly unidentifiable" warning)
g1=glmer.nb(mosq~UI_500M*species+(1|Neighborhood),data=mL);summary(g1)
#need to rescale UI to make model more identifiable
mL$UI_500MR=mL$UI_500M/100 #Can rescale UI to help model fitting and avoid warning
g1aR=glm.nb(mosq~UI_500MR*species,data=mL);summary(g1aR)
g1R=glmer.nb(mosq~UI_500MR*species+(1|Neighborhood),data=mL);summary(g1R)

options(scipen = 999) #makes y-axis #s prettier; set to 0 to restore
sjPlot::plot_model(g1, type = "pred", terms = c("UI_500M", "species"),title="",
                   pred.type = "re",show.data=T,colors=c("red","blue"))+
  scale_y_continuous(trans='log10')+
  theme_few()+
  labs(x = "Urbanization w/in 500m",y="Mosquito abundance, log scale")
options(scipen = 0)


#Use the fitted model to make graphical version of model across a range of urbanization
nd2=data.frame(UI_500MR=rep(c(0:100)/100,2),species=rep(c("A. albopictus","A. aegypti"),each=101))
nd2$mosq=predict(g1R,newdata=nd2,type="response",re.form=NA) #re.form=NA excludes RE so don't need to specify that in new data

#Plot the data and the fitted model together on same graph
ggplot(mL,aes(x=UI_500MR,y=mosq,shape=species))+theme_few()+
  geom_point(aes(color=Neighborhood),size=3)+
  scale_y_continuous(trans='log10')+#scale_color_manual(values=c("red","blue"))+
  geom_line(data=nd2,aes(x=UI_500MR,y=mosq,linetype=species))+
  labs(x = "Urbanization w/in 500m",y="Mosquito abundance +/- 1 SE, log scale")


nd2$mosqlink=predict(g1R,newdata=nd2,re.form=NA,type="link") #predict mean values on log scale
pf1 = function(fit) {   predict(fit,nd2,re.form=NA) } #function for bootstrapping
bb=bootMer(g1R,nsim=50,FUN=pf1,seed=69) #bootstrap to estimate uncertainty in predictions
nd2$SE=apply(bb$t, 2, sd) #summarize bootstrap samples
nd2$pSE=exp(nd2$mosqlink+nd2$SE) #predicted mean + 1 SE
nd2$mSE=exp(nd2$mosqlink-nd2$SE) # predicted mean - 1 SE

#Plot the data and the fitted model together on same graph - easier to compare data & model!
ggplot(mL,aes(x=UI_500MR,y=mosq,color=species,shape=species))+theme_few()+
  geom_point(size=3)+
  geom_ribbon(data=nd2,aes(x=UI_500MR,ymin=mSE,ymax=pSE,color=species,fill=species),
              alpha=0.1,linetype=0)+
  scale_y_continuous(trans='log10')+scale_color_manual(values=c("red","blue"))+
  geom_line(data=nd2,aes(x=UI_500MR,y=mosq,color=species))+
  labs(x = "Urbanization w/in 500m",y="Mosquito abundance +/- 1 SE, log scale")+
  theme(legend.position="top")

ggplot(mL,aes(x=UI_500MR,y=mosq,shape=species))+theme_few()+
  geom_point(aes(color=Neighborhood),size=3)+
  geom_ribbon(data=nd2,aes(x=UI_500MR,ymin=mSE,ymax=pSE),
              alpha=0.1,linetype=0)+
  scale_y_continuous(trans='log10')+
  geom_line(data=nd2,aes(x=UI_500MR,y=mosq,linetype=species))+
  labs(x = "Urbanization w/in 500m",y="Mosquito abundance +/- 1 SE, log scale")

#Fixed vs random effects; can't estimate UI_500M w/ fixed Neighborhood effect
g1d =glm.nb(mosq~Neighborhood+UI_500M+species,data=mL);summary(g1d)
#g1di=glm.nb(mosq~Neighborhood+UI_500M*species,data=mL);summary(g1di) #interaction
#But you can w/ random effect
g1ni=glmer.nb(mosq~UI_500M+species+(1|Neighborhood),data=mL);summary(g1ni)
g1=glmer.nb(mosq~UI_500M*species+(1|Neighborhood),data=mL);summary(g1)