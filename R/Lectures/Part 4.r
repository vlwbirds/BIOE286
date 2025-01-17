#Part 4.r
lapply(c("tidyverse","ggthemes","ggpubr","emmeans","car","GGally"),require,character.only=T) #load packages
setwd("c:/marm/teaching/286/lecture R code/")

#Cricket Temp & Female density w/ no correlation among predictors
#Create data
set.seed(1) #make example repeatable
m=data.frame(Temp=rnorm(n=50,mean=65,sd=1), #create Temp data
             FemDens=rnorm(50,30,1)) #create Female Density data
m$SingRate=1*m$Temp + 2*m$FemDens+ rnorm(50,2,2.5) #Singing Rate is influenced by both predictors, with slopes 1 and 2

#Plot data: Singing rate vs Temp
ggplot(m,aes(y=SingRate,x=Temp))+theme_few()+
  geom_point(aes(col=FemDens),size=3)+
  theme(text=element_text(size=20))+
  stat_smooth(method="lm")+labs(x="Temperature",y="Singing Rate")+
  scale_color_gradientn(colors = rainbow(4))

#Analyze data: Singing rate vs Temp
M_Temp=lm(SingRate~Temp,data=m);summary(M_Temp) #Model with just Temp

#Plot data: Singing rate vs Female density
ggplot(m,aes(y=SingRate,x=FemDens))+theme_few()+
  geom_point(aes(col=Temp),size=3)+
  theme(text=element_text(size=20))+
  stat_smooth(method="lm")+labs(x="Female Density",y="Singing Rate")+
  scale_color_gradientn(colors = rainbow(4))

#Analyze data: Singing rate vs Female Density
m_FemDens=lm(SingRate~FemDens,data=m);summary(m_FemDens) #model w/ just female density

#Analyze data: Singing rate vs Temp and Female Density
m_Temp_FemDens=lm(SingRate~Temp+FemDens,data=m); summary(m_Temp_FemDens)

#Plot data and model: Singing rate vs Female density
FemDensv=seq(min(m$FemDens),max(m$FemDens),by=0.1);Temps=62:67
nd1=data.frame(Temp=rep(Temps,length(FemDensv)),FemDens=rep(FemDensv,length(Temps)))
nd1$SingRate=predict(m_Temp_FemDens,newdata=nd1)
ggplot(m,aes(y=SingRate,x=FemDens))+theme_few()+
  geom_point(aes(col=Temp),size=3)+
  theme(text=element_text(size=20))+
  labs(x="Female Density",y="Singing Rate")+
  geom_line(data=nd1,mapping=aes(x=FemDens,y=SingRate,col=Temp,group=Temp))+
  scale_color_gradientn(colors = rainbow(4))

#Plot correlation among predictors
ggplot(m,aes(y=Temp,x=FemDens))+geom_point(size=3)+theme_few()+
  theme(axis.title=element_text(size=20),axis.text=element_text(size=20))+
  labs(x="Female Density",y="Temperature")+ 
  ggpubr::stat_cor(label.x = 31, label.y = 63,size=6)

#Elephant seal Sex-specific growth
m=read.csv("NSFRoD2022_Deployment_Summary_RSB.csv")
head(m[,c("Seal.ID","Sex","Age","DeploymentMass")])

#Simple plot of data
ggplot(m, aes(y=DeploymentMass, x=Age, color=Sex))+  theme_few()+ 
  ylab("Mass, kg")+ geom_jitter(height=0, width=0.25,size=3)+ 
  theme(text=element_text(size=25))

#Additive model of Sex, Age
f1=lm(DeploymentMass~Sex+Age,data=m); summary(f1)
#Interactive model of Sex, Age
f2=lm(DeploymentMass~Sex*Age, data=m); summary(f2)

#Plot of data + interactive model
ggplot(m, aes(y=DeploymentMass,x=Age,color=Sex))+ geom_jitter(height=0,width=0.25,size=3) + theme_few()+
  ylab("Mass, kg")+stat_smooth(method="lm")

#Show individual sex-specific slopes rather than comparison of slopes
emmeans::lstrends(f2,"Sex",var="Age",infer=T)
f2b=lm(DeploymentMass~Sex+Sex:Age, data=m); summary(f2b)

#Example: Cricket Temperature and Humidity w/ correlated predictors
lapply(c("tidyverse", "ggthemes","car"), require, character.only=T)#packages
set.seed(2) #make example repeatable
m2=mvtnorm::rmvnorm(n=50, mean=c(65,65), sigma=matrix(c(5,-2,-2,5), ncol=2))
colnames(m2)=c("Temp","Humid")
m2=as.data.frame(m2)
m2=m2 %>% mutate(SingRate=.25*Temp+.25*Humid+rnorm(50,2,1.5))

#Plot the two univariate relationships
ggplot(m2,aes(y=SingRate,x=Temp,col=Humid))+geom_point(size=3)+theme_few()+
  theme(text=element_text(size=20))+
  scale_color_gradientn(colors = rainbow(4))
ggplot(m2,aes(y=SingRate,x=Humid,col=Temp))+geom_point(size=3)+theme_few()+
  theme(text=element_text(size=20))+
  scale_color_gradientn(colors = rainbow(4))

#Analyze the two univariate relationships
f1=lm(SingRate~Temp,data=m2);summary(f1)
f2=lm(SingRate~Humid,data=m2);summary(f2)

#Analyze both variables together
f3=lm(SingRate~Temp+Humid,data=m2); summary(f3)

#Plot the correlation among predictors
ggplot(m2,aes(y=Temp,x=Humid))+geom_point(size=3)+theme_few()+
  theme(axis.title=element_text(size=20),axis.text=element_text(size=20))+
  ggpubr::stat_cor(label.x=58,label.y=60,size=6)

#Slope for temp, humidity in multiple regression
#Model with both (repeated from above)
f3=lm(SingRate~Temp+Humid,data=m2); summary(f3)

#Slope for Temp:
r_SR_Humid=resid(lm(SingRate~Humid,data=m2))
r_Temp_Humid=resid(lm(Temp~Humid,data=m2))
summary(lm(r_SR_Humid~r_Temp_Humid))

#Slope for Humidity
r_SR_Temp=resid(lm(SingRate~Temp,data=m2))
r_Humid_Temp=resid(lm(Humid~Temp,data=m2))
summary(lm(r_SR_Temp~r_Humid_Temp))

#Backwards stepwise regression with positively correlated variables: leg length example
set.seed(2);RL=rnorm(20,mean=31:50,sd=1)
LL=rnorm(20,mean=31:50,sd=1)
H=.5*RL+.5*LL+rnorm(20,sd=3)
pairs(cbind(RL,LL,H))
df=data.frame(RL=RL,LL=LL,H=H)
GGally::ggpairs(df,columns=c("RL","LL","H"))

#Fit models w/ both legs or each one individually
summary(lm(H~RL+LL))
summary(lm(H~RL))
summary(lm(H~LL))

#Slopes for individual predictors in multiple regression using residuals
#Right leg slope
R_RL_LL=resid(lm(RL~LL)) #resids of RL on LL
R_H_LL = resid(lm(H~LL)) #resids of H on LL
summary(lm(R_H_LL~R_RL_LL))

#Left leg slope
R_LL_RL=resid(lm(LL~RL))
R_H_RL = resid(lm(H~RL))
summary(lm(R_H_RL~R_LL_RL))

#Inflation of SE of slopes: sqrt(1/(1-R2))
sqrt(1/(1-0.93)) #SE of slope inflation: 3.77
sqrt(14.24) #SE of slope inflation: 3.77
0.1494*3.77 #SE of slope with inflation 0.56

#Variance inflation factors
car::vif(lm(H~RL+LL))

#Backwards stepwise regression
summary(lm(H~RL+LL))
#Remove LL:
summary(lm(H~RL))

#Eseal myoglobin
lapply(c("tidyverse","ggthemes","fitdistrplus","lme4","car","emmeans","brglm2"),require,character.only=T) #load packages
setwd("c:/marm/teaching/286/lecture R code/")
m=read.csv("RoD 2022-2024 Juvenile NES Mb Values.csv")
m=m[complete.cases(m),]
ggplot(m,aes(x=BodyMass,y=MeanMb))+theme_few()+
  geom_point(size=3)+stat_smooth(method="lm")+
  theme(text=element_text(size=20))+
  labs(y="Myoglobin")

summary(lm(MeanMb~BodyMass,data=m))

ggplot(m,aes(x=BodyMass,y=MeanMb,color=factor(Age) ))+theme_few()+
  geom_point(aes(shape=Sex),size=3)+stat_smooth(method="lm")+
  labs(y="Myoglobin")

summary(lm(MeanMb~BodyMass+Age+Sex,data=m))
with(m,cor.test(BodyMass,Age))

#Forwards step-wise regression with negatively correlated predictors
set.seed(12) #Make repeatable data
Sun=rnorm(50);Rain=-0.3*Sun+rnorm(50) #Neg correlation
Plant_Biomass=.5*Sun+.5*Rain+rnorm(50,0,1.5)
df=data.frame(Sun=Sun,Rain=Rain,Plant_Biomass=Plant_Biomass)
GGally::ggpairs(data=df,columns=c("Sun","Rain","Plant_Biomass"))
summary(lm(Plant_Biomass~Rain));summary(lm(Plant_Biomass~Sun))

cor.test(Sun,Rain)
summary(lm(Plant_Biomass~Rain));summary(lm(Plant_Biomass~Sun))
summary(lm(Plant_Biomass~Sun+Rain))
car::vif(lm(Plant_Biomass~Sun+Rain))

#AIC model comparison
set.seed(2);Sun=rnorm(50);Rain=-Sun+rnorm(50);#negatively correlated with Sun
Tree_Biomass=0.5*Sun+0.5*Rain+rnorm(50,0,1.5)
df2=data.frame(Sun=Sun,Rain=Rain,Tree_Biomass=Tree_Biomass) #Needed for GGplot,GGally
#Alternatively, w/ piping & tidyverse
set.seed(2)
df2=data.frame(Sun=rnorm(50))
df2=df2 %>% mutate(Rain=-Sun+rnorm(50), Tree_Biomass=.5*Sun+.5*Rain+rnorm(50,0,1.5))
GGally::ggpairs(data=df2,columns=c("Sun","Rain","Tree_Biomass"))

#Fit models
f0=lm(Tree_Biomass~1,data=df2);summary(f0)
f1=lm(Tree_Biomass~Sun,data=df2);summary(f1)
f2=lm(Tree_Biomass~Rain,data=df2);summary(f2)
f3=lm(Tree_Biomass~Sun+Rain,data=df2);summary(f3)
#AIC comparison
AIC(f0,f1,f2,f3)
car::vif(f3)

#Multi-model averaging
#Model weights:
AICcmodavg::aictab(list("f0"=f0,"f1"=f1,"f2"=f2,"f3"=f3),second.ord=F)
#Multi-model parameter estimate and 95% CI
AICcmodavg::modavgShrink(list("f0"=f0,"f1"=f1,"f2"=f2,"f3"=f3), 
                         parm="Rain", second.ord=F)
#Multi-model predictions
AICcmodavg::modavgPred(list("f0"=f0,"f1"=f1,"f2"=f2,"f3"=f3), 
                       newdata=data.frame(Sun=c(0,1),Rain=c(3,5)))
nd2=data.frame(Sun=c(0,1),Rain=c(3,5))
np1=AICcmodavg::modavgPred(list("f0"=f0,"f1"=f1,"f2"=f2,"f3"=f3), newdata=nd2)
nd2=cbind(nd2,np1[c(2,3,5,6)]);nd2

#AIC vs P-values
set.seed(5);x1=rnorm(20);y=x1+2.8*rnorm(20)
f0=lm(y~1);f1=lm(y~x1);summary(f1)
AIC(f0,f1)

#No true zeros
set.seed(2);RL=rnorm(20,mean=31:50,sd=1)
LL=rnorm(20,mean=31:50,sd=1)
H=.5*RL+.5*LL+rnorm(20,sd=3)
#Fit models w/ both legs or each one individually
summary(lm(H~RL+LL))
summary(lm(H~RL))
summary(lm(H~LL))
ppcor::pcor(cbind(H,RL,LL))

#Significance of categorical predictor w/ >2 categories
m=read.csv("BatFungalLoads.csv") #read data file
head(m)
g3=lm(logLoad ~ temperature + species, data=m[m$date_sampled=="2014-03-24",]); summary(g3)
car::Anova(g3)
anova(g3)

#Supplemental Fungal growth on bats supplemental example
m=read.csv("BatFungalLoads.csv")
#For rest of code, see slides

