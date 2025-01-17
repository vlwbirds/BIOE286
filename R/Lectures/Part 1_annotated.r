#Part 1 Part 1 Introduction Frequentist Theory Regression Comparing Groups
#Code for Intro Mosquitoes in Cameroon example is in later lecture (Part 6 GLMERs)



# Tues. Jan. 7th, 2025 ----------------------------------------------------------

# Do research visits disturb bats example:

## SLIDE 36

# 1. Set up ------------------------------------------------------------

lapply(c("tidyverse","ggthemes","lme4"),require,character.only=T) #apply the "require" function to load packages
setwd("~/Documents/U.C. Santa Cruz/TAing/BIOE 286 - Experimental Design and Data Analysis/Winter 2025/Annotated lecture code") # Set working directory
m=read.csv("Bat Disturbance Data.csv") # Read in data!



# BAT EXAMPLE -------------------------------------------------------------



# 2. Look at the raw data -------------------------------------------------

## SLIDE 37
# Filter the data to look at headers of two sets of 4 example data rows
head(m[m$Site.ID=="1"&m$Visits_t==1,],4) # First has 1 visit per year (Visits_t==1)
head(m[m$Site.ID=="Bear Creek Cave"&m$Visits_t==2,],4) # Second has 2 visits per year

# Filter for rows 250-386 to look at
x1=m[250:386,c("Visits_t","MYLU_lambda","MYLU_LLambda")] #for showing data on slide
# Filter out weird data (e.g. NAs, infinite data points)
x1=x1[!is.na(x1$MYLU_LLambda)&is.finite(x1$MYLU_LLambda)&x1$Visits_t<3,];x1
#Use subset of data for this example
x1=m[m$MYLU_LLambda>-1.5&m$Visits_t<3,c("Visits_t","MYLU_lambda","MYLU_LLambda")]
x1=x1[!is.na(x1$MYLU_LLambda)&is.finite(x1$MYLU_LLambda),]
nrow(x1) # How many rows?

# Aggregate the data rows by the number of visits and calculate the length (FUN = length) of each set of data 
# (aka how many rows are there per each "visitation" category?)
aggregate(MYLU_LLambda~Visits_t,data=x1,FUN=length)

## SLIDE 43
# Do the above, but instead of length, calculate the mean (FUN = mean) per each number of visits
x1m=aggregate(MYLU_LLambda~Visits_t,data=x1,FUN=mean);x1m # This does it using the aggregate function
x1mT=x1 %>% group_by(Visits_t) %>% summarize(mMYLU_LLambda=mean(MYLU_LLambda));x1mT # This does the exact same thing using dplyr/tidyverse functions instead of aggregate

# Now calculate the difference between the means
meandiff=x1m[2,2]-x1m[1,2];meandiff
# Calculate the range of lambdas
range(x1$MYLU_LLambda)

# Now calculate the standard deviation within the aggregate function
x1m$SD=c(aggregate(MYLU_LLambda~Visits_t,data=x1,FUN=sd)[,2]);x1m


# 3. Graphing popn growth rates ---------------------------------------


# * 3.1 Plot the raw data as points ---------------------------------------

## SLIDE 45
# Plot the data in points! geom_jitter adds the points onto the graph with a bit of random jitter side to side
ggplot(x1,aes(y=MYLU_LLambda,x=factor(Visits_t) ))+theme_few()+
  labs(x="# of visits",y="Log10 population growth rate, lambda")+
  geom_jitter(width=0.2,shape=1,size=3)+
  geom_point(data=x1m,size=5) # This adds the means specifically


# Digression about sampling from a distribution ---------------------------

## SLIDE 50

# Generate and plot a normal distribution
# Mean of 0 and standard deviation of 1
curve(dnorm(x,0,1),xlim=c(-2,2),xlab="Response variable",ylab="Probability",xaxt="n");abline(v=0)
arrows(0,.23,1,.23) # Add a little arrow that shows distance from the mean

#Simple null hypothesis
curve(dnorm(x,3,1),xlim=c(1,5),xlab="Response variable",ylab="Probability");abline(v=3)
arrows(3,.23,4,.23)
set.seed(1) #make example repeatable
# Generate 20random numbers, mean of 3 and sd of 1
nh=rnorm(n=20,mean=3,sd=1);nh 
# What is the mean of the first 10 numbers? What about the second 10?
# Remember, these all come from the same distribution
mean(nh[1:10]) #first 10 numbers
mean(nh[11:20]) #second 10 numbers

# Now what is the average difference between all the values from the two subsamples?
diff=mean(nh[1:10])-mean(nh[11:20]);diff

# Now let's plot the two different sets of data - the first 10 and second 10 - and see how they differ
curve(dnorm(x,3,1),xlim=c(0,6),ylim=c(0,.5),xlab="Response variable",ylab="Probability");abline(v=3)
lines(density(nh[1:10]),lty=1,main="",col="blue");abline(v=mean(nh[1:10]),col="blue");
text(1.5,.3,"1st 10\n numbers",col="blue")
lines(density(nh[11:20]),lty=1,main="",col="red");abline(v=mean(nh[11:20]),col="red");
text(5,.3,"2nd 10\n numbers",col="red")
# THey are both on the same graph as the original normal distribution in black - but slightly different
# THis difference is due to random chance!



# * 3.2 Make histograms and add stuff to them! -----------------------------


## SLIDE 52/53
# This assigns color values to these data objects
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

# Running hist() over and over adds the colors for each category
hist(x1$MYLU_LLambda,lty=1,main="",breaks=20,col=0,xlab="Log10 Pop growth rates")
hist(x1$MYLU_LLambda[x1$Visits_t==1],col=c1,lty=2,add=T,breaks=20)
hist(x1$MYLU_LLambda[x1$Visits_t==2],col=c2,lty=2,add=T,breaks=20)
# abline adds lines at the means that we calculate
abline(v=mean(x1$MYLU_LLambda))
abline(v=mean(x1$MYLU_LLambda[x1$Visits_t==1]),col="lightblue")
abline(v=mean(x1$MYLU_LLambda[x1$Visits_t==2]),col="pink")


# * 3.3 Create density plot -----------------------------------------------

# Now instead of a histogram, create a density plot of all of the data
plot(density(x1$MYLU_LLambda),col="black",lty=1,main="",ylim=c(0,1.5),lwd=2,
     xlab="Log10 Pop growth rates")
# Now plot each group (1 vs 2 visits) separately
lines(density(x1$MYLU_LLambda[x1$Visits_t==1]),col="red",lty=2)
lines(density(x1$MYLU_LLambda[x1$Visits_t==2]),col="blue",lty=2)
# And also add the means as vertical lines
abline(v=mean(x1$MYLU_LLambda))
abline(v=mean(x1$MYLU_LLambda[x1$Visits_t==1]),col="lightblue")
abline(v=mean(x1$MYLU_LLambda[x1$Visits_t==2]),col="pink")
# And add text!
text(-.7,1.4,"1 visit",col="red");text(-.7,.9,"2 visits",col="blue");
text(-.7,1.2,"A single distribution",col="black")
text(-.7,1.1,"for all the data",col="black")


# * 3.4 Visualize distribution, mean, sd ----------------------------------

## SLIDE 54
# Visualize distribution, mean, and sd for data
m_mll=mean(x1$MYLU_LLambda);sd_mll=sd(x1$MYLU_LLambda) # calculate a mean of ALL the data, regardless of what "group" it belongs too
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


# 4. Simulate differences between popn growth rates --------------------------------------------------------------

## SLIDE 55 & 56
# Simulate random data and see how often we get a result as big as we observed with the real data
diff=NA #initialize variable
nsim=10000 # # of simulations
set.seed(1) #make random draws repeatable
for (i in 1:nsim) {
  draws=rnorm(89,mean=m_mll,sd=0.428) #draw from normal dist
  means=c(mean(draws[1:51]),mean(draws[52:89])) #calculate means of 1st 51 #s & other 38
  diff[i]=means[2]-means[1] #store diff b/w means
}
draws[1:51]
draws[52:89]
mean(draws[1:51])
mean(draws[52:89])
diff[i]
## SLIDE 58 - first 100 simulations
diff[1:100]


## SLIDE 59-64
# Graph a histrogram of the mean difference between popns
par(mfrow=c(1,1),mgp=c(1.5,.5,0),cex=1.,mar=c(3, 3, 0.5, 0.5))#b,l,t,r # Set some of the plotting parameters
hist(diff,xlab="Difference between simulated log pop growth rates",main="",xlim=c(-.4,.4)); # Create the histogram
abline(v=meandiff,col="red") # Add a line at the observed difference
text(-.24,1800,"Difference observed",col="red") # Add text
text(-.24,1600,round(meandiff,3),col="red")

## SLIDE 60-64
# Graph a histrogram of the absolute difference between popns
hist(abs(diff),xlab="Difference between simulated log pop growth rates",main="");
abline(v=abs(meandiff),col="red")
text(.16,1600,"abs(Difference observed)",col="red")
text(.11,1500,abs(round(meandiff,3)),col="red")

# Calculate fractions of simulations with bigger differences
# These are p-values!!
## SLIDE 64!!!!
sum((diff)>0.073)/nsim #fraction of sims w/ bigger diff
sum(abs(diff)>0.073)/nsim #fraction of sims w/ bigger abs diff
meandiff=x1m[2,2]-x1m[1,2];meandiff

# Fit the model using lm
f1=lm(MYLU_LLambda~as.factor(Visits_t),data=x1) #fit model to get SE of diff
meandiff+1.96*c(0.092,-0.092) #approx 95% CI for diff

# Tues. Jan. 7th, 2025 ----------------------------------------------------------

# CRICKET EXAMPLE ---------------------------------------------------------
## SLIDE 74

#Correlation: Cricket example


# 1. Load in the data -----------------------------------------------------


m=read.csv("RegressionDataCrickets.csv");m


# 2. Covariation of the data ----------------------------------------------------------


# * 2.1 Plot the data and run correlation ---------------------------------

## SLIDE 76
# Plot it up - eyeball test!
ggplot(m,aes(y=ChirpsPerMin,x=TempF))+theme_few()+
  geom_point(size=2)+labs(x="Temperature, degrees F",y="Chirps per minute")

## SLIDE 79
# Correlation between paired samples 
# the covariation of the variables divided by the square root of the product of the variation of each
cov(m$TempF,m$ChirpsPerMin)/sqrt(var(m$TempF)*var(m$ChirpsPerMin)) # This does it by hand
cor.test(m$TempF,m$ChirpsPerMin)$estimate # This is the nice built-in test! Thank goodness.

# Look at the different parts of cor.test objects
obj <- cor.test(m$TempF,m$ChirpsPerMin)
str(obj)

# * 2.2. Create density plots of the variables ----------------------------

## SLIDE 82
# Create a density plot of the random data that has a mean and sd of the chirps data
curve(dnorm(x,mean(m$ChirpsPerMin),sd(m$ChirpsPerMin)),xlab="Chirps per minute",ylab="Probability",
      xlim=c(mean(m$ChirpsPerMin)-4*sd(m$ChirpsPerMin),mean(m$ChirpsPerMin)+4*sd(m$ChirpsPerMin)),)
abline(v=mean(m$ChirpsPerMin)) # Add a line at the mean value of chirps per min.
text(19,.25,paste("Mean=",round(mean(m$ChirpsPerMin),3)) ) # Add the text
text(19,.22,paste("SD=",round(sd(m$ChirpsPerMin),3)) )

# Create a density plot of the random data that has a mean and sd of the Temperature data
curve(dnorm(x,mean(m$TempF),sd(m$TempF)),xlab="Temperature, degrees F",ylab="Probability",
      xlim=c(mean(m$TempF)-4*sd(m$TempF),mean(m$TempF)+4*sd(m$TempF)),);
abline(v=mean(m$TempF));text(20,.2,paste("SD=",round(sd(m$TempF),3)) )
text(100,.035,paste("Mean=",round(mean(m$TempF),3)) )
text(100,.03,paste("SD=",round(sd(m$TempF),3)) )



# 3. Simulations to calculate likelihood of happening by chance -----------



# * 3.1 Run simulations ---------------------------------------------------

## SLIDE 84
nsim=10000 # of draws/simulations
rxyv=NA #initialize correlation value variable
set.seed(1) #make random draws repeatable
for (i in 1:nsim) {
  chirp=rnorm(15,mean=mean(m$ChirpsPerMin),sd=sd(m$ChirpsPerMin))
  tempF=rnorm(15,mean=mean(m$TempF),sd=sd(m$TempF))
  rxyv[i]=cor.test(tempF,chirp)$estimate #calculate correlation coef
}

## SLIDE 85
cbind(chirp,tempF)
plot(chirp~tempF,pch=19)
rxyv[9990:10000]
cor.test(tempF,chirp)$estimate


# * 3.2 Plot the simulated (random) data ----------------------------------------

## SLIDE 87
# Plot the one-tailed version
hist(rxyv,main="",xlim=c(-1,1));abline(v=0.695,col="red",lwd=2);
text(0.35,1350,"Observed value\n 0.695",col="red")
# plot the two-tailed version
hist(abs(rxyv),main="");abline(v=0.695,col="red",lwd=2)
text(0.55,1250,"Observed value\n 0.695",col="red")

## STILL SLIDE 87
# Fraction of draws with more positive and bigger correlations than that observed in the real data
# p-values!!
sum(rxyv>0.695)/nsim #frac of draws w/ more positive correlation
sum(abs(rxyv)>0.695)/nsim #frac of draws w/ bigger correlation


# * 3.3 Plot the data with a regression line ------------------------------

## SLIDE 92

ggplot(m,aes(x=TempF,y=ChirpsPerMin))+ # put the data in the ggplot and give it the x and y axis variables
  theme_few()+ # Add on a new theme for funsies
  geom_point(size=3)+ # Add points to the graph
  stat_smooth(method="lm")+ # Add on a linear model (method = "lm") line fit to the data
  xlab("Temperature(F)")+ # X label
  ylab("Chirps per minute")+ # Y label
  ggpubr::stat_regline_equation() # and lastly, add in the regression line equation onto the graph using a function in the ggpubr package

