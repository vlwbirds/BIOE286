set.seed(2) #make draws repeatable
sd(rnorm(n=500,mean=25,sd=30)) #sd of 500 numbers
# 30.97133
sd(rnorm(n=5000,mean=25,sd=30)) #sd of 5000 numbers
# 29.74251
sd(rnorm(n=5000,mean=125,sd=30)) #sd of 5000 numbers w/ different mean
# 30.20055

sd(colMeans(matrix(rnorm(n=100),ncol=10))) #sd of 10 means of n=10 numbers each*
# 0.2883578
sd(colMeans(matrix(rnorm(1000),ncol=10))) #sd of 10 means of n=100 numbers each
# 0.1116078
sd(colMeans(matrix(rnorm(1000),ncol=100))) #sd of 100 means of n=10 numbers each
# 0.3097929


