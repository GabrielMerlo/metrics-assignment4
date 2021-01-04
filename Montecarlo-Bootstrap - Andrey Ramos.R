#Montecarlo Simulations

set.seed(12345)
N = 1000

# 300 Observations

many_mean = rep(NA, N)
many_sd = rep(NA, N)
for(i in 1:N){
  X = rnorm(300, 0, 1)
  X_mean = mean(X)
  X_sd = sd(X)
  many_mean[i] = X_mean
  many_sd[i]=X_sd
}

CI_asy_1=many_mean+qnorm(0.975)*many_sd/sqrt(300)
CI_asy_2=many_mean-qnorm(0.975)*many_sd/sqrt(300)

CEA300=mean(CI_asy_2 <= 0 & 0 <= CI_asy_1)-0.95

# 100 observations

many_mean = rep(NA, N)
many_sd = rep(NA, N)
for(i in 1:N){
  X = rnorm(100, 0, 1)
  X_mean = mean(X)
  X_sd = sd(X)
  many_mean[i] = X_mean
  many_sd[i]=X_sd
}

CI_asy_1=many_mean+qnorm(0.975)*many_sd/sqrt(100)
CI_asy_2=many_mean-qnorm(0.975)*many_sd/sqrt(100)

CEA100=mean(CI_asy_2 <= 0 & 0 <= CI_asy_1)-0.95

# 50 observations

many_mean = rep(NA, N)
many_sd = rep(NA, N)
for(i in 1:N){
  X = rnorm(50, 0, 1)
  X_mean = mean(X)
  X_sd = sd(X)
  many_mean[i] = X_mean
  many_sd[i]=X_sd
}

CI_asy_1=many_mean+qnorm(0.975)*many_sd/sqrt(50)
CI_asy_2=many_mean-qnorm(0.975)*many_sd/sqrt(50)

CEA50=mean(CI_asy_2 <= 0 & 0 <= CI_asy_1)-0.95

table(CEA300, CEA100, CEA50)

# In general, the coverage error of the interval decreases when the sample size increases.

# Boostrap

# 300 observations

B=500
StatisticsB=matrix (NA, 500, 1000)
many_mean=rep(NA,1000)
many_sd=rep(NA,1000)

for (j in 1:1000){
X=rnorm(300,0,1)
centered=X-mean(X)
for(i in 1:B){
  rsample=sample (centered, size=300, replace =T) 
  meanB=mean(rsample)
  sdB=sd(rsample)
  statistic=sqrt(300)*meanB/sdB
  StatisticsB[i,j]=statistic
}
  X_mean=mean(X)
  X_sd=sd(X)
  many_mean[j] = X_mean
  many_sd[j]=X_sd
}

confidence=matrix(NA,1000,2)

for (i in 1:1000){
  CI_asy_1=many_mean[i]+quantile(StatisticsB[,i],0.975)*many_sd[i]/sqrt(300)
  CI_asy_2=many_mean[i]-quantile(StatisticsB[,i],0.975)*many_sd[i]/sqrt(300)
  confidence[i,1]= CI_asy_1
  confidence[i,2]= CI_asy_2
  }

CEB300=mean(confidence[,2] <= 0 & 0 <= confidence[,1])-0.95

# 100 observations

StatisticsB=matrix (NA, 500, 1000)
many_mean=rep(NA,1000)
many_sd=rep(NA,1000)

for (j in 1:1000){
  X=rnorm(100,0,1)
  centered=X-mean(X)
  for(i in 1:B){
    rsample=sample (centered, size=100, replace =T) 
    meanB=mean(rsample)
    sdB=sd(rsample)
    statistic=sqrt(100)*meanB/sdB
    StatisticsB[i,j]=statistic
  }
  X_mean=mean(X)
  X_sd=sd(X)
  many_mean[j] = X_mean
  many_sd[j]=X_sd
}

confidence=matrix(NA,1000,2)

for (i in 1:1000){
  CI_asy_1=many_mean[i]+quantile(StatisticsB[,i],0.975)*many_sd[i]/sqrt(100)
  CI_asy_2=many_mean[i]-quantile(StatisticsB[,i],0.975)*many_sd[i]/sqrt(100)
  confidence[i,1]= CI_asy_1
  confidence[i,2]= CI_asy_2
}

CEB100=mean(confidence[,2] <= 0 & 0 <= confidence[,1])-0.95


#50 Observations

StatisticsB=matrix (NA, 500, 1000)
many_mean=rep(NA,1000)
many_sd=rep(NA,1000)

for (j in 1:1000){
  X=rnorm(100,0,1)
  centered=X-mean(X)
  for(i in 1:B){
    rsample=sample (centered, size=100, replace =T) 
    meanB=mean(rsample)
    sdB=sd(rsample)
    statistic=sqrt(100)*meanB/sdB
    StatisticsB[i,j]=statistic
  }
  X_mean=mean(X)
  X_sd=sd(X)
  many_mean[j] = X_mean
  many_sd[j]=X_sd
}

confidence=matrix(NA,1000,2)

for (i in 1:1000){
  CI_asy_1=many_mean[i]+quantile(StatisticsB[,i],0.975)*many_sd[i]/sqrt(100)
  CI_asy_2=many_mean[i]-quantile(StatisticsB[,i],0.975)*many_sd[i]/sqrt(100)
  confidence[i,1]= CI_asy_1
  confidence[i,2]= CI_asy_2
}

CEB50=mean(confidence[,2] <= 0 & 0 <= confidence[,1])-0.95

Results<-matrix(c(CEA300, CEA100, CEA50, CEB300, CEB100, CEB50), 2,3, byrow = T)

# There is a gain in coverage by using boostrap methods.
