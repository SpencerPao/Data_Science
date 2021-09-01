rm(list=ls())
# K Means Set
setwd('C:/Users/Spenc/Documents/Youtube/Machine Learning/R/Bayesian Regression')
# 100 clusters
data <- read.csv("Kmeans_scaled_data100.csv")
# Taking out the proportions since I now have my logit data which converted the 
# predictor's domain from -inf to +inf
# 1 was defaulting on loan
# 0 not defaulting on loan
data <- data[,-c(dim(data)[2]- 1)]

index = data$Cluster_ID
# Taking out the cluster numbers from the analysis
data <- data[,-c(1)]


# Let us normalize the data...
# Storing the Response
response = data$logit_loan_default

# Normalizing the covariates
data = data[,-c(dim(data)[2])]

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

dfNorm <- as.data.frame(lapply(data, normalize))


y = response
x = dfNorm

# Combining the data...
# Separating training and testing sets...

dat <- cbind(x,y)
names(dat)[9] <- "LOGIT_LOAN_DEFAULT"
# Seperating for test and train...
# Predicting 7 Observations (Since I only have 30 observations )
test <- dat[1:20,]
train <- dat[-c(1:20),]

y = train$LOGIT_LOAN_DEFAULT
x = train[,c(1:8)]

model_lm <- lm(LOGIT_LOAN_DEFAULT~., data = dat)
pred_lm <- predict(model_lm, test[,c(-9)])
library(Metrics)
rmse(actual = test$LOGIT_LOAN_DEFAULT, predicted = pred_lm)
# construct your various confidence intervals etc...


# Designing the Gibbs Sampler (Markov Chains Monte Carlo Method)
# MCMC comprise a class of algorithms for sampling form a probability distr.
# "By constructing a Markov chain that has the desired distribution as its
# equiliburium distirbution, you can obtain a sample of the desired distribution
# by recording states from the chain." - Wiki

# Prior Specifications
m0 <- 0
m1 <- 0 
m2 <- 0
m3 <- 0 
m4 <- 0
m5 <- 0 
m6 <- 0
m7 <- 0
m8 <- 0

g <- 10
a <- 0.01
b <- 0.01

# Constants for MCMC
n <- dim(train)[[1]]

# Number of variables + Intercept
p <- 8 + 1

# Iterations
I <- 100000 # 100,000
betaStore <- matrix(0,I,9) # Stores Beta values
phiStore <- matrix(0,I,1) # Stores Phi Values
yPredStore <- matrix(0,I,1) # Stores predictions
pPredStore <- matrix(0,I,1) # Stores p values

testPred <- matrix(0,I,20)
predAccuracy <- matrix(0,I,1)

colnames(betaStore) <- c("Intercept",names(train[,-c(9)]))
colnames(phiStore) <- "Phi"

# Starting Values for beta coeffcients
betaCur0 <- m0
betaCur1 <- m1
betaCur2 <- m2
betaCur3 <- m3
betaCur4 <- m4
betaCur5 <- m5
betaCur6 <- m6
betaCur7 <- m7
betaCur8 <- m8
phiCur <- 1 #phi ~ Gamma(a,b)

#*** Implement Gibbs using a for-loop (Known as Markov Chain Monte Carlo) MCMC
#*# These include full conditionals
for (c in 1:I){
  #Draw Beta_0
  yStar <- y-betaCur1*x[,1]-betaCur2*x[,2]-betaCur3*x[,3] -betaCur4*x[,4]-betaCur5*x[,5] -betaCur6*x[,6]-betaCur7*x[,7]-betaCur8*x[,8]
  mnStar0 <- (n+1/g)^(-1)*(sum(yStar)+m0/g)
  sdStar0 <- sqrt((n+1/g)^(-1)*phiCur^(-1))
  betaCur0 <- rnorm(1,mnStar0,sdStar0)
  #Draw Beta_1
  yStar <- y-betaCur0-betaCur2*x[,2]-betaCur3*x[,3] -betaCur4*x[,4]-betaCur5*x[,5] -betaCur6*x[,6]-betaCur7*x[,7]-betaCur8*x[,8]
  mnStar1 <- (sum(x[,1]^2)+1/g)^(-1)*(sum(x[,1]*yStar)+m1/g)
  sdStar1 <- sqrt((sum(x[,1]^2)+1/g)^(-1)*phiCur^(-1))
  betaCur1 <- rnorm(1,mnStar1,sdStar1)
  #Draw Beta_2
  yStar <- y-betaCur0-betaCur1*x[,1]-betaCur3*x[,3] -betaCur4*x[,4]-betaCur5*x[,5] -betaCur6*x[,6]-betaCur7*x[,7]-betaCur8*x[,8]
  mnStar2 <- (sum(x[,2]^2)+1/g)^(-1)*(sum(x[,2]*yStar)+m2/g)
  sdStar2 <- sqrt((sum(x[,2]^2)+1/g)^(-1)*phiCur^(-1))
  betaCur2 <- rnorm(1,mnStar2,sdStar2)
  #Draw Beta_3
  yStar <- y-betaCur0-betaCur1*x[,1]-betaCur2*x[,2]-betaCur4*x[,4]-betaCur5*x[,5] -betaCur6*x[,6]-betaCur7*x[,7]-betaCur8*x[,8]
  mnStar3 <- (sum(x[,3]^2)+1/g)^(-1)*(sum(x[,3]*yStar)+m3/g)
  sdStar3 <- sqrt((sum(x[,3]^2)+1/g)^(-1)*phiCur^(-1))
  betaCur3 <- rnorm(1,mnStar3,sdStar3)
  #Draw Beta_4
  yStar <- y-betaCur0-betaCur1*x[,1]-betaCur2*x[,2]-betaCur3*x[,3]-betaCur5*x[,5] -betaCur6*x[,6]-betaCur7*x[,7]-betaCur8*x[,8]
  mnStar4 <- (sum(x[,4]^2)+1/g)^(-1)*(sum(x[,4]*yStar)+m4/g)
  sdStar4 <- sqrt((sum(x[,4]^2)+1/g)^(-1)*phiCur^(-1))
  betaCur4 <- rnorm(1,mnStar4,sdStar4)
  #Draw Beta_5
  yStar <- y-betaCur0-betaCur1*x[,1]-betaCur2*x[,2]-betaCur3*x[,3]-betaCur4*x[,4] -betaCur6*x[,6]-betaCur7*x[,7]-betaCur8*x[,8]
  mnStar5 <- (sum(x[,5]^2)+1/g)^(-1)*(sum(x[,5]*yStar)+m5/g)
  sdStar5 <- sqrt((sum(x[,5]^2)+1/g)^(-1)*phiCur^(-1))
  betaCur5 <- rnorm(1,mnStar5,sdStar5)
  #Draw Beta_6
  yStar <- y-betaCur0-betaCur1*x[,1]-betaCur2*x[,2]-betaCur3*x[,3]-betaCur4*x[,4] -betaCur5*x[,5]-betaCur7*x[,7]-betaCur8*x[,8]
  mnStar6 <- (sum(x[,6]^2)+1/g)^(-1)*(sum(x[,6]*yStar)+m6/g)
  sdStar6 <- sqrt((sum(x[,6]^2)+1/g)^(-1)*phiCur^(-1))
  betaCur6 <- rnorm(1,mnStar6,sdStar6)
  #Draw Beta_7
  yStar <- y-betaCur0-betaCur1*x[,1]-betaCur2*x[,2]-betaCur3*x[,3]-betaCur4*x[,4] -betaCur5*x[,5]-betaCur6*x[,6]-betaCur8*x[,8]
  mnStar7 <- (sum(x[,7]^2)+1/g)^(-1)*(sum(x[,7]*yStar)+m7/g)
  sdStar7 <- sqrt((sum(x[,7]^2)+1/g)^(-1)*phiCur^(-1))
  betaCur7 <- rnorm(1,mnStar7,sdStar7)
  #Draw Beta_8
  yStar <- y-betaCur0-betaCur1*x[,1]-betaCur2*x[,2]-betaCur3*x[,3]-betaCur4*x[,4] -betaCur5*x[,5]-betaCur6*x[,6]-betaCur7*x[,7]
  mnStar8 <- (sum(x[,8]^2)+1/g)^(-1)*(sum(x[,8]*yStar)+m8/g)
  sdStar8 <- sqrt((sum(x[,8]^2)+1/g)^(-1)*phiCur^(-1))
  betaCur8 <- rnorm(1,mnStar8,sdStar8)
  
  #---------------------------------------------------------------------------
  #Draw phi
  aStar <- n/2+p/2+a
  bTemp1 <- sum( (y-betaCur0-betaCur1*x[,1]-betaCur2*x[,2]-betaCur3*x[,3]-betaCur4*x[,4] -betaCur5*x[,5]-betaCur6*x[,6]-betaCur7*x[,7]-betaCur8*x[,8])^2)
  bTemp2 <- ( (betaCur0-m0)^2+(betaCur1-m1)^2+(betaCur2-m2)^2+(betaCur3-m3)^2 + (betaCur4-m4)^2+(betaCur5-m5)^2+(betaCur6-m6)^2+(betaCur7-m7)^2+(betaCur8-m8)^2) /g
  bStar <- .5*(bTemp1+bTemp2)+b
  phiCur <- rgamma(1,aStar,rate=bStar)
  #Store the draws
  betaStore[c,1] <- betaCur0
  betaStore[c,2] <- betaCur1
  betaStore[c,3] <- betaCur2
  betaStore[c,4] <- betaCur3
  betaStore[c,5] <- betaCur4
  betaStore[c,6] <- betaCur5
  betaStore[c,7] <- betaCur6
  betaStore[c,8] <- betaCur7
  betaStore[c,9] <- betaCur8
  phiStore[c,1] <- phiCur
  #predY
  muPred <- betaCur0+betaCur1*test[,1]+betaCur2*test[,2]+betaCur3*test[,3]+betaCur4*test[,4]+betaCur5*test[,5]+betaCur6*test[,6]+betaCur7*test[,7]+betaCur8*test[,8]
  sdPred <- sqrt(1/phiCur)
  yPredStore[c,1] <- rnorm(1, muPred, sdPred)
  pPredStore[c,1] <- (exp(yPredStore[c,1]) / (1+exp(yPredStore[c,1])))
  # This is to 'convert' the logit function back to the original 'form'
  
  PA = 0
  for (m in 1:dim(test)[1]) {
    for (k in 1:dim(test)[1]) {
      muPred <- betaCur0+betaCur1*test[k,1]+betaCur2*test[k,2]+betaCur3*test[k,3]+betaCur4*test[k,4]+betaCur5*test[k,5]+betaCur6*test[k,6]+betaCur7*test[k,7]+betaCur8*test[k,8]
      sdPred <- sqrt(1/phiCur)
      testPred[c,k] <- rnorm(1, muPred, sdPred)
    }
    # Finding the interval
    
    CITestPred<- quantile(testPred[c,], probs = c(0.025,0.975))
    if (test$LOGIT_LOAN_DEFAULT[m] > as.numeric(CITestPred[1]) & (test$LOGIT_LOAN_DEFAULT[m] < as.numeric(CITestPred[2]))) {
      PA = PA + 1
    }
  }
  predAccuracy[c,1] <- PA / 20
  print(c)
  
}

# Printing the output of the MCMC
par(mfrow=c(5,2))
plot(betaStore[,1], main="Beta0", type="l")
plot(betaStore[,2], main="Beta1", type="l")
plot(betaStore[,3], main="Beta2", type="l")
plot(betaStore[,4], main="Beta3", type="l")
plot(betaStore[,5], main="Beta4", type="l")
plot(betaStore[,6], main="Beta5", type="l")
plot(betaStore[,7], main="Beta6", type="l")
plot(betaStore[,8], main="Beta7", type="l")
plot(betaStore[,9], main="Beta8", type="l")
plot(phiStore[,1], main="Phi", type="l")

# Removing Burn in
Burn_in = 1000
Burn_In_Beta <- betaStore[-c(1:Burn_in),]
Burn_In_Phi <- phiStore[-c(1:Burn_in),] 

par(mfrow=c(5,2))
plot(Burn_In_Beta[,1], main="Post Burn In : Beta0", type="l")
plot(Burn_In_Beta[,2], main="Post Burn In : Beta1", type="l")
plot(Burn_In_Beta[,3], main="Post Burn In : Beta2", type="l")
plot(Burn_In_Beta[,4], main="Post Burn In : Beta3", type="l")
plot(Burn_In_Beta[,5], main="Post Burn In : Beta4", type="l")
plot(Burn_In_Beta[,6], main="Post Burn In : Beta5", type="l")
plot(Burn_In_Beta[,7], main="Post Burn In : Beta6", type="l")
plot(Burn_In_Beta[,8], main="Post Burn In : Beta7", type="l")
plot(Burn_In_Beta[,9], main="Post Burn In : Beta8", type="l")
plot(Burn_In_Phi, main="Post Burn In : Phi", type="l")

library(coda)
# Convergence Test
# Ehhh - It doesn't seem to converge... (for all plots)
# If P value < 0.05, then you can treat the first 1,000 (burn in)
# as not burn in.
# We want the P values to be greater than 0.05. This tells us that the data
# is reaching a 'randomized' distribution. Else, we don't have a good distribution.
con_test_0 <- geweke.diag(as.mcmc(Burn_In_Beta[,1]), frac1=0.1, frac2=0.5)
1-pnorm(abs(con_test_0$z), 0, 1)
con_test_1 <- geweke.diag(as.mcmc(Burn_In_Beta[,2]), frac1=0.1, frac2=0.5)
1-pnorm(abs(con_test_1$z), 0, 1)
con_test_2 <- geweke.diag(as.mcmc(Burn_In_Beta[,3]), frac1=0.1, frac2=0.5)
1-pnorm(abs(con_test_2$z), 0, 1)
con_test_3 <- geweke.diag(as.mcmc(Burn_In_Beta[,4]), frac1=0.1, frac2=0.5)
1-pnorm(abs(con_test_3$z), 0, 1)
con_test_4 <- geweke.diag(as.mcmc(Burn_In_Beta[,5]), frac1=0.1, frac2=0.5)
1-pnorm(abs(con_test_4$z), 0, 1)
con_test_5 <- geweke.diag(as.mcmc(Burn_In_Beta[,6]), frac1=0.1, frac2=0.5)
1-pnorm(abs(con_test_5$z), 0, 1)
con_test_6 <- geweke.diag(as.mcmc(Burn_In_Beta[,7]), frac1=0.1, frac2=0.5)
1-pnorm(abs(con_test_6$z), 0, 1)
con_test_7 <- geweke.diag(as.mcmc(Burn_In_Beta[,8]), frac1=0.1, frac2=0.5)
1-pnorm(abs(con_test_7$z), 0, 1)
con_test_8 <- geweke.diag(as.mcmc(Burn_In_Beta[,9]), frac1=0.1, frac2=0.5)
1-pnorm(abs(con_test_8$z), 0, 1)
con_test_phi <- geweke.diag(as.mcmc(Burn_In_Phi), frac1=0.1, frac2=0.5)
1-pnorm(abs(con_test_phi$z), 0, 1)


# Summarize the Marginal Posterior Distributions
# Means
ex0 <- mean(Burn_In_Beta[,1]); ex0
ex1 <- mean(Burn_In_Beta[,2]); ex1
ex2 <- mean(Burn_In_Beta[,3]); ex2
ex3 <- mean(Burn_In_Beta[,4]); ex3
ex4 <- mean(Burn_In_Beta[,5]); ex4
ex5 <- mean(Burn_In_Beta[,6]); ex5
ex6 <- mean(Burn_In_Beta[,7]); ex6
ex7 <- mean(Burn_In_Beta[,8]); ex7
ex8 <- mean(Burn_In_Beta[,9]); ex8
exPhi <- mean(Burn_In_Phi); exPhi

# Standard Deviations
sd(Burn_In_Beta[,1])
sd(Burn_In_Beta[,2])
sd(Burn_In_Beta[,3])
sd(Burn_In_Beta[,4])
sd(Burn_In_Beta[,5])
sd(Burn_In_Beta[,6])
sd(Burn_In_Beta[,7])
sd(Burn_In_Beta[,8])
sd(Burn_In_Beta[,9])
sd(Burn_In_Phi)

# Quantiles
ci0 <- quantile(Burn_In_Beta[,1], prob=c(.025,.975)) 
ci1 <- quantile(Burn_In_Beta[,2], prob=c(.025,.975)) 
ci2 <- quantile(Burn_In_Beta[,3], prob=c(.025,.975)) 
ci3 <- quantile(Burn_In_Beta[,4], prob=c(.025,.975)) 
ci4 <- quantile(Burn_In_Beta[,5], prob=c(.025,.975))
ci5 <- quantile(Burn_In_Beta[,6], prob=c(.025,.975))
ci6 <- quantile(Burn_In_Beta[,7], prob=c(.025,.975))
ci7 <- quantile(Burn_In_Beta[,8], prob=c(.025,.975))
ci8 <- quantile(Burn_In_Beta[,9], prob=c(.025,.975))
ciPhi <- quantile(Burn_In_Phi, probs = c(.025,.975))

# Graphs
par(mfrow=c(5,2)) 
hist(Burn_In_Beta[,1], main="Beta0", prob=TRUE); abline(v=c(ex0,ci0), col="red") 
hist(Burn_In_Beta[,2], main="Beta1", prob=TRUE); abline(v=c(ex1,ci1), col="red") 
hist(Burn_In_Beta[,3], main="Beta2", prob=TRUE); abline(v=c(ex2,ci2), col="red") 
hist(Burn_In_Beta[,4], main="Beta3", prob=TRUE); abline(v=c(ex3,ci3), col="red") 
hist(Burn_In_Beta[,5], main="Beta4", prob=TRUE); abline(v=c(ex4,ci4), col="red") 
hist(Burn_In_Beta[,6], main="Beta5", prob=TRUE); abline(v=c(ex5,ci5), col="red") 
hist(Burn_In_Beta[,7], main="Beta6", prob=TRUE); abline(v=c(ex6,ci6), col="red") 
hist(Burn_In_Beta[,8], main="Beta7", prob=TRUE); abline(v=c(ex7,ci7), col="red") 
hist(Burn_In_Beta[,9], main="Beta8", prob=TRUE); abline(v=c(ex8,ci8), col="red") 
hist(Burn_In_Phi, main="Phi", prob=TRUE); abline(v=c(exPhi,ciPhi), col="red")

# Seeing significance.
pr0 <- sum(Burn_In_Beta[,1] > 0)/(I-Burn_in); pr0
pr1 <- sum(Burn_In_Beta[,2] > 0)/(I-Burn_in); pr1
pr2 <- sum(Burn_In_Beta[,3] > 0)/(I-Burn_in); pr2
pr3 <- sum(Burn_In_Beta[,4] > 0)/(I-Burn_in); pr3
pr4 <- sum(Burn_In_Beta[,5] > 0)/(I-Burn_in); pr4
pr5 <- sum(Burn_In_Beta[,6] > 0)/(I-Burn_in); pr5
pr6 <- sum(Burn_In_Beta[,7] > 0)/(I-Burn_in); pr6
pr7 <- sum(Burn_In_Beta[,8] > 0)/(I-Burn_in); pr7
pr8 <- sum(Burn_In_Beta[,9] > 0)/(I-Burn_in); pr8

# prediction
Burn_In_Pred <- yPredStore[-c(1:Burn_in), ]
par(mfrow = c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
plot(Burn_In_Pred, type = "l", xlab = "Index", ylab = "Predicition")
# taking a look at test
test

# histogram
hist(Burn_In_Pred)
# Mean prediction
mean(Burn_In_Pred)
sd(Burn_In_Pred)
quantile(Burn_In_Pred, probs = c(0.025,0.975))
HPDinterval(as.mcmc(Burn_In_Pred), 0.95)


# 0 to 1 Conversion
# From the confidence interval...
#bound1 <- exp(-0.6764872)/(1+exp(-0.6764872))
#bound2 <- exp(-0.2433787)/(1+exp(-0.2433787))



d <- cbind(x,y)
library(rstanarm)
# MCMC is default.
# default priors are weakly informative.
# default prior is Normal(0,2.5) , mean std.
model <- stan_glm(y ~. ,data = d)
summary(model)
# mean_PPD: Mean of the posterior pred. distribution 
# log-posterior: Similar to the log-likelihood form max. likelihood
# n_eff: estimate of the effective number of draws from the posterior distribution
# Should be greater than 10% of max
# R hat: Measures ratio of the average variance of samples within each to the 
# variance of the pooled samples across chains
# If all chains are stationary, R hat will be 1. In general, you want these values
# to be < 1.1
model$stanfit
# Check out the prior
prior_summary(model)
