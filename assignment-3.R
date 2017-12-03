rm(list = ls())
####################################################################################
##                                   Assignment 3                                ###
##                             Ehsan Ghamarian (5805848)                         ###
####################################################################################

require(ggplot2)

install.packages("ggplot2")
###################################################################################
#                       Question One- Part 1: simulating data                    ##
###################################################################################
## Create two groups, with 50 subjects each
## Control has a higher mean (160) than test group (150)
## Standard deviation is 15

set.seed(333)   # we use this to make our code replicable. 
control <- matrix(rnorm(50,150,15), nrow=50, ncol=1) # This line generates a normal distribution of 50 cases with a mean of 150 and sd of 15.
experimental <- matrix(rnorm(50,160,15), nrow=50, ncol=1) # This line generates a normal distribution of 50 cases with a mean of 160 and sd of 15.


set.seed(333)
TTest <- t.test(control, experimental, var.equal = TRUE) #we use in-built t.test function in R to test whether the mean difference between the two groups is significantly different from zero.
TTest # here we produce the results in the console, which are as follows:

#Two Sample t-test

#data:  control and experimental
#t = -3.1042, df = 98, p-value = 0.002495
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -14.695759  -3.233617
#sample estimates:
#  mean of x mean of y 
#150.1759  159.1406 

#### Conclusion ####

# Based on the result of the t.test, we can conclude that the mean difference 
#between the two groups is significantly different from zero (p< .05). To see this 
#graphically, we can use the following code

randT <- rt(30000, df=49) # generate 30,000 cases from a t.distribution with 49 degrees of freedom

ggplot(data.frame(x=randT)) +             #This is where we specify what we would like to plot
  geom_density(aes(x=x), fill="lightslategrey") + #here we add a layer with the density of the t.distribution we acquired above
  geom_vline(xintercept=TTest$statistic, col = "red")+ #this adds a vertical line specifying where our obtained t value is
  geom_vline(xintercept=mean(randT) + c(-2, 2) * sd(randT), col = "blue", linetype=2) # this adds vertical lines with 2 sd in either direction from the mean of the t distribution we generated.



#######################################################################################
#                                 Question 1- Part 2                                  #
#######################################################################################


set.seed(333)

power.stat <- function(n1, n2, nsim=1000, mean1, mean2, sd1, sd2){
  lower <- qt(.025,df=sum(n1, n2) - 2)
  upper <- qt(.975,df=sum(n1, n2) - 2)
  t.statistic <- replicate(nsim, t.test(rnorm(n1, mean1, sd1), rnorm(n2, mean2, sd2))$statistic)
  sum(t.statistic < lower | t.statistic > upper) / nsim
}

power.p.value <- function(n1, n2, nsim=1000, mean1, mean2, sd1, sd2, var.equal=TRUE){
  t.pvalue <- replicate(nsim, t.test(rnorm(n1, mean1, sd1), rnorm(n2, mean2, sd2))$p.value)
  sum(t.pvalue < .025 | t.pvalue > .975) / nsim
  hist(t.pvalue)
}

power.stat(50, 50, 1000, 150, 160, 15, 15)

power.p.value(50, 50, 1000, 150, 160, 15, 15, var.equal = FALSE)

powerfunction.t.test <- function(N, n1, mean1, stdev1, n2, mean2, stdev2, alpha, alternative=alternative.default){
  alternative.default="two.sided"
  tvalues <- matrix(data = NA, nrow = N, ncol = 1) # empty matrix for the t-statistics
  pvalues <- matrix(data = NA, nrow = N, ncol = 1) # empty matrix for the p-values
  for(i in 1:N){
    group1 <- rnorm(n=n1,mean=mean1,sd=stdev1) # simulate group 1 based on the chosen parameters
    group2 <- rnorm(n=n2,mean=mean2,sd=stdev2) # simulate group 2 based on the chosen parameters
    if(alternative==alternative.default){
      tvalues[i,] <- t.test(group1, group2, alternative = "two.sided", var.equal = TRUE)$statistic
      pvalues[i,] <- t.test(group1, group2, alternative = "two.sided", var.equal = TRUE)$p.value
    } else if(alternative == "greater"){
      tvalues[i,] <- t.test(group1, group2, alternative = "greater", var.equal = TRUE)$statistic
      pvalues[i,] <- t.test(group1, group2, alternative = "greater", var.equal = TRUE)$p.value
    }else if(alternative=="less"){
      tvalues[i,] <- t.test(group1, group2, alternative = "less", var.equal = TRUE)$statistic
      pvalues[i,] <- t.test(group1, group2, alternative = "less", var.equal = TRUE)$p.value
    }
  }
  
  power <- length(pvalues[alpha >= pvalues]) / N
  return(power)
}

power.t.test(n = 50, delta = 10, sd = 15, type = "two.sample")
power.t.test(n = 50, delta = 10, sd = 15, type = "two.sample", alternative = "one.sided")


####################################################################################
#                              Question 2- Part 1                                  #
####################################################################################
## Boostrapping
# ...

require(ggplot2)

set.seed(333)

Bootstrap.t.test <- function(x, y, nsim){
  # This function performs a bootstrap for estimation purposes.
  sample.t.value <- t.test(x, y)$statistic
  boot.t.values<-matrix(rep(0,nsim), nsim, 1)
  combined <- c(x, y)
  for (i in 1:nsim){
    #sampling with replacement, keeping group structure 
    bootsample1 <- sample(combined, size = length(x), replace=TRUE)
    bootsample2 <- sample(combined, size = length(y), replace=TRUE)
    boot.t.values[i] <- t.test(bootsample1, bootsample2)$statistic
  }
  mean1 <- mean(x)
  mean2 <- mean(y)
  
  meanboot.t.value <- mean(boot.t.values)
  bias <- meanboot.t.value - sample.t.value
  # obtain 95% bootstrap percentile confidence interval
  CI<-quantile(boot.t.values, c(0.025, 0.975))
  
  a<-data.frame(boot.t.values)
  
  plot <- ggplot(data = a, mapping = aes(a$boot.t.values)) +
    geom_density(fill = "lightslategray") +
    geom_vline(xintercept=sample.t.value, col = "red") + 
    geom_vline(xintercept = CI, col = "blue", linetype = 2) +
    labs(title = "Bootstrap T- test", x="Bootstrap t values", y = "Frequency") + 
    theme(plot.title = element_text(hjust = 0.5))
  
  print(plot)
  pvalue <- sum(abs(boot.t.values) > abs(sample.t.value)) / nsim
  
  list(sample.t.value=sample.t.value, 
       meanboot.t.value=meanboot.t.value,
       CI=CI, bias=bias,
       mean1=mean(x), mean2=mean(y), pvalue=pvalue)
}

## Flight data ##
CSFI <- c(2, 5, 5, 6, 6, 7, 8, 9)
TFI <- c(1, 1, 2, 3, 3, 4, 5, 7, 7, 8)

Bootstrap.t.test(CSFI, TFI, 1000)
Bootstrap.t.test(TFI, CSFI, 1000)

Bootstrap.t.test(control, experimental, 1000)
Bootstrap.t.test(experimental, control, 1000)

index.ttest <-function(x,y,nsim){
  sample.t.value <- t.test(x, y)$statistic
  boot.t.values<-matrix(rep(0,nsim),nrow=nsim,ncol=1)
  
  combined <- c(x, y)

  for(i in 1:nsim){
    xindex<-as.integer(runif(length(x),min=1,max=length(combined) + 1))
    yindex<-as.integer(runif(length(y),min=1,max=length(combined) + 1))
    bootsample1<-combined[c(xindex)]
    bootsample2<-combined[c(yindex)]
    boot.t.values[i]<-t.test(bootsample1,bootsample2)$statistic
    
  }
  mean1 <- mean(x)
  mean2 <- mean(y)
  
  meanboot.t.value <- mean(boot.t.values)
  bias <- meanboot.t.value - sample.t.value
  # obtain 95% bootstrap percentile confidence interval
  CI<-quantile(boot.t.values, c(0.025, 0.975))
  
  a<-data.frame(boot.t.values)
  
  plot <- ggplot(data = a, mapping = aes(a$boot.t.values)) +
    geom_density(fill = "lightslategray") +
    geom_vline(xintercept=sample.t.value, col = "red") + 
    geom_vline(xintercept = CI, col = "blue", linetype = 2) +
    labs(title = "Bootstrap T- test", x="Bootstrap t values", y = "Frequency") + 
    theme(plot.title = element_text(hjust = 0.5))
  
  print(plot)
  pvalue <- sum(abs(boot.t.values) > abs(sample.t.value)) / nsim
  
  list(sample.t.value=sample.t.value, 
       meanboot.t.value=meanboot.t.value,
       CI=CI, bias=bias,
       mean1=mean(x), mean2=mean(y), pvalue=pvalue)
 
}

index.ttest(CSFI, TFI, 1000)

jacknife.estimate <-function(x,y) {
  xlen <- length(x)
  theta.hat = mean(x) - mean(y)
  theta.jack = numeric(xlen)
  for (i in 1:length(x))
    theta.jack[i] <- mean(x[-i]) - mean(y)
  bias <- (xlen - 1) * (mean(theta.jack) - theta.hat)
  bias
}

jacknife.estimate(TFI, CSFI)


####################################################################################
#                              Question 2- Part 2                                  #
####################################################################################

library(ggplot2)

csfi<-c(2,5,5,6,6,7,8,9)
tfi<-c(1,1,2,3,3,4,5,7,7,8)

set.seed(333)

resample.ttest<-function(x,y,nsim){
  tval<-t.test(x,y)$statistic
  resamppval<-matrix(rep(0,nsim),nrow=nsim,ncol=1)
  resamptval<-matrix(rep(0,nsim),nrow=nsim,ncol=1)
  
  for(i in 1:nsim){
    xindex<-as.integer(runif(length(x),min=1,max=length(x)+0.999999))
    yindex<-as.integer(runif(length(y),min=1,max=length(y)+0.999999))
    bootx<-x[c(xindex)]
    booty<-y[c(yindex)]
    resamptval[i]<-t.test(bootx,booty)$statistic
    resamppval[i]<-t.test(bootx,booty)$p.value
  }
  CI<-quantile(resamptval,c(0.025,0.975))
  z<-data.frame(resamptval)
  a<-ggplot(data=z,mapping=aes(z$resamptval)) +
    labs(title="Bootstrap T-test", x="Bootstrap t-value", y="Count") +
    geom_histogram(binwidth = 0.15,fill="firebrick1",col=I("firebrick")) +
    geom_vline(xintercept=tval,linetype="dotted",size=.8) +
    geom_vline(xintercept = CI,col="blue",linetype="dashed",size=0.8) +
    theme(plot.title = element_text(hjust = 0.5))
  
  w<-data.frame(resamppval)
  b<-ggplot(data=w,mapping=aes(w$resamppval)) +
    labs(title="Bootstrap p-value", x="Bootstrap p-value", y="Count") +
    geom_histogram(binwidth = 0.01,fill="firebrick1",col=I("firebrick")) +
    geom_vline(xintercept=0.05,linetype="dotted",size=.8) +
    theme(plot.title = element_text(hjust = 0.5))
  #print(a)
  #print(b)
  list(tval=tval,resamptval=resamptval,CI=CI)
}
resample.ttest(CSFI,TFI,1000)
####################################################################################
#                              Question 2- Part 3                                  #
####################################################################################