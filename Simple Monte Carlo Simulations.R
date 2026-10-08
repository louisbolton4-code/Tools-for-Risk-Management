#Setting a seed means that you are fixing a defined random process that will always return the same values
set.seed(10)
print(sample(1:25,3))
#Here the returned sample will always be 11, 9 and 10 when simultaneously set as seed 10



#The following is a simple way of simulating throwing a fair coin
samples<-sample(c("H","T"),10,replace=TRUE,prob=c(0.5,0.5))
print(samples)



#We can use simulations to verify what we know about distributions
n<-10^4 #nr samples, arbitrary 'large' number
samples<-sample(seq(1,6),n,replace=TRUE,prob=rep(1/6,6))
estimate<-sum(samples)/n
cat("The estimated expected value is",estimate)
#As the experiment is that of a fair six sided die, we expect the estimate to be close to 3.5



#We can use built in distributions as well
n<-10^4 #nr samples, arbitrary 'large' number
#r followed by a distribution produces n observations from that distribution
samples_U<-rnorm(n,1,2) #n samples from U~N(1,2)
samples_V<-rbeta(n,1,1) #n samples from V~Beta(1, 1)
samples_X<-samples_U*samples_V 

#We can estimate the expectation of the product of 2 distributions thanks to a large number n
est_exp_X<-sum(samples_X)/n
cat("The estimated expected value of X is",est_exp_X)


#Here we define the indicator function where we aim to retain the samples greater than 2
indicator<-function(x) {
  if (x>2) {
    return(1)} 
  else {
    return(0)}}

samples_X_indicated<-sapply(samples_X,indicator)
#Here we estimate the probability of observing a sample X greater than 2
est_prob_X<-sum(samples_X_indicated)/n
cat("The estimated probability is",est_prob_X)



#The following is an estimation of Unif{1, ..., 6} with and increase in sample size 
#Leading to a better and better estimate of the true expectation
for(k in 1:6) {
  n=10^k
  Samples_X = sample(seq(1, 5), n, replace=TRUE, prob=rep(1/5, 5))
  Estimate_X = sum(Samples_X)/n
  cat("Estimated E[X]:", k, Estimate_X)
}
