#data<-data.frame(as.matrix(y,x,x1))
#data1<- with(data, list(x=x,y=y,x1=x1, N = length(x)))

y<- as.vector(trainSet$readmitted)
x<- factor(trainSet$number_diagnoses)
x2<-factor(trainSet$num_lab_procedures)
x3<-factor(trainSet$num_medications)
x4<-factor(trainSet$num_procedures)
x5<-factor(trainSet$number_emergency)

data<-data.frame(as.matrix(y,x,x2,x3,x4,x5))
data1<- with(data, list(x=x,y=y,x2=x2,x3=x3,x4=x4, x5=x5,N = length(x)))
#--------------------------------------------------------
#Model BUilding 

library(R2jags)

lm1_jags <- function(){
  for (i in 1:N){
    y[i] ~ dbern(mu[i])
    mu[i] <- 1 / (1 + exp(-p[i]))
    p[i] <- a+b[1]*x[i]+b[2]*x2[i]+b[3]*x3[i]+b[4]*x4+b[5]*x5
  }
  
  for (i in 1:5){
    
    b[i] ~ dnorm(0, 0.0001)
  }
  
  a ~ dnorm(0, 0.0001) # intercept
}
#-----------------------------------------------------------
#run chain
init_values <- function(){
  list(a = rnorm(1), b = rnorm(5,0,0.01))
}

params <- c("a", "b")

fit_lm1 <- jags(data = data1, inits = init_values, parameters.to.save = params, model.file = lm1_jags,
                n.chains = 3, n.iter = 20000, n.burnin = 1000, n.thin = 10, DIC = F)

fit_lm1
plot(fit_lm1)
#--------------------------------------------------------
traceplot(fit_lm1, mfrow = c(2, 2), ask = F)
lm1_mcmc <- as.mcmc(fit_lm1)
par(mar=c(1,1,1,1))
plot(lm1_mcmc)
#---------------------------------------------------------
# the result shows below, var 2 and 6 is not fit for this model

"Inference for Bugs model at "
", fit using jags,
3 chains, each with 10000 iterations (first 2000 discarded), n.thin = 10
n.sims = 2400 iterations saved
mu.vect sd.vect   2.5%    25%    50%    75%  97.5%  Rhat n.eff
a     -1.488   0.119 -1.722 -1.569 -1.492 -1.408 -1.257 1.000  2400
b[1]   0.106   0.013  0.079  0.097  0.106  0.114  0.132 1.001  2400
b[2]  -0.436   1.088 -2.889 -1.144 -0.099  0.458  0.966 2.806     4
b[3]   0.005   0.001  0.002  0.004  0.005  0.005  0.007 1.001  2400
b[4]   0.012   0.004  0.005  0.010  0.012  0.015  0.019 1.004  2100
b[5]  -0.100   0.016 -0.132 -0.111 -0.100 -0.089 -0.068 1.001  2400
b[6]   0.800   1.090 -0.615 -0.092  0.465  1.507  3.251 2.806     4
#attempt 2
     mu.vect sd.vect   2.5%    25%    50%    75%  97.5%  Rhat n.eff
a     -1.081   0.111 -1.290 -1.160 -1.085 -1.006 -0.857 1.007   340
b[1]   0.111   0.014  0.083  0.101  0.111  0.120  0.136 1.003   850
b[2]   0.004   0.001  0.002  0.003  0.004  0.005  0.007 1.002   990
b[3]   0.013   0.004  0.006  0.011  0.013  0.016  0.020 1.001  2400
b[4]  -0.109   0.016 -0.141 -0.120 -0.109 -0.097 -0.077 1.001  2400
For each parameter, n.eff is a crude measure of effective sample size,
and Rhat is the potential scale reduction factor (at convergence, Rhat=1).
#testing 
"

# data testing 
xt<- as.numeric(testSet$number_diagnoses)
x2t<-as.numeric(testSet$num_lab_procedures)
x3t<-as.numeric(testSet$num_medications)
x4t<-as.numeric(testSet$num_procedures)
ata<-data.frame(as.matrix(yt,xt,x2t,x3t,x4t))
data2<- with(data, list(xt=xt,yt=yt,x2t=x2t,x3t=x3t,x4t=x4t, N = length(x)))
  
lm1_mcmc_combi <- as.mcmc(rbind(lm1_mcmc[[1]], lm1_mcmc[[2]], lm1_mcmc[[3]]))
pred_mean_mean <- mean(lm1_mcmc_combi[, "a"]) + xt * mean(lm1_mcmc_combi[, "b[1]"])+x2t * mean(lm1_mcmc_combi[, "b[2]"])+x3t * mean(lm1_mcmc_combi[, "b[3]"])+x4t * mean(lm1_mcmc_combi[, "b[4]"])
newy<- 1 / (1 + exp(-pred_mean_mean))
roundedy<-as.matrix(round(newy))
as.matrix(roundedy)
yt<- as.matrix(testSet$readmitted)
colSums(roundedy==yt)/length(yt)*100
#approximatly 57%

