#############
#######ps8###

###########
#######Q1_a
beta<-3
alpha<-2
lambda<-1
x<-seq(0.1,20,0.05)
pareto<-dpareto(x,alpha,beta)
exp<-dexp(x,lambda)
plot(x+2,exp,type='l',lty=2,ylim=range(0,1.5),xlab='x',ylab='p')
lines(x,pareto)
legend('topright',lty=c(1,2),legend=c('pareto','exp'))

#######Q1_b
beta<-3
alpha<-2
lambda<-1
m<-1000
sample_pareto<-rpareto(m,scale=2,shape=3)
hfg_2<-sample_pareto^5*exp(-sample_pareto+2)/24
weight_2<-sample_pareto^4*exp(-sample_pareto+2)/24
mean(hfg_2)
var(hfg_2)
########Q1_c
beta<-3
alpha<-2
lambda<-1
m<-1000
sample_exp<-rexp(m)+2
hfg_3<-24*sample_exp^(-3)*exp(sample_exp-2)
weight_3<-24*sample_exp^(-4)*exp(sample_exp-2)
mean(hfg_3)
var(hfg_3)

##############hist of weight
par(mfrow=c(1,2))
hist(weight_2)
hist(weight_3)

###############
#############Q2
theta <- function(x1,x2) atan2(x2, x1)/(2*pi)

f <- function(x) {
  f1 <- 10*(x[3] - 10*theta(x[1],x[2]))
  f2 <- 10*(sqrt(x[1]^2 + x[2]^2) - 1)
  f3 <- x[3]
  return(f1^2 + f2^2 + f3^2)
}

####fix x3 and calculate the value of f(c(x1,x2,mean(x3)))
x1<-seq(-20,20,0.2)
x2<-seq(-20,20,0.2)
x3<-seq(-20,20,0.2)
f_value12<-matrix(nrow=201,ncol=201)
for(i in 1:201){
  for(j in 1:201){
    f_value12[i,j]<-f(c(x1[i],x2[j],1))
  }
}
####fix x2
f_value13<-matrix(nrow=201,ncol=201)
for(i in 1:201){
  for(j in 1:201){
    f_value13[i,j]<-f(c(x1[i],1,x3[j]))
  }
}

####fix x1
f_value23<-matrix(nrow=201,ncol=201)
for(i in 1:201){
  for(j in 1:201){
    f_value23[i,j]<-f(c(1,x2[i],x3[j]))
  }
}

########compare the plot
par(mfrow=c(1,3))
image(x1,x2,f_value12)
contour(x1,x2,f_value12,add=TRUE,drawlabels = FALSE)
image(x1,x3,f_value13)
contour(x1,x3,f_value13,add=TRUE,drawlabels = FALSE)
image(x2,x3,f_value23)
contour(x2,x3,f_value23,add=TRUE,drawlabels = FALSE)

############
start<-c(100,-10,-10)
op<-optim(par=start,fn=f)



######Q3

########Q3_c
######## write ez function to get the expectation of Z and Z^2;
######## which in my algorithm in (a) equals to Bi and Ai
ez<-function(mu,sigma,k){
  result<-c()
  
  ###calculate the Ez and Ez^2
  k_star<-(k-mu)/sigma
  r_kstar<-dnorm(k_star)/(1-pnorm(k_star))
  
  ###store the value in a vector
  result[1]<-mu+sigma*r_kstar
  result[2]<-result[1]^2+sigma^2*(1+k_star*r_kstar-r_kstar^2)
  return(result)
}

###write the function to get censored y according to the given proportion
censored_y<-function(x,y,proportion,beta){
  b<-rep(0,100);a<-rep(0,100)
  tau<-sort(yComplete)[n-n*proportion+1]
  
  ###find the y needed to be censored and then calaulate the corresponding Bi and Ai
  for(i in 1:n){
    if (y[i]>=tau) {
      y[i]<-ez(beta[1]+beta[2]*x[i],sqrt(beta[3]),tau)[1]
      b[i]<-y[i]
      a[i]<-ez(beta[1]+beta[2]*x[i],sqrt(beta[3]),tau)[2]
    }
  }
  ###store the result in a matrix
  data<-cbind(y,a,b)
  return(data)
}

####write the final function to get estimated theta
em<-function(proportion){
  
  ###get x and y
  set.seed(1)
  n <- 100; beta0 <- 1; beta1 <- 2; sigma2 <- 6
  x <- runif(n)
  b<-rep(0,100);a<-rep(0,100)
  yComplete <- rnorm(n, beta0 + beta1*x, sqrt(sigma2))
  
  ###initiate some settings
  itera<-0;beta_0<-c();beta_1<-c();error<-100
  
  ########to calculate the initial beta, first calculate the complete x and y
  tau<-sort(yComplete)[n-n*proportion+1]
  y0<-c();x0<-c()
  y0<-yComplete[which(yComplete<tau)]
  x0<-x[which(yComplete<tau)]
  
  ######calculate the beta_0
  lm0<-lm(y0~x0)
  beta_0[1]<-summary(lm0)$coef[1]
  beta_0[2]<-summary(lm0)$coef[2]
  beta_0[3]<-sum(lm0$residuals^2)/length(y0)
  
  ###do iteration until the number of iteration is over 1000 times or the error is smaller than 0.00001
  while(error>=0.00001&itera<=1000){
    itera<-itera+1
    yab<-censored_y(x,yComplete,proportion,beta_0) 
    y<-yab[,1]
    a<-yab[,2]
    b<-yab[,3]
    lm<-lm(y~x)
    
    ###get a new beta
    beta_1[1]<-summary(lm)$coef[1]
    beta_1[2]<-summary(lm)$coef[2]
    beta_1[3]<-(sum(lm$residuals^2)-crossprod(b)+sum(a))/n
    
    ####calculate the error between new beta and old beta
    error<-crossprod(beta_0-beta_1)
    beta_0<-beta_1
  }
  return(c(beta_0,error,itera))
}
em(0.2);em(0.8)

#######Q3_d
###function to calculate the log likelihood value
f<-function(input,proportion=0.2){
  beta00<-input[1];beta01<-input[2];sigma_square<-input[3]
  
  set.seed(1)
  n <- 100; beta0 <- 1; beta1 <- 2; sigma2 <- 6
  x <- runif(n); yComplete <- rnorm(n, beta0 + beta1*x, sqrt(sigma2))
  
  ###get two parts of data(x,y)
  tau<-sort(yComplete)[n-n*proportion+1]
  y_censored<-yComplete[which(yComplete>=tau)]
  x_censored<-x[which(yComplete>=tau)]
  y_complete<-yComplete[which(yComplete<tau)]
  x_complete<-x[which(yComplete<tau)]
  
  ###calculate the log likelihood value of censored data and complete data
  l_censored<-sum(log(1-pnorm((tau-beta00-beta01*x_censored)/sqrt(sigma_square))))
  l_complete=-n*(1-proportion)/2*log(2*pi*sigma_square)-1/(2*sigma_square)*
    crossprod(y_complete-beta00-beta01*x_complete)
  
  ###return the negative of sum of two log likelihood value
  return(-(l_censored+l_complete))
  
}

###get the result and compare with my EM algorithm
op<-optim(par=c(1,1,1),fn=f,lower=c(0,0,0),method = 'BFGS')
op$par;op$counts
em(0.2)[1:3];em(0.2)[5]




###########################################################
####try the situation of proportion equals to 0.8
f<-function(input,proportion=0.8){
  beta00<-input[1];beta01<-input[2];sigma_square<-input[3]
  
  set.seed(1)
  n <- 100; beta0 <- 1; beta1 <- 2; sigma2 <- 6
  x <- runif(n); yComplete <- rnorm(n, beta0 + beta1*x, sqrt(sigma2))
  
  ###get two parts of data(x,y)
  tau<-sort(yComplete)[n-n*proportion+1]
  y_censored<-yComplete[which(yComplete>=tau)]
  x_censored<-x[which(yComplete>=tau)]
  y_complete<-yComplete[which(yComplete<tau)]
  x_complete<-x[which(yComplete<tau)]
  
  ###calculate the log likelihood value of censored data and complete data
  l_censored<-sum(log(1-pnorm((tau-beta00-beta01*x_censored)/sqrt(sigma_square))))
  l_complete=-n*(1-proportion)/2*log(2*pi*sigma_square)-1/(2*sigma_square)*
    crossprod(y_complete-beta00-beta01*x_complete)
  
  ###return the negative of sum of two log likelihood value
  return(-(l_censored+l_complete))
  
}

###get the result and compare with my EM algorithm
op<-optim(par=c(1,1,1),fn=f,lower=c(0,0,0),method = 'BFGS')
op$par;op$counts
em(0.8)[1:3];em(0.8)[5]

