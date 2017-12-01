ez<-function(mu,sigma,k){
  result<-c()
  k_star<-(k-mu)/sigma
  r_kstar<-dnorm(k_star)/(1-pnorm(k_star))
  result[1]<-mu+sigma*r_kstar
  result[2]<-result[1]^2+sigma^2*(1+k_star*r_kstar-r_kstar^2)
  return(result)
}

###get censored y
censored_y<-function(x,y,proportion,beta){
  b<-rep(0,100);a<-rep(0,100)
  tau<-sort(yComplete)[n-n*proportion+1]
  for(i in 1:n){
    if (y[i]>=tau) {
      y[i]<-ez(beta[1]+beta[2]*x[i],sqrt(beta[3]),tau)[1]
      b[i]<-y[i]
      a[i]<-ez(beta[1]+beta[2]*x[i],sqrt(beta[3]),tau)[2]
    }
  }
  data<-cbind(y,a,b)
  return(data)
}


em<-function(proportion){
  
###get x and y
  set.seed(3)
  n <- 100; beta0 <- 1; beta1 <- 2; sigma2 <- 6
  x <- runif(n)
  b<-rep(0,100);a<-rep(0,100)
  itera<-0;beta_0<-c();beta_1<-c();error<-100
  yComplete <- rnorm(n, beta0 + beta1*x, sqrt(sigma2))
  
########calculate the initial beta  
  tau<-sort(yComplete)[n-n*proportion+1]
  y0<-c();x0<-c()
  y0<-yComplete[which(yComplete<tau)]
  x0<-x[which(yComplete<tau)]
  
  lm0<-lm(y0~x0)
  beta_0[1]<-summary(lm0)$coef[1]
  beta_0[2]<-summary(lm0)$coef[2]
  beta_0[3]<-sum(lm0$residuals^2)/length(y0)
  
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
  return(c(beta_0,error))
}


f<-function(input){
  beta00<-input[1];beta01<-input[2];sigma_square<-input[3]
  proportion=0.2
  set.seed(1)
  n <- 100; beta0 <- 1; beta1 <- 2; sigma2 <- 6
  x <- runif(n); yComplete <- rnorm(n, beta0 + beta1*x, sqrt(sigma2))
  
  tau<-sort(yComplete)[n-n*proportion+1]
  y_censored<-yComplete[which(yComplete>=tau)]
  x_censored<-x[which(yComplete>=tau)]
  y_complete<-yComplete[which(yComplete<tau)]
  x_complete<-x[which(yComplete<tau)]
  
  l_censored<-log(n*proportion-sum(pnorm((tau-beta00-beta01*x_censored)/sqrt(sigma_square))))
  l_complete=-n*(1-proportion)*log(2*pi*sigma_square)-1/(2*sigma_square)*crossprod(y_complete-beta00-beta01*x_complete)
  
  return(-(l_censored+l_complete))
}
optim(par=c(1,1,1),fn=f,lower=c(0.1,0.1,0.1),method = 'BFGS')
