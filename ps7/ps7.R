##########Q4

a)
QR_beta<-backsolve(R, backsolve(R, t(X)%*%Y+t(A)%*%
                                    backsolve(AR,backsolve(AR,
                                                             -A%*%backsolve(R, t(Q)%*%Y)+b,transpose=T)),
                                  transpose=T))
b)

#define the function which uses QR decomposition 
QR_beta<-function(A,X,Y,b){ 
  
  #get Q, R of in QR decompodition of X
  X_R<-qr.R(qr(X))
  X_Q<-qr.Q(qr(X))
  
  #get R in QR decomposition of AR^{-1}
  AR_R<-qr.R(qr(t(A%*%solve(X_R))))
  
  #calculate beta hat in the same way in question (a)
  beta<-backsolve(X_R, backsolve(X_R, t(X)%*%Y+t(A)%*%
                                   backsolve(AR_R,backsolve(AR_R,
                                                            -A%*%backsolve(X_R, t(X_Q)%*%Y)+b,transpose=T)),
                                 transpose=T))
  return(beta) 
}

#define the function of methods which uses solve to calculate the inverse of matrics
solve_beta<-function(A,X,Y,b){
  d<-t(X)%*%Y
  solve(crossprod(X))%*%d+
    solve(crossprod(X))%*%t(A)%*%
    solve(A%*%solve(crossprod(X))%*%t(A))%*%(-A%*%(solve(crossprod(X))%*%d)+b)
}


Then I give an example to examine the efficency of QR decomposition

#set basic parameters
m<-100
n<-1000
p<-1000
A<-matrix(rnorm(m*p),m)
X<-matrix(rnorm(n*p),n)
Y<-rnorm(n)
b<-rnorm(m)
d<-t(X)%*%Y

#compare the result of two methods
solve_beta(A,X,Y,b)[1:5]
QR_beta(A,X,Y,b)[1:5]

#compare the time used to run the two functions
system.time(solve_beta(A,X,Y,b))
system.time(QR_beta(A,X,Y,b))





################Q6
#create eigenvectors
set.seed(1)
Z<-matrix(rnorm(10000),100)
true_eigenvec<-eigen(crossprod(Z))$vectors

#create eigenvalues


non_positive<-c(rep(0,100))
con_num<-c()
eigenval<-matrix(nrow=100,ncol=100)
for(i in 1:100)
  eigenval[i,]<-runif(100,0,i^8)
for(i in 1:100){
  Sigma<-diag(eigenval[i,])
  con_num[i]<-max(eigenval[i,])/min(eigenval[i,])
  true_eigenval<-diag(Sigma)
  R_eigenval<-eigen(true_eigenvec%*%Sigma%*%t(true_eigenvec))$values
  if(sum(R_eigenval>0)<100)
    non_positive[i]<-1
}