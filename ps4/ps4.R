##################################################
#######ps4-Q1
##################################################
###(a)
x <- 1:10
f <- function(input){
  print(.Internal(inspect(input)))       #check the address of input
  data <- input
  print(.Internal(inspect(data)))        #check the address of data
  g <- function(param) return(param * data) 
  return(g)
}
data<-100
myFun <- f(x)

###(b)
x <- rnorm(1e5)
f <- function(input){
  data <- input
  g <- function(param) return(param * data) 
  return(g)
}
myFun <- f(x)
object.size(x)
length(serialize(myFun,NULL))

###(c)
x <- 1:10
f <- function(data){
  g <- function(param) return(param * data) 
  return(g)
}
myFun <- f(x) 
rm(x)
data <- 100 
myFun(3)
ls(envir=environment(myFun))

###(d)
x <- 1:10
f <- function(data){
  force(data)
  g <- function(param) return(param * data) 
  return(g)
}
myFun <- f(x) 
rm(x)
data <- 100 
myFun(3)
##################################################
#######ps4-Q2
##################################################
###(a)
list1=list(a=rnorm(1e5),b=rnorm(1e5))
.Internal(inspect(list1))
list1[[1]][1]<-100
.Internal(inspect(list2))

###(b)
list2=list(a=rnorm(1e5),b=rnorm(1e5))
list2_cp=list2
.Internal(inspect((list2)))
.Internal(inspect((list2_cp)))
#the address of list2_cp is same with that of list2
list2_cp[[1]][1]<-100
.Internal(inspect(list2_cp))           
#after change, we can find only the address of relevant vector changes

###(c)
list2=list(a=list(rnorm(1e7),rnorm(1e7)),b=list(rnorm(1e7),rnorm(1e7)))
list2_cp=list2
.Internal(inspect(list2))
.Internal(inspect(list2_cp))
gc(reset=TRUE)
list2_cp[[1]][1]<-100
gc()
.Internal(inspect(list2_cp))

###(d)
gc()
tmp <- list()
x <- rnorm(1e7)
tmp[[1]] <- x
tmp[[2]] <- x 
.Internal(inspect(tmp))
object.size(tmp)
gc()


##################################################
#######ps4-Q3
##################################################
ll <- function(Theta, A) {
  sum.ind <- which(A==1, arr.ind=T)
  logLik <- sum(log(Theta[sum.ind])) - sum(Theta)
  return(logLik)
}
#######################################
##original code########################
oneUpdate <- function(A, n, K, theta.old, thresh = 0.1) { 
  theta.old1 <- theta.old
  Theta.old <- theta.old %*% t(theta.old)
  L.old <- ll(Theta.old, A)
  q <- array(0, dim = c(n, n, K))
  for (i in 1:n) { 
    for (j in 1:n) {
      for (z in 1:K) {
        if (theta.old[i, z]*theta.old[j, z] == 0){
          q[i, j, z] <- 0 } else {
            q[i, j, z] <- theta.old[i, z]*theta.old[j, z] /Theta.old[i, j]
          } 
      }
    } 
  }
  theta.new <- theta.old 
  for (z in 1:K) {
    theta.new[,z] <- rowSums(A*q[,,z])/sqrt(sum(A*q[,,z])) 
  }
  Theta.new <- theta.new %*% t(theta.new) 
  L.new <- ll(Theta.new, A)
  converge.check <- abs(L.new - L.old) < thresh 
  theta.new <- theta.new/rowSums(theta.new) 
  return(list(theta = theta.new, loglik = L.new,converged = converge.check))
} 

#######################################
##revised code#########################
oneUpdate_new <- function(A, n, K, theta.old, thresh = 0.1) { 
  theta.old1 <- theta.old
  Theta.old <- theta.old %*% t(theta.old)
  L.old <- ll(Theta.old, A)
  q <- array(0, dim = c(n, n, K))
  for (z in 1:K) {
    q[ , , z] <- theta.old[, z]%*%t(theta.old[ , z]) /Theta.old
  }
  theta.new <- theta.old 
  for (z in 1:K) {
    theta.new[,z] <- rowSums(A*q[,,z])/sqrt(sum(A*q[,,z])) 
  }
  Theta.new <- theta.new %*% t(theta.new) 
  L.new <- ll(Theta.new, A)
  converge.check <- abs(L.new - L.old) < thresh 
  theta.new <- theta.new/rowSums(theta.new) 
  return(list(theta = theta.new, loglik = L.new,converged = converge.check))
}   
# initialize the parameters at random starting values
temp <- matrix(runif(n*K), n, K) 
theta.init <- temp/rowSums(temp)
# do single update
out <- oneUpdate(A, n, K, theta.init)



##################################################
#######ps4-Q4
##################################################
PIKK <- function(x, k) {
  x[sort(runif(length(x)), index.return = TRUE)$ix[1:k]]
}
FYKD <- function(x, k) { 
  n <- length(x)
  for(i in 1:n) {
    j = sample(i:n, 1)
    tmp <- x[i]
    x[i] <- x[j]
    x[j] <- tmp
  }
  return(x[1:k])
}