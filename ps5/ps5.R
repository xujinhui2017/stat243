###############
###stat243-ps5
##############

###question2
2^53-1
2^53
2^53+1
2^53+2
2^54+1
2^54+2
2^54+3
2^54+4

###question3
#a
library(data.table)
library(microbenchmark)
compare_copytime=function(x,y){
  timex=microbenchmark(copy(x))$time      #get 100 data of time
  timey=microbenchmark(copy(y))$time
  if (mean(timex)>mean(timey)) print("copying the first one takes more time")
  else print('copying the second one cost more time')
  boxplot(log(timex),log(timey))
}
numvec<-rnorm(1e7)
intvec<-as.integer(numvec)
compare_copytime(intvec,numvec)

#b
compare_subtime=function(x,y){
  timex=microbenchmark(xsub<-x[1:5*1e6])$time       #get 100 data of time
  timey=microbenchmark(ysub<-y[1:5*1e6])$time
  if (mean(timex)>mean(timey)) print("taking a subset of the first one takes more time")
  else print('taking a subset of the second one cost more time')
  boxplot(log(timex),log(timey))
}
compare_subtime(intvec,numvec)

