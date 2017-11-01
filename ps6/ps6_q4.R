require(parallel) # one of the core R packages
require(doParallel)
library(foreach)
library(readr)
nCores <- as.integer(Sys.getenv("SLURM_CPUS_ON_NODE"))
registerDoParallel(nCores) 

nSub <- 240 # do only first 30 for illustration
setwd('/global/scratch/paciorek/wikistats_full/dated_for_R')
dir <- list.files(pattern = "part*")

find <- function(dir) {
  table = read_delim(dir, delim = " ", col_names = F)
  data = as.data.frame(table)
  row_number = grep("Barack_Obama", data[,4])
  find_result <- data[row_number,]
  return(find_result)
}

result_final <- foreach(i = 1:nSub,
                  .combine = rbind
) %dopar% {
 # cat('Starting ', dir[i], '.\n', sep = '')
  result_part <- find(dir[i])
 # cat('Finishing ', dir[i], '.\n', sep = '')
  result_part
}
dim<-dim(result_final)
head<-head(result_final,10)
write.table(result_final,file='/global/home/users/jinhui_xu/result.txt')
write.table(dim,file='/global/home/users/jinhui_xu/dim.txt')
write.table(head,file='/global/home/users/jinhui_xu/head.txt')