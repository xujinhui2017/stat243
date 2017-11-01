library(parallel)
library(doParallel)
library(foreach)

nCores <- as.integer(Sys.getenv("SLURM_CPUS_ON_NODE"))
registerDoParallel(nCores)

nSub <- 1

results <- foreach(i = 0:nSub, .packages = c("readr", "stringr", "dplyr"), 
                   .combine = rbind, 
                   .verbose = TRUE) %dopar%{
  filedir <- paste("/global/scratch/paciorek/wikistats_full/dated_for_R/part-", 
                   str_pad(i ,width = 5, side = "left", pad = "0"), sep = "")
  data <- readr::read_delim(filedir, delim = " ", col_names = FALSE)
  index <- grep("Barack_Obama", data$X4)
  df <- as.data.frame(data[index, ])
  df
                   }
dim=dim(results)
write.table(dim,file='/global/home/users/jinhui_xu/dimresults.txt')