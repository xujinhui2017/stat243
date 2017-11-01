###############
###Question2###
library(RSQLite)
drv<-dbDriver('SQLite')
dir<-'~/stat243/stat243-fall-2017/myps/stat243/ps6/'
dbFilename <- 'stackoverflow-2016.db'
db<-dbConnect(drv,dbname=file.path(dir,dbFilename))


####################################
dbGetQuery(db, "create view py_question 
           as select 
           U.displayname,T.tag,Q.questionid,Q.ownerid 
           from questions Q join questions_tags T on Q.questionid = T.questionid 
           join users U on Q.ownerid = U.userid 
           where T.tag = 'python' ")

result1<-dbGetQuery(db,"select distinct Q.ownerid
                   from questions Q
                    join questions_tags T on Q.questionid = T.questionid 
                    join users U on Q.ownerid = U.userid  
                    where T.tag ='r' and ownerid not in (select ownerid from py_question)")

#####################################
result<-dbGetQuery(db, "select distinct Q.ownerid 
           from questions Q 
           join questions_tags T on Q.questionid = T.questionid 
           join users U on Q.ownerid = U.userid 
           where T.tag = 'r' and ownerid not in 
           (select ownerid 
           from questions Q
           join questions_tags T on Q.questionid = T.questionid 
           join users U on Q.ownerid = U.userid 
           where T.tag = 'python' ) ")


########################
#######Question3########

ssh jinhui_xu@hpc.brc.berkeley.edu

###basic set
srun -A ic_stat243 -p savio2 --nodes=4 -t 1:00:00 --pty bash
module load java spark
source /global/home/groups/allhands/bin/spark_helper.sh
spark-start
env | grep SPARK
module unload python
pyspark --master $SPARK_URL --executor-memory 60G

###write the direction
dir = '/global/scratch/paciorek/wikistats_full/dated'

### read data and do some checks ###
lines = sc.textFile(dir)

######################
def find(line, regex = "stock", language = 'en'):
    vals = line.split(' ')
    if len(vals) < 6:
         return(False)
    tmp = re.search(regex, vals[3])
    if tmp is None or (language != None and vals[2] != language):
         return(False)
    else:
         return(True)

########################
import re
from operator import add
def stratify(line):
  vals = line.split(' ')
  return(vals[0] + '-' + vals[1] + '-' + vals[2], int(vals[4]))

stock= lines.filter(find).repartition(480)
counts= stock.map(stratify).reduceByKey(add)

########################
def transform(vals):
  key = vals[0].split('-')
  return(",".join((key[0], key[1], key[2], str(vals[1]))))

########################
outputDir = '/global/home/users/jinhui_xu/q3'
counts.map(transform).repartition(1).saveAsTextFile(outputDir)

#######################
scp jinhui_xu@dtn.brc.berkeley.edu:/global/home/users/jinhui_xu/q3/part-00000 ~/stat243/stat243-fall-2017/myps/stat243/ps6

setwd('~/stat243/stat243-fall-2017')
stock<-read.csv('part-00000',header=TRUE)
stock_day<-aggregate(stock[,4],by=list(stock[,1]),FUN=sum)
plot(stock_day[,2],xlab='date',ylab='number of hits')
lines(stock_day[,2])
title(main='stock hits from Oct. to Dec.')

########################
#######Question4########
sbatch ps6_q4.sh
squeue -A ic_stat243
a)
scp jinhui_xu@dtn.brc.berkeley.edu:~/dim.txt ~/stat243/stat243-fall-2017/myps/stat243/ps6
scp jinhui_xu@dtn.brc.berkeley.edu:~/head.txt ~/stat243/stat243-fall-2017/myps/stat243/ps6
read.csv('~/stat243/stat243-fall-2017/myps/stat243/ps6/dim.txt')
read.csv('~/stat243/stat243-fall-2017/myps/stat243/ps6/head.txt')

b)
out<-readLines('ps6_q4.out')
out[c(length(out)-1,length(out))]
