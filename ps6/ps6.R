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







########################
#######Question4########


