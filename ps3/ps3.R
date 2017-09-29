#stat243-PS3   JINHUI XU  
#2017 fall

##Question2 a
library(stringr)
library(ggplot2)
text=readLines("http://www.gutenberg.org/cache/epub/100/pg100.txt");
#note that year is the start of a play and also the end of last play
year_rownumber=grep('^[[:digit:]]{4}',text)
year_play=text[year_rownumber[2:(length(year_rownumber)-1)]]

#then store play in playlist$playi
playlist=list();
for(i in 1:36) eval(parse(text=paste(paste('playlist$play',i,sep=''),'=text[(year_rownumber[i+1]):(year_rownumber[i+2]-1)]')))



#################################################################################################
##Question2 b
#the second content is exactly the title of play.
title=c();
for(i in 1:36) title[i]=playlist[[i]][which(playlist[[i]]!="")[2]]

#write a functiom that can read the number of act and scene
count_number=function(str_target,searchlist){
  content_list=list()
  content_number=c()
  for(i in 1:length(searchlist)){
    str_locate=c()
    str_locate=grep(str_target,searchlist[[i]])
    content_list[[i]]=searchlist[[i]][str_locate]
    content_number[i]=length(str_locate)
  }
  list=list(content_list,content_number)
  return(list)
}
scene_list=list()
act_list=list()
scene_number=c()
act_number()
scene_str="[S][Cc][Ee][Nn][Ee][[:blank:]]"
act_str="A[Cc][Tt] " 
scene_list=count_number(scene_str,playlist)[[1]]
scene_number=count_number(scene_str,playlist)[[2]]
act_list=count_number(act_str,playlist)[[1]]
#act_number is a little bit special 
act_number=c();for(i in 1: 36) act_number[i]=length(unique(str_extract_all(act_list[[i]],'A[Cc][Tt][[:blank:]]?[[:alpha:]]*')))

#after get the data of year,tile,act_number,scene_number, combine them 
metadata=cbind(year_play,title,act_number,scene_number)

# get the body of play
body_list=list();
for(i in 1:36) body_list[[i]]=playlist[[i]][grep("^[[:blank:]]*[Ss][Cc][Ee][Nn][Ee][[:punct:]]",playlist[[i]]):length(playlist[[i]])]
result=list()
for(i in 1:36) {
  play_result=list()
  play_result[[1]]=year_play[i]
  play_result[[2]]=title[i]
  play_result[[3]]=act_number[i]
  play_result[[4]]=scene_number[i]
  play_result[[5]]=body_list[[i]]
  names(play_result)=c('year_play','title','act_number','scene_number','body_list')
  result[[i]]=play_result
}




###########################################################################################################
##Question2 c

# update the body_list,delete some useless information
body_update=body_list
for(i in 1:36){
  n=grep("[S][Cc][Ee][Nn][Ee][[:blank:]]",body_list[[i]])
  for(j in 1:length(n))
  body_update[[i]][n[j]:(n[j]+4)]='null'
}

#
speaker_first_content=list()
name=list()
speaker_locate=list()
speaker_number=c()
for(i in 1:36){
  body_update[[i]]=str_replace_all(body_update[[i]],'((Exit|Exeunt|Enter|Re-enter)([[:graph:][:blank:]])*)|(\\[([[:graph:][:blank:]])+\\])','')
  speaker_locate[[i]]=grep('^[[:blank:]]{0,4}[[:upper:]]+[[:lower:]]*([[:blank:]]*[[:upper:]]+[[:lower:]]*)*[.][[:blank:]]*[[:upper:]][[:blank:]]*[[:punct:]]*([[:blank:]][[:upper:]])?[[:blank:]]*[[:punct:]]?[[:lower:]]',body_update[[i]])
  speaker_first_content[[i]]=body_update[[i]][speaker_locate[[i]]]
  #speaker_first_content[[i]]=str_replace_all(speaker_first_content[[i]],'Exit([[:graph:][:blank:]])*','')
  #speaker_first_content[[i]]=str_replace_all(speaker_first_content[[i]],'((Exit|Exeunt|Enter|Re-enter)([[:graph:][:blank:]])*)|(\\[([[:graph:][:blank:]])+\\])','')
  name[[i]]=unique(str_replace_all(str_extract(speaker_first_content[[i]],"^[[:blank:]]{0,4}[[:upper:]]+[[:lower:]]*([[:blank:]]*[[:upper:]]*[[:lower:]]*){0,2}"),"^[[:blank:]]*",''))
  if((length(grep('[[:lower:]]+',name[[i]]))/length(name[[i]]))<0.3&&(length(grep('[[:lower:]]+',name[[i]]))/length(name[[i]]))>0) name[[i]]=name[[i]][-grep('[[:lower:]]+',name[[i]])]
  speaker_number[i]=length(name[[i]])
}


#
chunk_name_play=list();for(r in 1:36){
  chunk=list();for(k in 1:length(name[[r]])){
    l=c()
    l=grep(paste('^[[:blank:]]*',name[[r]][k],sep=''),body_update[[r]])
    rownumber=c();
       for(i in 1:length(l)){
          j=0;rownumber[i]=1;
          while((length(grep('^[[:blank:]]{0,4}[[:upper:]]+[[:lower:]]*([[:blank:]]*[[:upper:]]+[[:lower:]]*)*[.]',body_update[[r]][(l[i]+j+1)]))==0)&&(length(grep("[[:lower:]]",body_update[[r]][(l[i]+j+1)]))==1)){
          j=j+1
          rownumber[i]=rownumber[i]+1
          }
       }
    namei=list()
    for(i in 1:length(l)){ 
     namei[[i]]=body_update[[r]][l[i]:(l[i]+rownumber[i]-1)]
     namei[[i]]=str_replace(namei[[i]],name[[r]][k],'')
     namei[[i]]=str_replace(namei[[i]],'[[:blank:]]{2,}','')
     namei[[i]]=str_replace(namei[[i]],'\\. ','')
    }
  chunk[[k]]=namei
  }
chunk_name_play[[r]]=chunk
}

################################################################################################
#d)
speaker_number;
chunk_number=c(rep(0,36))
sentences_number=c(rep(0,36))
words_number=c(rep(0,36))
unique_words=c()
word_extract=list()
sentences_extract=list()
for(i in 1:36){
  unique_words[i]=length(unique(unlist(str_extract_all(chunk_name_play[[i]],"[[:alpha:]]+[']?[[:alpha:]]*"))))-1
  word_extract=str_extract_all(chunk_name_play[[i]],'[[:graph:]]+')
  sentences_extract=str_extract_all(chunk_name_play[[i]],'[.!]')
  for (j in 1:speaker_number[i]){
    chunk_number[i]=chunk_number[i]+length(chunk_name_play[[i]][[j]])
    sentences_number[i]=sentences_number[i]+length(sentences_extract[[j]])
    words_number[i]=words_number[i]+length(word_extract[[j]])

  }
}
words_perchunk=words_number/chunk_number
data_final=cbind(act_number,scene_number,speaker_number,chunk_number,sentences_number,words_perchunk,unique_words)

#######################################################################################################
##Q2 e)

for(i in 1:7) data_final[,i]=as.numeric(data_final[,i])
year_sort=sort(unique(year_play))
n=length(year_sort)
data_sort_byyear=c()
for(i in 1:n){
  temp=data_final[which(year_play==year_sort[i]),]
  if(length(dim(temp))>0){
  year_mean=apply(temp,2,mean)
  data_sort_byyear=rbind(data_sort_byyear,year_mean)
  }
  else
  data_sort_byyear=rbind(data_sort_byyear,temp)
  rownames(data_sort_byyear)[i]=i
}
data_sort_byyear=cbind(as.numeric(year_sort),data_sort_byyear)
colnames(data_sort_byyear)[1]='year'
data_sort_byyear=round(data_sort_byyear,2)

#########################################
#plot
par(mfrow=c(2,3))
for(i in 3:8) plot(data_sort_byyear[,c(1,i)])
qplot(data_sort_byyear[,1],data_sort_byyear[,8],geom=c('point','smooth'),method='lm',xlab = 'year',ylab = 'number of unique words')

####################################################################################
#Question 3
####################################################################################
#setClass('play',
#         representation(
#           year='numeric',                   #year published of the play
#           title='character',                #title of the play
#           text='character',                 #original content of the play
#           body='character',                 #body of the play
#           chunk='character',                #all spoken chunks in the play
#           acts_number='numeric',            #number of acts
#           scenes_number='numeric',          #number of scenes
#           speakers_number='numeric',        #number of unique speakers
#           chunks_number='numeric',          #number of chunks
#           sentences_number='numeric',       #number of sentences
#           words_number='numeric',           #number of total words
#           unique_words='numeric',           #number of unique words
#           words_perchunk='numeric'          #number of words per chunk
#         ),
#         prototype(
#           ...                       #we can set default of slots except text
#         )
#)
#Methods:
#get_basic_information #input text of the play and output c(year,title,body,acts_number,scenes_number)  
#get_chunk #input body of the play and output chunk(all spoken chunks in the play)
#get_chunk_information #input chunk and output c(speakers_number,chunks_number,sentences_number,words_number,unique_words,words_perchunk)
  