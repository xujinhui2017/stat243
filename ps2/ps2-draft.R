#1.1.1
## save letters in text format
chars <- sample(letters, 1e6, replace = TRUE)
write.table(chars, file = 'tmp1.csv', row.names = FALSE, quote = FALSE,
            col.names = FALSE)
system('ls -l tmp1.csv', intern = TRUE)

#1.1.2
chars <- paste(chars, collapse = '')
write.table(chars, file = 'tmp2.csv', row.names = FALSE, quote = FALSE,
            col.names = FALSE)
system('ls -l tmp2.csv', intern = TRUE)

#1.1.3
## save in binary format
nums <- rnorm(1e6)
save(nums, file = 'tmp3.Rda') 
system('ls -l tmp3.Rda', intern = TRUE)

#1.1.4
## save in text format
write.table(nums, file = 'tmp4.csv', row.names = FALSE, quote = FALSE, col.names = FALSE, sep = ',')
system('ls -l tmp4.csv', intern = TRUE)

#1.1.5
write.table(round(nums, 2), file = 'tmp5.csv', row.names = FALSE, quote = FALSE, col.names = FALSE, sep = ',')
system('ls -l tmp5.csv', intern = TRUE)

#1.2.1
chars <- sample(letters, 1e6, replace = TRUE) 
chars <- paste(chars, collapse = '') 
save(chars, file = 'tmp6.Rda')
system('ls -l tmp6.Rda', intern = TRUE)

#1.2.2
chars <- rep('a', 1e6)
chars <- paste(chars, collapse = '') 
save(chars, file = 'tmp7.Rda') 
system('ls -l tmp7.Rda', intern = TRUE)

#2.1
library(XML)
library(curl)
library(stringr)

get_html=function(name){
  url=gsub(" ","",paste("https://scholar.google.com/scholar?hl=en&q=",name))
  html=htmlParse(readLines(url))
  content=getNodeSet(html,'//h4[@class="gs_rt2"]')
  if(content[1]=="NULL") {
    result="Sorry,can not find scholar name"
    return(result)
  }
  else
  url_1=as.character(xmlAttrs(content[[1]][[1]]))
  urlfinal=gsub(" ","",paste("https://scholar.google.com/",url_1))
  userid=substr(url_1,str_locate(url_1,"user")[1,2]+2,str_locate(url_1,"&")[1,1]-1)
  result=list(html=htmlParse(readLines(urlfinal)),userID=userid)
  return(result)
}


#2.2
get_article_inf=function(html){
  content=getNodeSet(html,'//tr[@class]')
  article_title=c()
  authors=c()
  journal_information=c()
  citations_number=c()
  year_publication=c()
  for(i in 1:20){
    article_title[i]=xmlValue(content[[i]][[1]][[1]])
    authors[i]=xmlValue(content[[i]][[1]][[2]])
    journal_information[i]=xmlValue(content[[i]][[1]][[3]][[1]])
    year_publication[i]=gsub(",","",xmlValue(content[[i]][[1]][[3]][[2]]))
    citations_number[i]=xmlValue(content[[i]][[2]][[1]])
    data=cbind(article_title,authors,journal_information,year_publication,citations_number)
    data=as.data.frame(data)
  }
return(data)
}


#2.3
library(testthat)
test_that("wrong input",{
expect_equal(get_html('abc'),"Sorry,can not find scholar name")
expect_equal(get_html('Albert Einstein')$userID,"c6CJjYAAAAJ")
}
)

#2.4
get_all_article=function(name){
  article_title=c()
  authors=c()
  journal_information=c()
  citations_number=c()
  year_publication=c()
  url0=gsub(" ","",paste("https://scholar.google.com/scholar?hl=en&q=",name))
  html0=htmlParse(readLines(url0))
  content=getNodeSet(html0,'//h4[@class="gs_rt2"]')
  url_1=as.character(xmlAttrs(content[[1]][[1]]))
  urlfinal=gsub(" ","",paste("https://scholar.google.com/",url_1))
  length=20;j=0;
  while(length==20){
    url=gsub(" ","",paste(urlfinal,"&cstart=",20*j,"&pagesize=20"))
    html=htmlParse(readLines(url))
    content=getNodeSet(html,'//tr[@class]')
    length=length(content)
    for(i in 1:length){
      article_title[i+20*j]=xmlValue(content[[i]][[1]][[1]])
      authors[i+20*j]=xmlValue(content[[i]][[1]][[2]])
      journal_information[i+20*j]=xmlValue(content[[i]][[1]][[3]][[1]])
      year_publication[i+20*j]=gsub(",","",xmlValue(content[[i]][[1]][[3]][[2]]))
      citations_number[i+20*j]=xmlValue(content[[i]][[2]][[1]])
      }
  j=j+1
  }
data=cbind(article_title,authors,journal_information,year_publication,citations_number)
data=as.data.frame(data)
return(data)
}
