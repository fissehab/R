
# setwd("C:/Fish/classes/spring_2015/R/programming_assignments/1")


pollutantmean <- function(directory,pollutant,id = 1:332) {
  

if (max(id) <=332){
  
  ID <-id
  
  for (i in seq(1,length(id))){
    
  if (id[i]<10){ ID[i]<-paste(0,0,id[i],".csv",sep='')}
  
  if (id[i]<100 &id[i]>=10){ID[i]<-paste(0,id[i],".csv",sep='')}
  
  else if(id[i]>=100) {ID[i]<-paste(id[i],".csv",sep='')}}
  
  d<-c()
  
  for (i in seq(1,length(id))){
   x<- read.table(paste(directory,'/',ID[i],sep=''),header=TRUE,sep=",")
   
   d<-rbind(d,x)
   
  }
 
result<-mean(d[,pollutant],na.rm=T)



}

else{
 result<- show('ID number out of bounds, mad ID =332')
}

result
}

