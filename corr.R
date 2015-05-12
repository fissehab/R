
corr<- function(directory, threshold = 0) {
  
  id<-1:332
  
  ID <-id
  
  for (i in 1:332){
    
  if (id[i]<10){ ID[i]<-paste(0,0,id[i],".csv",sep='')}
  
  if (id[i]<100 &id[i]>=10){ID[i]<-paste(0,id[i],".csv",sep='')}
  
  else if(id[i]>=100) {ID[i]<-paste(id[i],".csv",sep='')}}
  
 
  d <-c()
  
  m<-complete("specdata", 1:332)
  
  
  for (i in 1:332){
    if (m[i,2]> threshold){
   x<- read.table(paste(directory,'/',ID[i],sep=''),header=TRUE,sep=",")
   
   y<-x[(!is.na(x[,2]) & !is.na(x[,3])),]
  
    d<-c(d,cor(y[,2],y[,3]))
   
    }}
  
  d
}

