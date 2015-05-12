
complete <- function(directory,id = 1:332) {
  

if (max(id) <=332){
  
  ID <-id
  
  for (i in seq(1,length(id))){
    
  if (id[i]<10){ ID[i]<-paste(0,0,id[i],".csv",sep='')}
  
  if (id[i]<100 &id[i]>=10){ID[i]<-paste(0,id[i],".csv",sep='')}
  
  else if(id[i]>=100) {ID[i]<-paste(id[i],".csv",sep='')}}
  
  d<-matrix(0,ncol=2,nrow=length(id))
  
  for (i in seq(1,length(id))){
   x<- read.table(paste(directory,'/',ID[i],sep=''),header=TRUE,sep=",")
   z<-sum(!is.na(x[,2]) & !is.na(x[,3]))
   
   d[i,]<-c(id[i],z)
   
  }
  colnames(d)<-c("id", "nobs")
  
  d <- as.data.frame(d)
  
result<-d



}

else{
 result<- show('ID number out of bounds, mad ID =332')
}

result
}

