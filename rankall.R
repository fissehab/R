
setwd("C:/Fish/classes/spring_2015/R/programming_assignments/3")

rankall <- function(outcome,num='best') {
    
    if(is.character(num)){
        if (num %in% c('best','worst')==FALSE){
            stop('Invalid Rank')
        } }
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    
    if (outcome %in% c("heart failure","heart attack","pneumonia")==FALSE){
        stop("invalid outcome")}
    
   
 
    
    if (outcome %in% c("heart failure","heart attack","pneumonia")){
     if(outcome=="heart attack"){
         a<-data[,c(2,7,11)]
     }
     
     if(outcome=="heart failure"){
         a<-data[,c(2,7,17)]
     }
     
     if(outcome=="pneumonia"){
         a<-data[,c(2,7,23)]
     }}
    
    a[,3]<-as.numeric(a[,3])
    
    colnames(a)<-cbind('hospital','state','value')
    
    b<- split(a,a$state)
    
    c<-names(b)
     ans<-list()
    for  (letter in c){
        d<-b[[letter]]
         if (num=='best'){
                              
           e<- d[which(d[,3]==min(d[,3],na.rm=TRUE)),]

           hospitals<-e$hospital
           hospital<-sort(hospitals)[1]
           ans<-rbind(ans,e[hospital,])
     
     }
     
     if (num=='worst'){
         
         d<- c[which(c[,2]==max(c[,2],na.rm=TRUE)),]
         
         hospitals<-d$Hospital.Name
         hospital<-sort(hospitals)[1]
         
     }
     
    if(is.numeric(num) & num < length(c$Hospital.Name)){
        
        
        l<-c[!is.na(c[,2]),]
        ll<-unique(sort(l[,2]))
        
         m<-c()
        
        for ( i in 1:length(ll)){
            temp<-which(l[,2]==ll[i])
            m<-rbind(m,l[temp,])
        }
        
        k<-m[num,2]
        p1<-which(m[,2]<k)
        p<-which(m[,2]==k)
        
        num2<-num-length(p1)
        
        hospitals<-sort(m[p,1])
        
        hospital<-hospitals[num2] 
        
    }
     
    if (is.numeric(num)){
        if (num > length(c$Hospital.Name)){ 
            return(NA)}}

    }
    
    hospital   
}





