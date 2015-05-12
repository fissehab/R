
setwd("C:/Fish/classes/spring_2015/R/programming_assignments/3")

rankhospital <- function(state, outcome,num='best') {
    
    if(is.character(num)){
        if (num %in% c('best','worst')==FALSE){
            stop('Invalid Rank')
        } }
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    if (state %in% data$State ==FALSE) { stop("invalid state")}
    
    if (outcome %in% c("heart failure","heart attack","pneumonia")==FALSE){
        stop("invalid outcome")}
    
   
 
    
    if (state %in% data$State & outcome %in% c("heart failure","heart attack","pneumonia")){
     if(outcome=="heart attack"){
         a<-data[,c(2,7,11)]
     }
     
     if(outcome=="heart failure"){
         a<-data[,c(2,7,17)]
     }
     
     if(outcome=="pneumonia"){
         a<-data[,c(2,7,23)]
     }
     
     b<-split(a,a$State)
     c<-b[[state]]
     c<-c[,c(1,3)]
     colnames(c)<-cbind('Hospital.Name',outcome)
     c[,2]<-as.numeric(c[,2])
     
     if (num=='best'){
                              
     d<- c[which(c[,2]==min(c[,2],na.rm=TRUE)),]

     hospitals<-d$Hospital.Name
     hospital<-sort(hospital)[1]
     
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





