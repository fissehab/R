
#setwd("C:/Fish/classes/spring_2015/R/programming_assignments/3")

best <- function(state, outcome) {
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
 
    
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
                              
     d<- c[which(c[,2]==min(c[,2],na.rm=TRUE)),]

     best_hospitals<-d$Hospital.Name
     best_hospital<-sort(best_hospitals)[1]
     
    }
    
    else if (state %in% data$State ==FALSE) { stop("invalid state")}
    
    else if (outcome %in% c("heart failure","heart attack","pneumonia")==FALSE){
        stop("invalid outcome")
    }
    
    best_hospital   
}





