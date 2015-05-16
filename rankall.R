
# setwd("C:/Fish/classes/spring_2015/R/programming_assignments/3")

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
    
    states<-sort(unique(a$state))
    
    b<-split(a,a$state)
    
    ans_state<-c()
    ans_hospital<-c()
    
    
    
    
    for (letter in states){

   
    g<-b[[letter]]
    
     g<-g[,c(1,3)]

    if (num=='best'){
        
        d<- c[which(g[,2]==min(g[,2],na.rm=TRUE)),]
        
        hospitals<-d$hospital
        hospital<-sort(hospitals)[1]
        
    }
    
    if (num=='worst'){
        
        d<- g[which(g[,2]==max(g[,2],na.rm=TRUE)),]
        
        hospitals<-d$hospital
        hospital<-sort(hospitals)[1]
        
    }
    
    if(is.numeric(num) & num < length(g$hospital)){
        
        
        l<-g[!is.na(g[,2]),]
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
        if (num > length(g$hospital)){ 
            hospital<-NA}}
    
    ans_state<-c(ans_state,letter)
    ans_hospital<-c(ans_hospital,hospital)
    
    }
    
    df = data.frame(ans_hospital, ans_state)       # df is a data frame 
    colnames(df)<-c('hospital','state')
    
    df
} 
    

