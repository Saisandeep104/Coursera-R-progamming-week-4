best<- function(state,outcome){
        data<-read.csv("outcome-of-care-measures.csv")
        #p<-paste("Hospital.30.Day.Death..Mortality..Rates.from.",outcome,sep = "")
        input = c("heart attack","heart failure","pneumonia")
        data[,11]<-as.numeric(data[,11])
        data[,17]<-as.numeric(data[,17])
        data[,23]<-as.numeric(data[,23])
        
        if(!state%in%data$State){
                ##Check here is an error occours
                return("Invalid state")
        }
        if(!outcome%in%input){
                return("Invalid outcome")
        }
        
        if(outcome=="heart attack"){
                best_h<-data[which(data$State==state),]
                #best_h[,11]<-as.numeric(best_h[,11])
                best_h<-best_h[which(best_h$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==min(best_h$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),]
                #print(best_h)
                return(best_h$Hospital.Name)
        }
        if(outcome=="heart failure"){
                best_h<-data[which(data$State==state),]
                #best_h[,11]<-as.numeric(best_h[,11])
                #print(best_h)
                best_h<-best_h[which(best_h$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==min(best_h$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),]
                #print(best_h)
                best_h<-arrange(best_h,desc(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
                return(best_h$Hospital.Name)
        }
        if(outcome=="pneumonia"){
                best_h<-data[which(data$State==state),]
                #best_h[,11]<-as.numeric(best_h[,11])
                #print(best_h)
                best_h<-best_h[which(best_h$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==min(best_h$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),]
                #print(best_h)
                return(best_h$Hospital.Name)
        }
}

#function sorts(data){
#       best_h<-data[which(data$State==state),] 
#        best_h<-best_h[which(best_h$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==min(best_h$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),]
        
#}