#The function should use the following template.
rankhospital <- function(st, outcome, num = "best"){
## Read outcome data
## Check that state and outcome are valid
## For each state, find the hospital of the given rank
## Return a data frame with the

		outcsv <- read.csv("outcome-of-care-measures.csv");
		
		hospitalname <- outcsv[,2]
		state <- outcsv[,"State"]
		HeartAttack <- as.numeric(as.character(outcsv[,11]))
		HeartFailure <- as.numeric(as.character(outcsv[,17]))
		Pneumonia <- as.numeric(as.character(outcsv[,23]))
		#DatatobeManipulated<-data.frame(hospitalname,state,HeartAttack,HeartFailure,Pneumonia)
		
		if(outcome == "heart attack"){
			DatatobeManipulated<-data.frame(hospitalname,state,metrics=HeartAttack)		
			DatatobeManipulated<-DatatobeManipulated[order(hospitalname,state,DatatobeManipulated$metrics,decreasing = FALSE,na.last=NA),]
		}
		else if(outcome == "heart failure"){
			DatatobeManipulated<-data.frame(hospitalname,state,metrics=HeartFailure)		
			DatatobeManipulated<-DatatobeManipulated[order(hospitalname,state,DatatobeManipulated$metrics,decreasing = FALSE,na.last=NA),]
		}
		else if(outcome == "pneumonia"){
			DatatobeManipulated<-data.frame(hospitalname,state,metrics=Pneumonia)		
			DatatobeManipulated<-DatatobeManipulated[order(hospitalname,state,DatatobeManipulated$metrics,decreasing = FALSE,na.last=NA),]
		}

		#count the number of unique states
		#states<-sort(unique(DatatobeManipulated[,"state"]))
		
		#rankedhospitals <- vector()
		#rankedhospitals <- data.frame(hospitalName = character(), State = character(), stringsAsFactors = FALSE)
		#for(i in 1:length(states)){
 
			statesSpecificData<-DatatobeManipulated[which(DatatobeManipulated$state == st),]
			if(num == "best"){
			bes<-min(statesSpecificData$metrics)
			hosNM<-as.character(statesSpecificData[which(statesSpecificData$metrics==bes),1])[1]
			
			}else if(num == "worst"){
			wor<-max(statesSpecificData$metrics)
			hosNM<-as.character(statesSpecificData[which(statesSpecificData$metrics==wor),1])[1]
			
			
			}else {
				num <- as.integer(num)
				if(typeof(num) == "integer" && nrow(statesSpecificData) >= num){
				  statesSpecificData<-statesSpecificData[order(statesSpecificData$metrics),]
					hosNM<-as.character(statesSpecificData[num,1])
				
					
					  
				} else {
					hosNM<-NA

				}
		
			}
		
		#}#end for
		
        ## Return a data frame with the hospital names and the (abbreviated) 
        ## state name
		 # rankedhospitals <- as.data.frame(matrix(rankedhospitals, length(states), 2, byrow = TRUE))
        #colnames(rankedhospitals) <- c("hospital", "state")
        
	return(hosNM)

		

}