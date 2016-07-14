#Write a function called best that take two arguments:  the 2-character abbreviated name of a state and an outcome name.  The function reads the
#outcome-of-care-measures.csv  le and returns a character vector with  the  name  of  the  hospital  that  has  the  best  (i.e.   lowest)  30-day  mortality  for  the  speci ed  outcome
#in that state.  The hospital name is the name provided in the Hospital.Name variable.  The outcomes can be one of \heart attack", \heart failure", or \pneumonia".  Hospitals that do #not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings. Handling ties .  If there is a tie for the best hospital for a given #outcome, then the hospital names should be sorted in alphabetical order and the  rst hospital in that set should be chosen (i.e.  if hospitals \b", \c", and \f" are tied for best, then #hospital \b" should be returned).

best <- function(st, outcome) {
	if(nchar(st)<2 || nchar(st)> 2)
	{
		stop("Invalid State")
	}
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
			
			bes<-min(statesSpecificData$metrics)
			hosNM<-as.character(statesSpecificData[which(statesSpecificData$metrics==bes),1])[1]
			
		
		#}#end for
		
        ## Return a data frame with the hospital names and the (abbreviated) 
        ## state name
		 # rankedhospitals <- as.data.frame(matrix(rankedhospitals, length(states), 2, byrow = TRUE))
        #colnames(rankedhospitals) <- c("hospital", "state")
		
		return(hosNM)

}
