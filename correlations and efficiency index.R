#Correlation betwee strikes and time


#for all participants
#for experts
#for novices

#Efficiency index

#time/strikes for each splitting event as an index of efficiency
#data frame with participant group, participant number, time, strikes, and efficiency
#need to make a function that will divide the time by the strike value for each splitting event (by row)

data<-During.experiment.recording_fixed02.11.2014
data<-data[-(189:190),]
Time<-data$Time.s.
Time=as.numeric(as.character(Time))
Strikes<-data$Strikes
Strikes=as.numeric(as.character(Strikes))
Participant<-data$Participant.number

new.data<-data.frame(Participant,Time,Strikes)

new.data<- transform(new.data, Efficiency = Time / Strikes)



new.data<-data.frame

e.index<- function(x,y){
  
  
}