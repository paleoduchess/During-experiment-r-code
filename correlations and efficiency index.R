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
Group<-data$Participant.group
outcome<-data$Split.No.split
outcome=as.numeric(outcome)
data$efficiency <- Time/Strikes #creating efficiency index and attaching to data frame
efficiency<-data$efficiency


cor.test(Time,Strikes)
ggplot(Time, Strikes)

efficiency.lm<-lm(outcome ~ Time + Strikes + efficiency, data)

anova(efficiency.lm)
plot(efficiency.lm)

#do anova with efficiency - see if it correlates with novices and experts for each idividual KS