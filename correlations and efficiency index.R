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
data$efficiency <- ((Time/Strikes)*(data$Prob.success.)) #creating efficiency index and attaching to data frame ##change to include probabilty of success - Time/Strikes*prob(success)# need new column that takes number of successes divided by number of cobbles for each participant
data$efficiency <- ((Time/Strikes)*(100)) #creating efficiency index and attaching to data frame ##change to include probabilty of success - Time/Strikes*prob(success)# need new column that takes number of successes divided by number of cobbles for each participant

efficiency<-data$efficiency

plot(efficiency)

subset(data, efficiency == max(efficiency)) #extract row from data.frame

 #creating efficiency index and attaching to data frame

kruskalmc(efficiency~Participant) 



ggplot(data, aes(Participant, efficiency)) + geom_boxplot() + geom_jitter(aes(colour = Participant)) + ylab ("Efficiency")

ggplot(data, aes(Group, efficiency)) + geom_boxplot() + geom_jitter(aes(colour = Group)) + ylab ("Efficiency")



kruskalmc(efficiency~Group) 

kruskalmc(efficiency~Participant) 

kruskalmc(efficiency~Participant + Group) 

#do same stuff but only for successes
data.splits<-subset(data, outcome==2)


plot(data.splits$efficiency)

kruskalmc(data.splits$efficiency~data.splits$Participant.group) 

ggplot(data.splits, aes(Participant.number, as.numeric(as.character(efficiency)))) + geom_boxplot() + geom_jitter(aes(colour = Participant.number)) + ylab ("Efficiency")

ggplot(data.splits, aes(Participant.number, as.numeric(as.character(Time.s.)))) + geom_boxplot() + geom_jitter(aes(colour = Participant.number)) + ylab ("Time")

ggplot(data.splits, aes(Participant.number, as.numeric(as.character(Strikes)))) + geom_boxplot() + geom_jitter(aes(colour = Participant.number)) + ylab ("Time")


