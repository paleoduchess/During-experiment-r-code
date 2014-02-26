
data<-during.experiment

time<-data$Time.s.
time=as.numeric(as.character(time))
strikes<-data$Strikes
strikes=as.numeric(as.character(strikes))
participant<-data$Participant.number
group<-data$Participant.group
outcome<-data$Split.No.split
outcome=as.numeric(outcome)
data$efficiency <- ((time/strikes)*(data$Prob.success.)) #creating efficiency index and attaching to data frame ##change to include probabilty of success - Time/Strikes*prob(success)# need new column that takes number of successes divided by number of cobbles for each participant

efficiency<-data$efficiency #naming efficiency column

plot(efficiency, xlab="Cobble", ylab="Efficiency", main="Efficiency Plot") #visualize the efficiency data generally

subset(data, efficiency == max(efficiency)) #extract row from data.frame ##don't need this now, but could be useful for future datasets


##ANOVA for efficiency
kruskalmc(efficiency~group) #ANOVA by group
kruskalmc(efficiency~participant) #ANOVA by participant



ggplot(data, aes(Participant, efficiency)) + geom_boxplot() + geom_jitter(aes(colour = Participant)) + ylab ("Efficiency")

ggplot(data, aes(Group, efficiency)) + geom_boxplot() + geom_jitter(aes(colour = Group)) + ylab ("Efficiency")


#do same stuff but only for successes

data.splits<-subset(data, outcome==2)

plot(data.splits$efficiency, ylab="Efficiency", xlab="Cobble", main="Efficiency data for Successful Splits") #general plot for only successes

kruskalmc(data.splits$efficiency~data.splits$Participant.group) 

ggplot(data.splits, aes(Participant.number, as.numeric(as.character(efficiency)))) + geom_boxplot() + geom_jitter(aes(colour = Participant.number)) + ylab ("Efficiency")

ggplot(data.splits, aes(Participant.number, as.numeric(as.character(Time.s.)))) + geom_boxplot() + geom_jitter(aes(colour = Participant.number)) + ylab ("Time")

ggplot(data.splits, aes(Participant.number, as.numeric(as.character(Strikes)))) + geom_boxplot() + geom_jitter(aes(colour = Participant.number)) + ylab ("Time")


