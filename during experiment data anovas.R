#ANOVA for time
#not a normal distribution

data<-During.experiment.recording_fixed02.11.2014
data<-data[-(189:190),]
time<-as.numeric(as.character(data$Time.s.))


kruskalmc(time ~ data$Participant.group,probs=0.001, data = data) #anova for time by participant group
kruskalmc(time ~ data$Participant.number,probs=0.001, data = data) #anova for time by individual participant

data<-During.experiment.recording_fixed02.11.2014
strikes<-as.numeric(as.character(data$Strikes))
