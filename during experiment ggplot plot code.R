#Boxplots for all participants for time

data<-During.experiment.recording_fixed02.11.2014
data<-data[-(189:190),]
data$Time.s.=as.numeric(as.character(data$Time.s.))

p<-ggplot(data, aes(factor(Participant.number), Time.s.))
p + geom_boxplot()
p + geom_boxplot(aes(main = "Time (s) for Across Participants", xlab="Participant Number", ylab = "Time (s)")) + geom_jitter(aes(colour = factor(Participant.number)))
