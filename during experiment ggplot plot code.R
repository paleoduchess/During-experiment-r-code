#Boxplots for all participants for time

data<-During.experiment.recording_fixed02.11.2014
data<-data[-(189:190),]
data$Time.s.=as.numeric(as.character(data$Time.s.))
data$Strikes=as.numeric(as.character(data$Strikes))
Participant<-data$Participant.number
Time<-data$Time.s.
Strikes<-data$Strikes


#boxplot for time data across participants
p<-ggplot(data, aes(Participant, Time))
p + geom_boxplot()
p + geom_boxplot() + geom_jitter(aes(colour = Participant),cex=3)+ scale_y_continuous(name="Time", limits = c(0, 600), breaks = 0:600*50)


# strike data across participants
p<-ggplot(data, aes(Participant, Strikes))
p + geom_bar(stat="identity")
p + geom_bar(stat="identity") + geom_jitter(aes(colour = Participant),cex=3) + scale_y_continuous(name="Strikes", limits=c(0,800), breaks= 0:800*50)

p<-ggplot(data, aes(Participant, Strikes))

p + geom_jitter(aes(colour = Participant),cex=3) + scale_y_continuous(name="Strikes", limits=c(0,800), breaks= 0:800*50)
#would like to include average strikes/participant in this graph