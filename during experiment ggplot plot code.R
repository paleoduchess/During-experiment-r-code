#Boxplots for all participants for time

data<-During.experiment.recording_fixed02.11.2014
data<-data[-(189:190),]
data$Time.s.=as.numeric(as.character(data$Time.s.))
data$Strikes=as.numeric(as.character(data$Strikes))
Participant<-data$Participant.number
Time<-data$Time.s.
Strikes<-data$Strikes


#boxplot for time data across participants
p.t<-ggplot(data, aes(Participant, Time)) #graph where each individual has their own color
p.t + geom_boxplot()
p.t + geom_boxplot() + geom_jitter(aes(colour = Participant),cex=3)+ scale_y_continuous(name="Time", limits = c(0, 600), breaks = 0:600*50)


p.t<-ggplot(data, aes(Participant, Time)) #graph where data points are two colors corresponding to group
p.t + geom_boxplot()
p.t + geom_boxplot() + geom_jitter(aes(colour = Group),cex=3)+ scale_y_continuous(name="Time", limits = c(0, 600), breaks = 0:600*50)

p.t<-ggplot(data, aes(Participant, Time)) #graph where jitter has unique color/individual and boxplots have color corresponding to group
p.t + geom_boxplot()
p.t + geom_boxplot(aes(color = Group)) + geom_jitter(aes(colour = Participant),cex=3)+ scale_y_continuous(name="Time", limits = c(0, 600), breaks = 0:600*50)


#how to change colours to yellow and blue?

# strike data across participants

p.s<-ggplot(data, aes(Participant, Strikes)) #boxplot for strike data where each individual has unique color
p.s + geom_boxplot()
p.s + geom_boxplot() + geom_jitter(aes(colour = Participant),cex=3)+ scale_y_continuous(name="Strikes", limits = c(0, 1000), breaks = 0:1000*50)

p.s<-ggplot(data, aes(Participant, Strikes)) #boxplot for strike data where there are 2 colors for group
p.s + geom_boxplot()
p.s + geom_boxplot() + geom_jitter(aes(colour = Group),cex=3)+ scale_y_continuous(name="Strikes", limits = c(0, 1000), breaks = 0:1000*50)


p.b<-ggplot(data, aes(Participant, Strikes)) #colors represent groups
p.b + geom_bar(stat="identity")
p.b + geom_bar(stat="identity", position = "dodge", fill = Group) + geom_jitter(aes(colour = Group),cex=3) + scale_y_continuous(name="Strikes", limits=c(0,800), breaks= 0:800*50)
#want to change colors of bars

p.b<-ggplot(data, aes(Participant, Strikes)) #colors show both group (bars) and individual (in jitter)
p.b + geom_bar(stat="identity")
p.b + geom_bar(stat="identity", position = "dodge", fill = Group) + geom_jitter(aes(colour = Participant),cex=3) + scale_y_continuous(name="Strikes", limits=c(0,800), breaks= 0:800*50)


#need to change colors of bars
#would like to include average strikes/participant in this graph

##HISTOGRAMS
#During experiment data plots

data<- data.frame(During.experiment.recording_fixed02.11.2014)
data<-data.frame(data[-(189:190),])
data$Time.s.=as.numeric(as.character(data$Time.s.))
Time<-data$Time.s.
Participant<-data$Participant.number
Group<-data$Participant.group
Strikes<-data$Strikes
Strikes=as.numeric(as.character(Strikes))

expert<-subset(data, Group == "Expert") #extract data for experts only
novice<-subset(data, Group == "Novice") #extract data for novices only


##TIME

#plot all participants on x axis and time on y
p2<-ggplot(data, aes(Time, fill = Group)) + geom_density(alpha = 0.2) #overlayed histograms
p2 + ggtitle("Expert vs Novice Times") + xlab("Time") + ylab("Density") + scale_x_continuous("Time", lim=c(0,600), breaks=0:600*50)


strike.density<-ggplot(data, aes(Strikes, fill = Group)) + geom_density(alpha = 0.2) #overlayed histograms
strike.density + ggtitle("Expert vs Novice Strikes") + xlab("Time") + ylab("Density") + scale_x_continuous("Strikes", lim=c(0,1000), breaks=0:1000*50)

