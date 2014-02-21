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
p.t + geom_boxplot() + geom_jitter(aes(colour = Participant),cex=3)+ scale_y_continuous(name="Time (s)", limits = c(0, 600), breaks = 0:600*50)


p.t.2<-ggplot(data, aes(Participant, Time)) #graph where data points are two colors corresponding to group
p.t.2 + geom_boxplot()
p.t.2 + geom_boxplot() + geom_jitter(aes(colour = Group),cex=3)+ scale_y_continuous(name="Time (s)", limits = c(0, 600), breaks = 0:600*50)

p.t.3<-ggplot(data, aes(Participant, Time)) #graph where jitter has unique color/individual and boxplots have color corresponding to group
p.t.3 + geom_boxplot()
p.t.3 + geom_boxplot(aes(color = Group)) + geom_jitter(aes(colour = Participant),cex=3)+ scale_y_continuous(name="Time (s)", limits = c(0, 600), breaks = 0:600*50)


#how to change colours to yellow and blue?

# strike data across participants

p.s<-ggplot(data, aes(Participant, Strikes)) #boxplot for strike data where each individual has unique color
p.s + geom_boxplot()
p.s + geom_boxplot() + geom_jitter(aes(colour = Participant),cex=3)+ scale_y_continuous(name="Strikes", limits = c(0, 1000), breaks = 0:1000*50)

p.s.2<-ggplot(data, aes(Participant, Strikes)) #boxplot for strike data where there are 2 colors for group
p.s.2 + geom_boxplot()
p.s.2 + geom_boxplot() + geom_jitter(aes(colour = Group),cex=3)+ scale_y_continuous(name="Strikes", limits = c(0, 1000), breaks = 0:1000*50)

p.s.3<-ggplot(data, aes(Participant, Strikes)) #boxplot for strike data where there are 2 colors for group
p.s.3 + geom_boxplot()
p.s.3 + geom_boxplot(aes(color = Group)) + geom_jitter(aes(colour = Participant),cex=3)+ scale_y_continuous(name="Strikes", limits = c(0, 1000), breaks = 0:1000*50)


#want to play around with colors still
#would like to include average strikes/participant in this graph

##HISTOGRAMS

##TIME

#plot all participants on x axis and time on y
p2<-ggplot(data, aes(Time, fill = Group)) + geom_density(alpha = 0.2) #overlayed histograms
p2 + ggtitle("Expert vs Novice Time") + xlab("Time (s)") + ylab("Density") + scale_x_continuous("Time (s)", lim=c(0,600), breaks=0:600*50)

#Strikes
strike.density<-ggplot(data, aes(Strikes, fill = Group)) + geom_density(alpha = 0.2) #overlayed histograms
strike.density + ggtitle("Expert vs Novice Strikes") + xlab("Time") + ylab("Density") + scale_x_continuous("Strikes", lim=c(0,1000), breaks=0:1000*50)

