

#Boxplots for all participants for time

data<-during.experiment
data$Time.s.=as.numeric(as.character(data$Time.s.))
data$Strikes=as.numeric(as.character(data$Strikes))
participant<-data$Participant.number
time<-data$Time.s.
strikes<-data$Strikes

#general plots
plot(time, xlab= "Cobble", ylab = "Time", main="Time Data Plot")
plot(strikes, xlab= "Cobble", ylab = "Strikes", main="Strike Data Plot")

#boxplot for time data across participants
p.t<-ggplot(data, aes(participant, time)) #graph where each individual has their own color
p.t + geom_boxplot()
p.t + geom_boxplot() + geom_jitter(aes(colour = participant),cex=3)+ scale_y_continuous(name="Time (s)", limits = c(0, 600), breaks = 0:600*50)


p.t.2<-ggplot(data, aes(participant, time)) #graph where data points are two colors corresponding to group
p.t.2 + geom_boxplot()
p.t.2 + geom_boxplot() + geom_jitter(aes(colour = Group),cex=3)+ scale_y_continuous(name="Time (s)", limits = c(0, 600), breaks = 0:600*50)

p.t.3<-ggplot(data, aes(Participant, Time)) #graph where jitter has unique color/individual and boxplots have color corresponding to group
p.t.3 + geom_boxplot()
p.t.3 + geom_boxplot(aes(color = Group)) + geom_jitter(aes(colour = Participant),cex=3)+ scale_y_continuous(name="Time (s)", limits = c(0, 600), breaks = 0:600*50)


#how to change colours to yellow and blue?

# strike data across participants

p.s<-ggplot(data, aes(participant, strikes)) #boxplot for strike data where each individual has unique color
p.s + geom_boxplot()
p.s + geom_boxplot() + geom_jitter(aes(colour = participant),cex=3)+ scale_y_continuous(name="Strikes", limits = c(0, 1000), breaks = 0:1000*50)

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
p2<-ggplot(data, aes(time, fill = Participant.group)) + geom_density(alpha = 0.2) #overlayed histograms
p2 + ggtitle("Expert vs Novice Time") + xlab("Time (s)") + ylab("Density") + scale_x_continuous("Time (s)", lim=c(0,600), breaks=0:600*50)

#Strikes
strike.density<-ggplot(data, aes(strikes, fill = Participant.group)) + geom_density(alpha = 0.2) #overlayed histograms
strike.density + ggtitle("Expert vs Novice Strikes") + xlab("Strikes") + ylab("Density") + scale_x_continuous("Strikes", lim=c(0,1000), breaks=0:1000*50)

