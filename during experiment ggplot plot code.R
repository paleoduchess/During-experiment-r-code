library(ggplot2)

#Boxplots for all participants for time

data<-during.experiment
data$Time.s.=as.numeric(as.character(data$Time.s.))
data$Strikes=as.numeric(as.character(data$Strikes))
Participant<-data$Participant.number
Time<-data$Time.s.
Strikes<-data$Strikes
Group<-data$Participant.group

#general plots
plot(time, xlab= "Cobble", ylab = "Time", main="Time")
plot(strikes, xlab= "Cobble", ylab = "Strikes", main="Strikes")


#boxplot for time data across participants
p.t<-ggplot(data, aes(Participant, time)) #graph where each individual has their own color
p.t + geom_boxplot()
p.t + geom_boxplot() + geom_jitter(aes(colour = Participant),cex=3)+ scale_y_continuous(name="Time (s)", limits = c(0, 600), breaks = 0:600*50)+ggtitle("Time Across All Participants") + xlab("p < 0.001")

p.t.2<-ggplot(data, aes(Participant, Time)) #graph where data points are two colors corresponding to group
p.t.2 + geom_boxplot()
p.t.2 + geom_boxplot(aes(fill = Group)) + geom_jitter(cex=2)+ scale_y_continuous(name="Time (s)", limits = c(0, 600), breaks = 0:600*50) + theme_bw(18) + scale_fill_manual(name = "Group", values = c("dodgerblue3", "yellow"), labels = c("0" = "Expert", "1" = "Novice")) + ggtitle("Time Across All Participants") + xlab("p < 0.001")


# strike data across participants

p.s<-ggplot(data, aes(participant, strikes)) #boxplot for strike data where each individual has unique color
p.s + geom_boxplot()
p.s + geom_boxplot() + geom_jitter(aes(colour = participant),cex=3)+ scale_y_continuous(name="Strikes", limits = c(0, 1000), breaks = 0:1000*50)

p.s.2<-ggplot(data, aes(Participant, Strikes)) #boxplot for strike data where there are 2 colors for group
p.s.2 + geom_boxplot()
p.s.2 + geom_boxplot(aes(fill = Group)) + geom_jitter(cex=2)+ scale_y_continuous(name="Strikes", limits = c(0, 1000), breaks = 0:1000*50) + theme_bw(18) + scale_fill_manual(name = "Group", values = c("dodgerblue3", "yellow"), labels = c("0"= "Expert", "1" = "Novice")) + ggtitle("Strikes Across All Participants") + xlab("p < 0.001")


##HISTOGRAMS

##TIME

#plot all participants on x axis and time on y
p2<-ggplot(data, aes(Time, fill = Group)) + geom_density(alpha = 0.2) #overlayed histograms
p2 + ggtitle("Expert vs Novice Time") + xlab("Time (s)") + ylab("Density") + scale_x_continuous("Time (s)", lim=c(0,600), breaks=0:600*50) + scale_fill_manual(name = "Group", values = c("dodgerblue3", "yellow"), labels = c("0" = "Expert", "1" = "Novice")) + xlab("p < 0.001")

#Strikes
strike.density<-ggplot(data, aes(strikes, fill = Participant.group)) + geom_density(alpha = 0.2) #overlayed histograms
strike.density + ggtitle("Expert vs Novice Strikes") + xlab("Strikes") + ylab("Density") + scale_x_continuous("Strikes", lim=c(0,1000), breaks=0:1000*100) + scale_fill_manual(name = "Group", values = c("dodgerblue3", "yellow"), labels = c("0" = "Expert", "1" = "Novice")) + xlab("p < 0.001")

#Efficiency plots
outcome<-data$Split.No.split
outcome=as.numeric(outcome)
data$efficiency <- ((time/strikes)*(data$Prob.success.))
efficiency<-data$efficiency #naming efficiency column
data.splits<-subset(data, outcome==2)

#ggplots for efficiency (success or no success)

ggplot(data, aes(Participant, efficiency)) + geom_boxplot(aes(fill=Group)) + geom_jitter(cex=2) + ylab ("Efficiency") + theme_bw(18) + scale_fill_manual(name = "Group", values = c("dodgerblue3", "yellow"), labels = c("0"= "Expert", "1" = "Novice")) + ggtitle("Experts vs. Novice Efficiency") + xlab("Participants")


ggplot(data, aes(Group, efficiency)) + geom_boxplot(aes(fill=Group)) + geom_jitter(cex=2) + ylab ("Efficiency") + theme_bw(18) + scale_fill_manual(name = "Group", values = c("dodgerblue3", "yellow"), labels = c("0"= "Expert", "1" = "Novice")) + ggtitle("Experts vs. Novice Efficiency") + xlab("p < 0.001")



#ggplots for efficiency (success only)
ggplot(data.splits, aes(Participant.number, as.numeric(as.character(efficiency)))) + geom_boxplot() + geom_jitter(aes(colour = Participant.number)) + ylab ("Efficiency (Success Only)")

ggplot(data.splits, aes(Participant.number, as.numeric(as.character(Time.s.)))) + geom_boxplot() + geom_jitter(aes(colour = Participant.number)) + ylab ("Time")

ggplot(data.splits, aes(Participant.number, as.numeric(as.character(Strikes)))) + geom_boxplot() + geom_jitter(aes(colour = Participant.number)) + ylab ("Time")

#efficiency histograms
efficiency.density<-ggplot(data, aes(efficiency, fill = Group)) + geom_density(alpha = 0.2) #overlayed histograms
efficiency.density + ggtitle("Expert vs Novice Efficiency") + xlab("Efficiency") + ylab("Density") + scale_x_continuous("Efficiency", lim=c(0,10), breaks=0:10*0.5) + theme_bw(18) + scale_fill_manual(name = "Group", values = c("dodgerblue3", "yellow"), labels = c("0"= "Expert", "1" = "Novice")) + ggtitle("Experts vs. Novice Efficiency") + xlab("p < 0.001")

