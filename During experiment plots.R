
#During experiment data plots
data<- data.frame(During.experiment.recording_fixed02.11.2014)
data<-data.frame(data[-(189:190),])
data$Time.s.=as.numeric(as.character(data$Time.s.))



t.participants<-data.frame(data$Participant.number,data$Time.s.,data$Participant.group)

t.participants<-arrange(t.participants, desc(data$Time.s.))
t.participants$data.Time.s.=as.numeric(as.character(t.participants$data.Time.s.))
                                                          

##TIME PLOTS
#install plyr package
#plot all participants on x axis and time on y

plot(t.participants$data.Participant.number,t.participants$data.Time.s., main = "Participant Cobble Splitting Time", xlab = "Participants", ylab = "Time") #boxplot showing time values for each participant in both groups


hist(t.participants$data.Time.s.,breaks = 100, main = "Time (s) for All Participants", xlab="Time(s)") #visualizing Strike data frequencies across all participants
p3<-hist(t.participants$data.Time.s.[t.participants$data.Participant.group == 'Expert']) #showing Strike frequencies for experts only
p4<-hist(t.participants$data.Time.s.[t.participants$data.Participant.group == 'Novice']) #showing strike frequencies for novices only
time.plot<-plot(p3, col=rgb(0,0,1,1/4), xlim = c(0,800), main = "Expert vs. Novice Time", xlab = "Time", ylab="Frequency", )
time.plot<-plot(p4, col=rgb(1,0,0,1/4), xlim = c(0,800),  add=T)
legend('topright', c("Expert","Novice"), col = c('blue','red'), lty = 1, bty='0', cex=1.5)





 ##STRIKE PLOTS

s.participants<-data.frame(data$Participant.number,data$Strikes,data$Participant.group)

s.participants$data.Strikes=as.numeric(as.character(s.participants$data.Strikes))

s.participants<-arrange(s.participants, desc(s.participants$data.Strikes))



plot(s.participants$data.Participant.number,s.participants$data.Strikes, main = "Participant Cobble Splitting Strikes", xlab = "Participants", ylab = "Strikes") #boxplot showing time values for each participant in both groups

hist(s.participants$data.Strikes,breaks = 100, main="Strikes for all Participants", xlab = "Strikes") #visualizing Strike data frequencies across all participants
p1<-hist(s.participants$data.Strikes[s.participants$data.Participant.group == 'Expert']) #showing Strike frequencies for experts only
p2<-hist(s.participants$data.Strikes[s.participants$data.Participant.group == 'Novice']) #showing strike frequencies for novices only
strike.plot<-plot(p1, col=rgb(0,0,1,1/4), xlim = c(0,800), main = "Expert vs. Novice Strikes", xlab = "Strikes", ylab="Frequency", )
strike.plot<-plot(p2, col=rgb(1,0,0,1/4), xlim = c(0,800),  add=T)
legend('topright', c("Expert","Novice"), col = c('blue','red'), lty = 1, bty='o', cex=0.75)

