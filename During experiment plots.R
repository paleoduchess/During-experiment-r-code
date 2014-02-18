
#During experiment data plots
data<- During.experiment.recording_fixed02.11.2014
all.participants<-arrange(data, desc(Participant.number))
all.participants$Time.s.=as.numeric(as.character(all.participants$Time.s.))

##TIME PLOTS
#install plyr package
#plot all participants on x axis and time on y

hist(all.participants$Time.s.,breaks = 100) #visualizing Strike data frequencies across all participants
p3<-hist(all.participants$Time.s.[all.participants$Participant.group == 'Expert']) #showing Strike frequencies for experts only
p4<-hist(all.participants$Time.s.[all.participants$Participant.group == 'Novice']) #showing strike frequencies for novices only
time.plot<-plot(p3, col=rgb(0,0,1,1/4), xlim = c(0,800), main = "Expert vs. Novice Time", xlab = "Time", ylab="Frequency", )
time.plot<-plot(p4, col=rgb(1,0,0,1/4), xlim = c(0,800),  add=T)
legend('topright', c("Expert","Novice"), col = c('blue','red'), lty = 1, bty='o', cex=0.75)

#put together using ggplot?
#plot all participants from slowest to fastest (within each participant have cobbles ordered slowest to fastest)
#plot averages times across participants
plot(all.participants$Participant.number,as.numeric(as.character(all.participants$Time.s.)))
 ##STRIKE PLOTS
all.participants$Strikes=as.numeric(as.character(all.participants$Strikes))
hist(all.participants$Strikes,breaks = 100) #visualizing Strike data frequencies across all participants
p1<-hist(all.participants$Strikes[all.participants$Participant.group == 'Expert']) #showing Strike frequencies for experts only
p2<-hist(all.participants$Strikes[all.participants$Participant.group == 'Novice']) #showing strike frequencies for novices only
strike.plot<-plot(p1, col=rgb(0,0,1,1/4), xlim = c(0,800), main = "Expert vs. Novice Strikes", xlab = "Strikes", ylab="Frequency", )
strike.plot<-plot(p2, col=rgb(1,0,0,1/4), xlim = c(0,800),  add=T)
legend('topright', c("Expert","Novice"), col = c('blue','red'), lty = 1, bty='o', cex=0.75)

