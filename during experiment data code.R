data<-During.experiment.recording_fixed02.11.2014
behavior.data<-data[-(189:190),]
behavior.data<-data[-(189:190),-(4)]

read.table(behavior.data.new, header=T)

#by participant group
class(behavior.data) #check the object
arrange(behavior.data, desc(sample)) #load plyr package and sort r.cobble.data.random<-
behavior.data.new<-arrange(behavior.data, desc(Participant.group)) #arrange by participant group
expert.behavior<-subset(behavior.data.new, Participant.group == "Expert") #extract based on Expert
expert.time<-as.numeric(as.character(expert.behavior$Time.s.))
expert.strikes<-as.numeric(as.character(expert.behavior$Strikes))
novice.behavior<-subset(behavior.data.new, Participant.group == "Novice") #extract based on Novice
novice.time<-as.numeric(as.character(novice.behavior$Time.s.))
novice.strikes<-as.numeric(as.character(novice.behavior$Strikes))
expert.splits<-subset(expert.behavior, Split.No.split == "Y") #extract data for successful expert splits
expert.fail<-subset(expert.behavior, Split.No.split == "N") #extract data for failed expert splits
novice.splits<-subset(novice.behavior, Split.No.split == "Y") #extract data for successful novice splits
novice.fail<-subset(novice.behavior, Split.No.split == "N") #extract data for failed novice splits
summary(behavior.data.new) #summaries
summary(expert.behavior)
summary(novice.behavior)
summary(expert.splits)
summary(expert.fail)
summary(novice.splits)
summary(novice.fail)

# Expert descriptives
co.var<-function(x)(100*sd(x)/mean(x)) #function to calculate cv values

e.split.strikes<-as.numeric(as.character(expert.splits$Strikes)) #number of strikes for expert splits 
mean(e.split.strikes) #combined data for expert number of strikes
max(e.split.strikes)
min(e.split.strikes)
sd(e.split.strikes)
co.var(e.split.strikes)

e.fail.strikes<-as.numeric(as.character(expert.fail$Strikes)) #number of strikes for expert fails
mean(e.fail.strikes) #combined data for expert number of strikes
max(e.fail.strikes)
min(e.fail.strikes)
sd(e.fail.strikes)
co.var(e.fail.strikes)

#Novice descriptives
n.split.strikes<-as.numeric(as.character(novice.splits$Strikes)) #number of strikes for novice splits
mean(n.split.strikes) #combined data for novice number of strikes for splits
max(n.split.strikes)
min(n.split.strikes)
sd(n.split.strikes)
co.var(n.split.strikes)

n.fail.strikes<-as.numeric(as.character(novice.fail$Strikes)) #number of strikes for novice fails
mean(n.fail.strikes) #combined data for expert number of strikes
max(n.fail.strikes)
min(n.fail.strikes)
sd(n.fail.strikes)
co.var(n.fail.strikes)

#data for Time (seconds) descriptives
#experts
e.split.time<-as.numeric(as.character(expert.splits$Time.s.)) #time data for expert splits
mean(e.split.time)
max(e.split.time)
min(e.split.time)
sd(e.split.time)
co.var(e.split.time)

e.fail.time<-as.numeric(as.character(expert.fail$Time.s.)) #time data for expert fails
mean(e.fail.time)
max(e.fail.time)
min(e.fail.time)
sd(e.fail.time)
co.var(e.fail.time)

n.split.time<-as.numeric(as.character(novice.splits$Time.s.)) #time data for novice splits
mean(n.split.time)
max(n.split.time)
min(n.split.time)
sd(n.split.time)
co.var(n.split.time)

n.fail.time<-as.numeric(as.character(novice.fail$Time.s.)) #time data for novice fails
mean(n.fail.time)
max(n.fail.time)
min(n.fail.time)
sd(n.fail.time)
co.var(n.fail.time)

#Tests for normality
#experts

shapiro.test(e.split.strikes) #strikes for expert splits NO
kurtosis(e.split.strikes)
skewness(e.split.strikes)
qqnorm(e.split.strikes);qqline(e.split.strikes, col = 2) 

shapiro.test(e.fail.strikes) #NO
kurtosis(e.fail.strikes)
skewness(e.fail.strikes)
qqnorm(e.fail.strikes);qqline(e.fail.strikes, col = 2)

shapiro.test(e.split.time) #NO
kurtosis(e.split.time)
skewness(e.split.time)
qqnorm(e.split.time);qqline(e.split.time, col = 2)

shapiro.test(e.fail.time) #NO
kurtosis(e.fail.time)
skewness(e.fail.time)
qqnorm(e.fail.time);qqline(e.fail.time, col = 2)

#novices
shapiro.test(n.split.strikes) #NO
kurtosis(n.split.strikes)
skewness(n.split.strikes)
qqnorm(n.split.strikes);qqline(n.split.strikes, col = 2)

shapiro.test(n.fail.strikes) #NO
kurtosis(n.fail.strikes)
skewness(n.fail.strikes)
qqnorm(n.fail.strikes);qqline(n.fail.strikes, col = 2)

shapiro.test(n.split.time) #NO
kurtosis(n.split.time)
skewness((n.split.time))
qqnorm(n.split.time);qqline(n.split.time, col = 2)

shapiro.test(n.fail.time) #NO
kurtosis(n.fail.time)
skewness(n.fail.time)
qqnorm(n.fail.time);qqline(n.fail.time, col = 2)

#Plots
#Show expert vs. novice number of strikes for splits
boxplot(e.split.strikes,n.split.strikes, main = "Expert vs. Novice Number of Strikes for Split Cobbles", xlab = "Expert vs. Novice", ylab = "Number of Strikes")
boxplot(e.fail.strikes,n.fail.strikes, main = "Expert vs. Novice Number of Strikes for Failed Cobbles", xlab = "Expert vs. Novice", ylab = "Number of Strikes")
boxplot(e.split.time,n.split.time, main = "Expert vs. Novice Number Time for Split Cobbles", xlab = "Expert vs. Novice", ylab = "Time (seconds)")
boxplot(e.fail.time,n.fail.time, main = "Expert vs. Novice Number Time for Failed Cobbles", xlab = "Expert vs. Novice", ylab = "Time (seconds)")


#correlations

cor.test(e.split.time,e.split.strikes) #Experts time vs. strikes for splits
cor.test(e.fail.time,e.fail.strikes) #Experts time vs. strikes for fails
cor.test(n.split.time,n.split.strikes) #novices time vs. strikes for splits
cor.test(n.fail.time,n.fail.strikes) #novices time vs. strikes for fail

#nonparametric anova
expert.time<-as.numeric(as.character(expert.behavior$Time.s.))
expert.strike.glm.among<-glm(as.numeric(as.character(expert.behavior$Strikes)) ~ expert.time, family = poisson, data = expert.behavior)
summary(expert.strike.glm.among)
expert.strike.among<-anova(expert.strike.glm.among)
expert.strike.glm.within<-glm(as.numeric(as.character(expert.behavior$Strikes)) ~ expert.time + expert.behavior$Participant.number, family = poisson, data = expert.behavior)
summary(expert.strike.glm.within)
expert.strike.within<-anova(expert.strike.glm.within)


novice.glm<-glm(as.numeric(as.character(novice.behavior$Strikes))~as.numeric(as.character(novice.behavior$Time.s.)), family = poisson, data = expert.behavior)
summary(novice.glm)
anova(novice.glm)

strikes.glm<-glm(as.numeric(as.character(behavior.data.new$Strikes))~behavior.data$Participant.group, family = poisson, data = expert.behavior)
summary(strikes.glm)
anova(strikes.glm)
time.glm<-glm(as.numeric(as.character(behavior.data.new$Time.s.))~behavior.data$Participant.group, family = poisson, data = expert.behavior)
summary(time.glm)
anova(time.glm)

strikes.2.glm<-glm(as.numeric(as.character(behavior.data.new$Strikes))~behavior.data$Participant.group + as.numeric(as.character(behavior.data.new$Time.s.)), family = poisson, data = expert.behavior)
summary(strikes.2.glm)
anova(strikes.2.glm)

time.2.glm<-glm(as.numeric(as.character(behavior.data.new$Time))~behavior.data$Participant.group + as.numeric(as.character(behavior.data.new$Strikes)), family = poisson, data = expert.behavior)
summary(time.2.glm)
anova(time.2.glm)
#plots

plot(e.split.time,e.split.strikes) #expert time vs. strikes for split cobbles
plot(e.fail.time,e.fail.strikes) #expert time vs. strikes for failed cobbles
plot(n.split.time,n.split.strikes) #novice time vs. strikes for split cobbles
plot(n.fail.time,n.fail.strikes) #novice time vs. strikes for failed cobbles

plot(novice.time,novice.strikes) #novice time vs. novice strikes
plot(expert.time,expert.strikes) #expert time vs expert strikes
boxplot(novice.time,expert.time) #expert time vs. novice time
boxplot(novice.strikes,expert.strikes) #expert strikes vs. novice strikes

#plot expert vs novice time 
expert.novice.time<-data.frame(behavior.data.new$Time.s.)
expert.vs.novice.time.plot<-ggplot(expert.novice.time, aes(expert.novice.time, fill = Participant.group)) + geom_density(alpha = 0.2)
#by individual participant

class(behavior.data)
by.participant<-arrange(behavior.data,desc(Participant.number)) #arrange data by participant number
e1<-by.participant[(158:190),]#data for expert 1
e1.split<-subset(e1,Split.No.split == "Y") #extract data for all split events
e1.fail<-subset(e1,Split.No.split == "N") #extract data for all failed events

e2<-by.participant[(122:157),]#data for expert 2
e2.split<-subset(e2,Split.No.split == "Y")
e2.fail<-subset(e2,Split.No.split == "N")

e3<-by.participant[(89:121),]#data for expert 3
e3.split<-subset(e3,Split.No.split == "Y")
e3.fail<-subset(e3,Split.No.split == "N")

n1<-by.participant[(79:88),]#data for novice 1
n1.split<-subset(n1,Split.No.split == "Y")
n1.fail<-subset(n1,Split.No.split == "N")

n2<-by.participant[(51:58),]#data for novice 2
n2.split<-subset(n2,Split.No.split == "Y")
n2.split<-subset(n2,Split.No.split == "N")

n4<-by.participant[(41:50),]#data for novice 4
n4.split<-subset(n4,Split.No.split == "Y")
n4.fail<-subset(n4,Split.No.split == "N")

n5<-by.participant[(31:40),]#data for novice 5
n5.split<-subset(n5,Split.No.split == "Y")
n5.fail<-subset(n5,Split.No.split == "N")

n6<-by.participant[(21:30),]#data for novice 6
n6.split<-subset(n6,Split.No.split == "Y")
n6.fail<-subset(n6,Split.No.split == "N")

n7<-by.participant[(11:20),]#data for novice 7
n7.split<-subset(n7,Split.No.split == "Y")
n7.fail<-subset(n7,Split.No.split == "N")

n9<-by.participant[(1:10),]#data for novice 9
n9.split<-subset(n9,Split.No.split == "Y")
n9.fail<-subset(n9,Split.No.split == "N")

# correlations