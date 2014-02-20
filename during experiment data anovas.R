#ANOVA for time
#not a normal distribution

data<-During.experiment.recording_fixed02.11.2014
data<-data[-(189:190),]
time<-as.numeric(as.character(data$Time.s.))
expert.time<-subset(data, Participant.group =="Expert") #subset time data for experts only
expert.time$Time.s.=as.numeric(as.character(expert.time$Time.s.)) #make time data numeric

novice.time<-subset(data, Participant.group == "Novice") #subset time data for novices only
novice.time$Time.s.=as.numeric(as.character(novice.time$Time.s.)) #make time data numeric

#ANOVAS for time

kruskalmc(time ~ data$Participant.group,probs=0.001, data = data) #anova for time by participant group
kruskalmc(time ~ data$Participant.number,probs=0.001, data = data) #anova for time by individual participant

kruskalmc(expert.time$Time.s. ~ expert.time$Participant.number,probs=0.001, data = expert.time) #ANOVA for just experts
kruskalmc(novice.time$Time.s. ~ novice.time$Participant.number,probs=0.001, data = novice.time) #ANOVA for just novices
#No differences between individuals within expert or novice groups. But expert and novice groups do differ.

#ANOVAS for strikes
data<-During.experiment.recording_fixed02.11.2014
data<-data[-(189:190),]
strikes<-as.numeric(as.character(data$Strikes))

##use glm with anova to create an analysis of variance table, test using chisq

#anova for strikes between groups
s.glm.among<-glm(strikes~data$Participant.group, family = poisson, data = data)
summary(s.glm.among)
plot(s.glm.among)
summary(anova(s.glm.among))
anova(s.glm.among)
anova(s.glm.among, test = "Chisq")

#anova for strikes between individuals
s.glm.within<-glm(strikes~data$Participant.number, family = poisson, data = data)
summary(s.glm.within)
plot(s.glm.within)
summary(anova(s.glm.within))
anova(s.glm.within)
anova(s.glm.within, test = "Chisq")

#anova for strikes in experts only

expert.strike<-subset(data, Participant.group == "Expert") #subset data for experts only

expert.strike$Strikes=as.numeric(as.character(expert.strike$Strikes)) #make sure strikes column is numeric

s.glm.expert<-glm(expert.strike$Strikes~expert.strike$Participant.number, family=poisson, data=expert.strike)
summary(s.glm.expert)
plot(s.glm.expert)
anova(s.glm.expert)
summary(anova(s.glm.expert))
anova(s.glm.expert, test = "Chisq")

#Anova for strikes with just novice data
novice.strike<-subset(data, Participant.group =="Novice") #subset data for novices only

novice.strike$Strikes=as.numeric(as.character(novice.strike$Strikes))# make sure strikes column is numeric

s.glm.novice<-glm(novice.strike$Strikes~novice.strike$Participant.number, family=poisson, data=novice.strike) #glm for novice
summary(s.glm.novice)
plot(s.glm.novice)
anova(s.glm.novice)
summary(anova(s.glm.novice))
anova(s.glm.novice, test = "Chisq")



