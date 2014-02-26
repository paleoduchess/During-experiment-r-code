#ANOVA for time
#not a normal distribution

data<-during.experiment #whole data frame
time<-as.numeric(as.character(data$Time.s.)) #time for whole dataset
strikes<-as.numeric(as.character(data$Strikes)) #strikes for whole dataset
expert<-subset(data, Participant.group =="Expert") #subset data for experts only
expert.time<-as.numeric(as.character(expert$Time.s.)) #numeric time data for experts only
novice<-subset(data, Participant.group == "Novice") #subset data for novices only
novice.time<-as.numeric(as.character(novice$Time.s.)) #numeric time data for novices only
expert.strikes<-as.numeric(as.character(expert$Strikes)) #numeric strikes data for experts only
novice.strikes<-as.numeric(as.character(novice$Strikes)) #numeric strikes data for novices only

#ANOVAS for time with whole dataset

kruskalmc(time ~ data$Participant.group,probs=0.001, data = data) #anova for time by group
kruskalmc(time ~ data$Participant.number,probs=0.001, data = data) #anova for time by individual participant

kruskalmc(expert.time~ expert$Participant.number,probs=0.001, data = expert) #ANOVA for just experts
kruskalmc(novice.time$Time.s. ~ novice.time$Participant.number,probs=0.001, data = novice.time) #ANOVA for just novices

#ANOVAS for strikes
kruskalmc(strikes ~ data$Participant.group,probs=0.001, data = data) #anova for strikes by group
kruskalmc(strikes ~ data$Participant.number,probs=0.001, data = data) #anova for time by individual participant

kruskalmc(expert.time~ expert$Participant.number,probs=0.001, data = expert) #ANOVA for just experts
kruskalmc(novice.time$Time.s. ~ novice.time$Participant.number,probs=0.001, data = novice.time) #ANOVA for just novices

##GLM is a complicated output so I think we can stick with kruskalmc, but there is some glm code below to work with if we wish
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

##ANOVAs for efficiency
kruskalmc(efficiency~group) #ANOVA by group
kruskalmc(efficiency~participant) #ANOVA by participant

