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
outcome<-data$Split.No.split
outcome=as.numeric(outcome)
data$efficiency <- ((time/strikes)*(data$Prob.success.)) #creating efficiency index and attaching to data frame ##change to include probabilty of success - Time/Strikes*prob(success)# need new column that takes number of successes divided by number of cobbles for each participant
efficiency<-data$efficiency #naming efficiency column

#ANOVAS for time with whole dataset

kruskalmc(time ~ data$Participant.group,probs=0.001, data = data) #anova for time by group
kruskalmc(time ~ data$Participant.number,probs=0.001, data = data) #anova for time by individual participant

kruskalmc(expert.time~ expert$Participant.number,probs=0.001, data = expert) #ANOVA for just experts
kruskalmc(novice.time$Time.s. ~ novice.time$Participant.number,probs=0.001, data = novice.time) #ANOVA for just novices
#ANOVAS for time with splits only
kruskalmc(data.splits$Time.s. ~ data.splits$Participant.group,probs=0.001, data = data.splits) #anova for time by group
kruskalmc(data.splits$Time.s. ~ data.splits$Participant.number,probs=0.001, data = data.splits) #anova for time by individual participant

#ANOVAS for strikes
kruskalmc(strikes ~ data$Participant.group,probs=0.001, data = data) #anova for strikes by group
kruskalmc(strikes ~ data$Participant.number,probs=0.001, data = data) #anova for time by individual participant

kruskalmc(expert.time~ expert$Participant.number,probs=0.001, data = expert) #ANOVA for just experts
kruskalmc(novice.time$Time.s. ~ novice.time$Participant.number,probs=0.001, data = novice.time) #ANOVA for just novices

#ANOVAs for strikes with splits only
kruskalmc(data.splits$Strikes ~ data.splits$Participant.group,probs=0.001, data = data.splits) #anova for strikes by group
kruskalmc(data.splits$Strikes ~ data.splits$Participant.number,probs=0.001, data = data.splits) #anova for time by individual participant

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

#do same stuff for efficiency, but only for successes
data.splits<-subset(data, outcome==2)
kruskalmc(data.splits$efficiency~data.splits$Participant.group)
kruskalmc(data.splits$efficiency~data.splits$Participant.number)

time.among<-kruskalmc(time ~ data$Participant.group,probs=0.001, data = data) #anova for time by group
time.within<-kruskalmc(time ~ data$Participant.number,probs=0.001, data = data) #anova for time by individual participant
strikes.among<-kruskalmc(strikes ~ data$Participant.group,probs=0.001, data = data) #anova for strikes by group
strikes.within<-kruskalmc(strikes ~ data$Participant.number,probs=0.001, data = data) #anova for time by individual participant
efficiency.among<-kruskalmc(efficiency~data$Participant.group) #ANOVA by group
efficiency.within<-kruskalmc(efficiency~data$Participant.number) #ANOVA by participant

success.efficiency.among<-kruskalmc(data.splits$efficiency~data.splits$Participant.group) #anova for successful efficiency among groups
success.efficiency.within<-kruskalmc(data.splits$efficiency~data.splits$Participant.number) #anova for successful efficienct within groups
success.strikes.among<-kruskalmc(data.splits$Strikes~data.splits$Participant.group) #anova for successful strikes among groups
success.strikes.within<-kruskalmc(data.splits$Strikes~data.splits$Participant.number) #anova for successful strikes within groups
success.time.among<-kruskalmc(data.splits$Time.s.~data.splits$Participant.group) #anova for successful times among groups
success.time.within<-kruskalmc(data.splits$Time.s.~data.splits$Participant.number) #anova for successful times within groups

#SUMMARY TABLES
write.table(time.among, file = "time.among.csv", sep = ",", col.names = TRUE, qmethod = "double")
write.table(time.within, file = "time.within.csv", sep = ",", col.names = TRUE, qmethod = "double")
write.table(strikes.among, file = "strikes.among.csv", sep = ",", col.names = TRUE, qmethod = "double")
write.table(strikes.within, file = "strikes.within.csv", sep = ",", col.names = TRUE, qmethod = "double")
write.table(efficiency.among, file = "efficiency.among.csv", sep = ",", col.names = TRUE, qmethod = "double")
write.table(efficiency.within, file = "efficiency.within.csv", sep = ",", col.names = TRUE, qmethod = "double")
write.table(success.efficiency.among, file = "success.efficiency.among.csv", sep = ",", col.names = TRUE, qmethod = "double")
write.table(success.efficiency.within, file = "success.efficiency.within.csv", sep = ",", col.names = TRUE, qmethod = "double")
write.table(success.strikes.among, file = "success.strikes.among.csv", sep = ",", col.names = TRUE, qmethod = "double")
write.table(success.strikes.within, file = "success.strikes.within.csv", sep = ",", col.names = TRUE, qmethod = "double")
write.table(success.time.among, file = "success.time.among.csv", sep = ",", col.names = TRUE, qmethod = "double")
write.table(success.time.within, file = "success.time.within.csv", sep = ",", col.names = TRUE, qmethod = "double")
