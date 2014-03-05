#ANOVA tables
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

data$Participant.number<-factor(data$Participant.number)
data$Participant.group<-factor(data$Participant.group)
#NON PARAMETRIC ANOVA

library(pgirmess)

time.among<-kruskalmc(time ~ data$Participant.group,probs=0.001, data = data) #anova for time by group
time.within<-kruskalmc(time ~ data$Participant.number,probs=0.001, data = data) #anova for time by individual participant
strikes.among<-kruskalmc(strikes ~ data$Participant.group,probs=0.001, data = data) #anova for strikes by group
strikes.within<-kruskalmc(strikes ~ data$Participant.number,probs=0.001, data = data) #anova for time by individual participant
efficiency.among<-kruskalmc(efficiency~data$Participant.group) #ANOVA by group
efficiency.within<-kruskalmc(efficiency~data$Participant.number) #ANOVA by participant

data.splits<-subset(data, outcome==2) #subset data for only successful splits
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
