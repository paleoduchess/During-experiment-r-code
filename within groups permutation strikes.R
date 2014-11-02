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
data.splits<-subset(data, outcome==2) #subset data for only successful splits
data$Participant.number<-factor(data$Participant.number)
data$Participant.group<-factor(data$Participant.group)

e1<-subset(data.splits, Participant.number=="E1")
e2<-subset(data.splits, Participant.number=="E2")
e3<-subset(data.splits, Participant.number=="E3")
n1<-subset(data.splits, Participant.number=="N1")
n2<-subset(data.splits, Participant.number=="N2")
n3<-subset(data.splits, Participant.number=="N3")
n4<-subset(data.splits, Participant.number=="N4")
n5<-subset(data.splits, Participant.number=="N5")
n6<-subset(data.splits, Participant.number=="N6")
n7<-subset(data.splits, Participant.number=="N7")
n8<-subset(data.splits, Participant.number=="N8")
n9<-subset(data.splits, Participant.number=="N9")
n10<-subset(data.splits, Participant.number=="N10")
#individual permutation tests for STRIKES
#Strikes e1 vs. n1
strikes.combined<-c(e3$Strikes, n10$Strikes) #recombine the male and female glucose data sets

# create an observed difference
diff.means.strikes<-mean(e3$Strikes)-mean(n10$Strikes) #calculate the difference in their means for later comparison
number_of_permutations<- 10000 #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (strikes.combined, length(e3$Strikes), TRUE)
  b.random = sample (strikes.combined, length(n10$Strikes), TRUE)
  
  diff.random[i]<- mean(a.random) - mean(b.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.strikes)) / number_of_permutations
print (pvalue)  # < 0.001

