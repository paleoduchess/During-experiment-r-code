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

#PERMUTATION TESTS: expert vs. novice individuals
#TIME e1 vs. n1
time.combined<-c(e1$Time.s., n1$Time.s.) #recombine the male and female glucose data sets

# create an observed difference
diff.means.time<-mean(e1$Time.s.)-mean(n1$Time.s.) #calculate the difference in their means for later comparison
number_of_permutations<- 10000 #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (time.combined, length(e1$Time.s.), TRUE)
  b.random = sample (time.combined, length(n1$Time.s.), TRUE)
  
  diff.random[i]<- mean(a.random) - mean(b.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.time)) / number_of_permutations
print (pvalue)  # < 0.001

#TIME e1 vs. n4
time.combined<-c(e1$Time.s., n4$Time.s.) #recombine the male and female glucose data sets

# create an observed difference
diff.means.time<-mean(e1$Time.s.)-mean(n4$Time.s.) #calculate the difference in their means for later comparison
number_of_permutations<- 10000 #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (time.combined, length(e1$Time.s.), TRUE)
  b.random = sample (time.combined, length(n4$Time.s.), TRUE)
  
  diff.random[i]<- mean(a.random) - mean(b.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.time)) / number_of_permutations
print (pvalue)  # < 0.001

#TIME e1 vs. n6
time.combined<-c(e1$Time.s., n6$Time.s.) #recombine the male and female glucose data sets

# create an observed difference
diff.means.time<-mean(e1$Time.s.)-mean(n6$Time.s.) #calculate the difference in their means for later comparison
number_of_permutations<- 10000 #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (time.combined, length(e1$Time.s.), TRUE)
  b.random = sample (time.combined, length(n6$Time.s.), TRUE)
  
  diff.random[i]<- mean(a.random) - mean(b.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.time)) / number_of_permutations
print (pvalue)  # < 0.001

#TIME e1 vs. n7
time.combined<-c(e1$Time.s., n7$Time.s.) #recombine the male and female glucose data sets

# create an observed difference
diff.means.time<-mean(e1$Time.s.)-mean(n7$Time.s.) #calculate the difference in their means for later comparison
number_of_permutations<- 10000 #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (time.combined, length(e1$Time.s.), TRUE)
  b.random = sample (time.combined, length(n7$Time.s.), TRUE)
  
  diff.random[i]<- mean(a.random) - mean(b.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.time)) / number_of_permutations
print (pvalue)  # < 0.001

#TIME e1 vs. n9
time.combined<-c(e1$Time.s., n9$Time.s.) #recombine the male and female glucose data sets

# create an observed difference
diff.means.time<-mean(e1$Time.s.)-mean(n9$Time.s.) #calculate the difference in their means for later comparison
number_of_permutations<- 10000 #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (time.combined, length(e1$Time.s.), TRUE)
  b.random = sample (time.combined, length(n9$Time.s.), TRUE)
  
  diff.random[i]<- mean(a.random) - mean(b.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.time)) / number_of_permutations
print (pvalue)  # < 0.001

#TIME e1 vs. n9
time.combined<-c(e1$Time.s., n10$Time.s.) #recombine the male and female glucose data sets

# create an observed difference
diff.means.time<-mean(e1$Time.s.)-mean(n10$Time.s.) #calculate the difference in their means for later comparison
number_of_permutations<- 10000 #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (time.combined, length(e1$Time.s.), TRUE)
  b.random = sample (time.combined, length(n10$Time.s.), TRUE)
  
  diff.random[i]<- mean(a.random) - mean(b.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.time)) / number_of_permutations
print (pvalue)  # < 0.001

#TIME e2 vs. n1
time.combined<-c(e2$Time.s., n1$Time.s.) #recombine the male and female glucose data sets

# create an observed difference
diff.means.time<-mean(e2$Time.s.)-mean(n1$Time.s.) #calculate the difference in their means for later comparison
number_of_permutations<- 10000 #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (time.combined, length(e2$Time.s.), TRUE)
  b.random = sample (time.combined, length(n1$Time.s.), TRUE)
  
  diff.random[i]<- mean(a.random) - mean(b.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.time)) / number_of_permutations
print (pvalue)  # < 0.001

#TIME e2 vs. n4
time.combined<-c(e2$Time.s., n4$Time.s.) #recombine the male and female glucose data sets

# create an observed difference
diff.means.time<-mean(e2$Time.s.)-mean(n4$Time.s.) #calculate the difference in their means for later comparison
number_of_permutations<- 10000 #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (time.combined, length(e2$Time.s.), TRUE)
  b.random = sample (time.combined, length(n4$Time.s.), TRUE)
  
  diff.random[i]<- mean(a.random) - mean(b.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.time)) / number_of_permutations
print (pvalue)  # < 0.001

#TIME e2 vs. n6
time.combined<-c(e2$Time.s., n7$Time.s.) #recombine the male and female glucose data sets

# create an observed difference
diff.means.time<-mean(e2$Time.s.)-mean(n7$Time.s.) #calculate the difference in their means for later comparison
number_of_permutations<- 10000 #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (time.combined, length(e2$Time.s.), TRUE)
  b.random = sample (time.combined, length(n7$Time.s.), TRUE)
  
  diff.random[i]<- mean(a.random) - mean(b.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.time)) / number_of_permutations
print (pvalue)  # < 0.001

#TIME e2 vs. n9
time.combined<-c(e2$Time.s., n9$Time.s.) #recombine the male and female glucose data sets

# create an observed difference
diff.means.time<-mean(e2$Time.s.)-mean(n9$Time.s.) #calculate the difference in their means for later comparison
number_of_permutations<- 10000 #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (time.combined, length(e2$Time.s.), TRUE)
  b.random = sample (time.combined, length(n9$Time.s.), TRUE)
  
  diff.random[i]<- mean(a.random) - mean(b.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.time)) / number_of_permutations
print (pvalue)  # < 0.001

#TIME e2 vs. n10
time.combined<-c(e2$Time.s., n10$Time.s.) #recombine the male and female glucose data sets

# create an observed difference
diff.means.time<-mean(e2$Time.s.)-mean(n10$Time.s.) #calculate the difference in their means for later comparison
number_of_permutations<- 10000 #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (time.combined, length(e2$Time.s.), TRUE)
  b.random = sample (time.combined, length(n10$Time.s.), TRUE)
  
  diff.random[i]<- mean(a.random) - mean(b.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.time)) / number_of_permutations
print (pvalue)  # < 0.001

#TIME e2 vs. n10
time.combined<-c(e3$Time.s., n1$Time.s.) #recombine the male and female glucose data sets

# create an observed difference
diff.means.time<-mean(e3$Time.s.)-mean(n1$Time.s.) #calculate the difference in their means for later comparison
number_of_permutations<- 10000 #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (time.combined, length(e3$Time.s.), TRUE)
  b.random = sample (time.combined, length(n1$Time.s.), TRUE)
  
  diff.random[i]<- mean(a.random) - mean(b.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.time)) / number_of_permutations
print (pvalue)  # < 0.001

#TIME e2 vs. n10
time.combined<-c(e3$Time.s., n4$Time.s.) #recombine the male and female glucose data sets

# create an observed difference
diff.means.time<-mean(e3$Time.s.)-mean(n4$Time.s.) #calculate the difference in their means for later comparison
number_of_permutations<- 10000 #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (time.combined, length(e3$Time.s.), TRUE)
  b.random = sample (time.combined, length(n4$Time.s.), TRUE)
  
  diff.random[i]<- mean(a.random) - mean(b.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.time)) / number_of_permutations
print (pvalue)  # < 0.001

#TIME e2 vs. n10
time.combined<-c(e3$Time.s., n6$Time.s.) #recombine the male and female glucose data sets

# create an observed difference
diff.means.time<-mean(e3$Time.s.)-mean(n6$Time.s.) #calculate the difference in their means for later comparison
number_of_permutations<- 10000 #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (time.combined, length(e3$Time.s.), TRUE)
  b.random = sample (time.combined, length(n6$Time.s.), TRUE)
  
  diff.random[i]<- mean(a.random) - mean(b.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.time)) / number_of_permutations
print (pvalue)  # < 0.001

#TIME e2 vs. n10

time.combined<-c(e3$Time.s., n7$Time.s.) #recombine the male and female glucose data sets

# create an observed difference
diff.means.time<-mean(e3$Time.s.)-mean(n7$Time.s.) #calculate the difference in their means for later comparison
number_of_permutations<- 10000 #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (time.combined, length(e3$Time.s.), TRUE)
  b.random = sample (time.combined, length(n7$Time.s.), TRUE)
  
  diff.random[i]<- mean(a.random) - mean(b.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.time)) / number_of_permutations
print (pvalue)  # < 0.001

#TIME e2 vs. n10
time.combined<-c(e3$Time.s., n9$Time.s.) #recombine the male and female glucose data sets

# create an observed difference
diff.means.time<-mean(e3$Time.s.)-mean(n9$Time.s.) #calculate the difference in their means for later comparison
number_of_permutations<- 10000 #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (time.combined, length(e3$Time.s.), TRUE)
  b.random = sample (time.combined, length(n9$Time.s.), TRUE)
  
  diff.random[i]<- mean(a.random) - mean(b.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.time)) / number_of_permutations
print (pvalue)  # < 0.001

#TIME e2 vs. n10
time.combined<-c(e3$Time.s., n10$Time.s.) #recombine the male and female glucose data sets

# create an observed difference
diff.means.time<-mean(e3$Time.s.)-mean(n10$Time.s.) #calculate the difference in their means for later comparison
number_of_permutations<- 10000 #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (time.combined, length(e3$Time.s.), TRUE)
  b.random = sample (time.combined, length(n10$Time.s.), TRUE)
  
  diff.random[i]<- mean(a.random) - mean(b.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.time)) / number_of_permutations
print (pvalue)  # < 0.001
