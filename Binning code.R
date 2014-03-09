#Putting time and strike data vectors into defined 'bins'

#0-5, 5-10, 10-20, 20-40, 40-80, 80-160, 160-320+


data<-during.experiment #whole data frame
time<-data$Time.s.
strikes<-data$Strikes
time.bin <- cut(x=time, breaks=c(0,5,10,20,40,80,160,320,700), labels=c("0-5", "5-10", "10-20", "20-40", "40-80", "80-160", "160-320", "320+"), include.lowest=T) #noting the places for bin breaks and labeling them, including lowest values (e.g. 0)
time.bin<-as.numeric(time.bin) #make this vector a list of numbers for the bins

strikes.bin <- cut(x=strikes, breaks=c(0,5,10,20,40,80,160,320,1000), labels=c("0-5", "5-10", "10-20", "20-40", "40-80", "80-160", "160-320", "320+"), include.lowest=T)
strikes.bin<-as.numeric(strikes.bin)

data$time.bins<-time.bin #add binned time data to dataframe
data$strikes.bins<-strikes.bin #add binned strikes data to dataframe

write.table(data, file = "cat.data.csv", sep = ",", col.names = TRUE, qmethod = "double")
