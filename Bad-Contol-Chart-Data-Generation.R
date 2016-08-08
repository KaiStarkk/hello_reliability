#Clear console screen
cat("\014")

#Clear global (all) environmental variables
rm(list = ls())

#Grand process mean
gpMean <- 50

#Process control limits
LCL <- 30
UCL <- 70

#Zones
stdev = (UCL - gpMean)/3
zone.C.U =gpMean + stdev
zone.C.L = gpMean - stdev
zone.B.U = gpMean + 2*stdev
zone.B.L = gpMean - 2*stdev
# zone.A.U = UCL
# zone.A.L = LCL

#1 - One or more poitns fall outside control limits
bad1 = runif(2,UCL,UCL + stdev) #All data points above

#2 - Two out of three consecutive points are in zone A (3std)
bad2 = runif(3,zone.B.U+1,UCL-1)

#3 - Four out of five conseuctive points are in zone A or B
bad3 = runif(5,zone.C.U+1,UCL-1)

#4 - Nine conseuctive points are on one side of the average.
bad4 = runif(9,gpMean+1,UCL-1)

#5 - Six consective points are increasing or decreasing
bad5 = seq(from=LCL+1,to=UCL-1,length.out=6)
           
#6 - Fourteen consecutive points alternatie up and down
bad6=array(data = NA, dim = 14, dimnames = NULL)
for (i in 1:15){
  if((i %% 2) == 0){
    #Even numbers
    bad6[[i]] = runif(1, LCL+1, gpMean-1)
  } else {
    #Odd numbers
    bad6[[i]] = runif(1, gpMean+1, UCL)
  }
}

#7 - Fifteen consecutive points within Zone C (1std)
bad7 = runif(15,zone.C.L+1,zone.C.U-1)

#Combine together (just for fun)
badToTheBone = list(bad1, bad2, bad3, bad4, bad5, bad6, bad7)

#Write to CSV
write.csv(badToTheBone, file ="BadToTheBone.csv")
