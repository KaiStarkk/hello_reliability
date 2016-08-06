#Clear the environment
rm(list=ls())

#Load required libraries
library(matrixStats)

#Load in our big dataset (For now, use that other formula to generate a fake dataset )
voltages <- as.data.frame(replicate(4, rnorm(10,mean=1.31,sd=0.05)))

#Calculate X'' and R
sampleMeans <-rowMeans(voltages) 
gpAve <- mean(sampleMeans)

maxMinList <- rowRanges(as.matrix(voltages))
rangeAve<- mean(maxMinList[,2]-maxMinList[,1])

#Calculate UCL and LCL
a2 <- 0.729
ucl <- gpAve + a2*rangeAve
lcl <- gpAve - a2*rangeAve

#Determine the intervals for each zone. Store in a dataframe for easy reference later on
zoneDistance <- (ucl - gpAve)/3

cZones <- c(gpAve + zoneDistance, gpAve - zoneDistance)
bZones <- c(gpAve + 2*zoneDistance, gpAve - 2*zoneDistance)
aZones <- c(ucl, lcl)

zoneDf <- data.frame(cZones,bZones,aZones)
colnames(zoneDf) <- c('C', 'B', 'A')

#Plot X' with a clear centreline, dotted lines for each zone and a dashed line for UCL and LCL
results <- plot(sampleMeans, type = "b", ylab = "Average of each sample", xlab = "Sample", ylim = c(lcl-0.01,ucl+0.01))

#Plot the centreline
abline(h=gpAve, lty=4, col="green")
text(gpAve, results, labels="Process Average", pos=4)

#Plot LCL and UCL
abline(h=lcl, lty=2, col="red")
text(lcl, results, labels="LCL", pos=4)
abline(h=ucl, lty=2, col="red")
text(ucl, results, labels="UCL", pos=4)

#Plot the zones
abline(h=zoneDf$C[1], lty=3, col="blue")
abline(h=zoneDf$C[2], lty=3, col="blue")

abline(h=zoneDf$B[1], lty=3, col="blue")
abline(h=zoneDf$B[2], lty=3, col="blue")

text(gpAve + zoneDistance/2, results, labels="Zone C", pos=4)
text(gpAve - zoneDistance/2, results, labels="Zone C", pos=4)

text(gpAve + 1.5*zoneDistance, results, labels="Zone B", pos=4)
text(gpAve - 1.5*zoneDistance, results, labels="Zone B", pos=4)

text(gpAve + 2.5*zoneDistance, results, labels="Zone A", pos=4)
text(gpAve - 2.5*zoneDistance, results, labels="Zone A", pos=4)
