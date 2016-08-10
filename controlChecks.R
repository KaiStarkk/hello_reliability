#Clear console screen
#cat("\014")

#Clear global (all) environmental variables
#rm(list = ls())

library(kimisc)

#Rule 1: Point outside the control limits
firstCheck <- function(gpAve, sampleMeans, zoneDf){
  
  for(i in 1:length(sampleMeans)){
    if(!(in.interval.lo(sampleMeans[i], zoneDf$A[2], zoneDf$A[1]))){
      return(i)
    }
  }
  return(0)
  
}

#Rule 2: Out of 3 consecutive points, 2 consecutive points lie in zone A
secondCheck <- function(gpAve, sampleMeans, zoneDf){
  colnames(zoneDf)<-c('C','B','A') 
  #Ensure we have enough points to run this test
  if(length(sampleMeans)<3)
  {
    return(0)
  }
 
  for(i in 1:(length(sampleMeans)-2)){
    triggered <- 0
    for(j in i:(i+2)){
      #Are we in the upper zone A?
      if(in.interval.lo(sampleMeans[j],zoneDf$B[1],zoneDf$A[1])){
        triggered <- triggered + 1
        #print("trig" + triggered)
      }
      
      #Are we in the lower zone A? 
      else if(in.interval.lo(sampleMeans[j],zoneDf$A[2],zoneDf$B[2])){
        triggered <- triggered + 1
      }
      else{
        triggered <- 0
      }
    }
    
    if(triggered>=2){
      return(i)
    }
  }
  return(0)
}


#Rule 3: Out of 5 consecutive points, 4 consecutive points lie in zone A or B
thirdCheck <- function(gpAve, sampleMeans, zoneDf){
  colnames(zoneDf)<-c('C','B','A') 
  #Ensure we have enough points to run this test
  if(length(sampleMeans)<5)
  {
    return(0)
  }
  
  for(i in 1:(length(sampleMeans)-4)){
    triggered <- 0
    for(j in i:(i+4)){
      #Are we in the upper zone A?
      if((in.interval.lo(sampleMeans[j],zoneDf$B[1],zoneDf$A[1])) ||
         (in.interval.lo(sampleMeans[j],zoneDf$C[1],zoneDf$B[1]))){
        triggered <- triggered + 1
      }
      
      #Are we in the lower zone A? 
      else if((in.interval.lo(sampleMeans[j],zoneDf$A[2],zoneDf$B[2])) ||
              (in.interval.lo(sampleMeans[j],zoneDf$B[2],zoneDf$C[2]))){
        triggered <- triggered + 1
      }
      else{
        triggered <- 0
      }
      
      if(triggered==4){
        return(i)
      }
    }
  }
  return(0)
}

#Rule 4: Out of 9 consecutive points, 9 lie on one side on the centreline
fourthCheck <- function(gpAve, sampleMeans, zoneDf){
  
  #Ensure we have enough points to run this test
  if(length(sampleMeans)<9)
  {
    return(0)
  }
  
  for(i in 1:(length(sampleMeans)-8)){
    consecutivePoints <- 0
    
    #Are we above the line?
    for(j in i:(i+8)){
      if(sampleMeans[j]>gpAve){
        consecutivePoints <- consecutivePoints + 1      
      }
    }
    if(consecutivePoints==9){
      return(i)
    }
    
    consecutivePoints <- 0
    
    #Are we below the line?
    for(j in i:i+8){
      if(sampleMeans[j]<gpAve){
        consecutivePoints <- consecutivePoints + 1      
      }
    }
    if(consecutivePoints==9){
      return(i)
    }
  }
  return(0)
}

#Rule 5: Out of 6 consecutive points, 6 are increasing or  decreasing. Use cummax or cummin ASAP
fifthCheck <- function(gpAve, sampleMeans, zoneDf){
  
  #Ensure we have enough points to run this test
  if(length(sampleMeans)<6)
  {
    return(0)
  }
  
  for(i in 1:(length(sampleMeans)-5)){ 
    
    setToCheck <- sampleMeans[i:(i+5)] 
    
    #Are we increasing?
    if(all(setToCheck == cummax(setToCheck))){
      return(i)
    }
    #Are we decreasing?
    if(all(setToCheck == cummin(setToCheck))){
      return(i)
    }
  }
  return(0)
}

#Rule 6: Out of 14 consecutive points, all 14 oscillate
sixthCheck <- function(gpAve, sampleMeans, zoneDf){
  
  #Ensure we have enough points to run this test
  if(length(sampleMeans)<14)
  {
    return(0)
  }
  
  for(i in 2:(length(sampleMeans)-12)){ #Skip the first and last point
    #The way we do our checks, we start at point 2. So if point 3 follows the correct pattern, we would have
    #gained 3 consecutive points. So start the counter at 2
    consecutivePoints <- 2 
    
    #
    for(j in i:(i+11)){ #Again, avoid going to the last value
      print(paste("j",j))
      #Up and down
      if(sampleMeans[j-1]<sampleMeans[j] && sampleMeans[j]>sampleMeans[j+1]){
        consecutivePoints <- consecutivePoints + 1
      }
      #Down and up
      else if(sampleMeans[j-1]>sampleMeans[j] && sampleMeans[j]<sampleMeans[j+1]){
        consecutivePoints <- consecutivePoints + 1
      }
      #We had a set of 3 points without oscillation, go back to the first loop and start again
      else{
        break
      }
    }
    if(consecutivePoints==14){
      return(i-1) #Since we started checking at the second point, the set really started one before
    }
  }
  return(0)
}

#Rule 7: Out of 15 consecutive points, all 14 lie in zone C
seventhCheck <- function(gpAve, sampleMeans, zoneDf){
  
  #Ensure we have enough points to run this test
  if(length(sampleMeans)<15)
  {
    return(0)
  }
  
  for(i in 1:(length(sampleMeans)-14)){ 
    consecutivePoints <- 0 
    
    
    for(j in i:(i+14)){
      #Are we in upper or lower C
      if((in.interval.lo(sampleMeans[j], gpAve, zoneDf$C[1])) ||
         (in.interval.lo(sampleMeans[j], zoneDf$C[2], gpAve   ))){
        consecutivePoints <- consecutivePoints + 1
        #print("in interval")
      }
      #We found a single point that wasn't in C so don't bother checking the whole set
      else{
        break 
      }
    }
    
    if(consecutivePoints == 15){
      return(i)
    }
  }
  return(0)
}
