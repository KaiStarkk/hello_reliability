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
  if(nrow(sampleMeans)<6)
  {
    return(0)
  }
  
  for(i in 1:(nrow(sampleMeans)-5)){ 
    consecutivePoints <- 0
    
    #Are we increasing?
    for(j in i:i+5){
      if(sampleMeans[j]<sample[j+1]){
        consecutivePoints <- consecutivePoints + 1      
      }
    }
    if(consecutivePoints==6){
      return(i)
    }
    
    consecutivePoints <- 0
    
    #Are we decreasing?
    for(j in i:i+5){
      if(sampleMeans[j]>sampleMeans[j+1]){
        consecutivePoints <- consecutivePoints + 1      
      }
    }
    if(consecutivePoints==6){
      return(i)
    }
  }
  return(0)
}

#Rule 6: Out of 14 consecutive points, all 14 oscillate
sixthCheck <- function(gpAve, sampleMeans, zoneDf){
  
  #Ensure we have enough points to run this test
  if(nrow(sampleMeans)<14)
  {
    return(0)
  }
  
  for(i in 2:(nrow(sampleMeans)-13)){ #Start at the second point
    consecutivePoints <- 0 
    
    #
    for(j in i:i+14){
      #Up and down
      if(sampleMeans[j-1]<sampleMeans[j] && sampleMeans[j]<sampleMeans[j+1]){
        consecutivePoints <- consecutivePoints + 1    
      }
      #Down and up
      else if(sampleMeans[j-1]>sampleMeans[j] && sampleMeans[j]>sampleMeans[j+1]){
        consecutivePoints <- consecutivePoints + 1
      }
      #We had a set of 3 points without oscillation, go back to the first loop and start again
      else{
        break
      }
 
    }
    if(consecutivePoints==15){
      return(i)
    }
  }
  return(0)
}

#Rule 7: Out of 15 consecutive points, all 14 lie in zone C
seventhCheck <- function(gpAve, sampleMeans, zoneDf){
  
  #Ensure we have enough points to run this test
  if(nrow(sampleMeans)<15)
  {
    return(0)
  }
  
  for(i in 1:(nrow(sampleMeans)-14)){ 
    consecutivePoints <- 0 
    
    
    for(j in i:i+14){
      #Are we not in upper C?
      if(!(in.interval.lo(sampleMeans[j], zoneDf$B[1], zoneDf$C[1]  ))){
        break
      }
      #Are we not in lower C?
      else if(!(in.interval.lo(sampleMeans[j], zoneDf$C[2], zoneDf$B[2]  ))){
        break
      }
      #We were in zone C
      else{
        consecutivePoints <- consecutivePoints +1
      }
    }
    
    if(consecutivePoints == 15){
      return(i)
    }
  }
  return(0)
}
