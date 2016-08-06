library(kimisc)

#Rule 1: Point outside the control limits
firstCheck <- function(gpAve, sampleMeans, zoneDf){
  
  for(i in 1:nrow(sampleMeans)){
    if(!(in.interval.lo(sampleMeans[i], zoneDf$A[2], zoneDf$A[1]))){
      return(i)
    }
  }
  return(0)
  
}

#Rule 2: Out of 3 consecutive points, 2 consecutive points lie in zone A
secondCheck <- function(gpAve, sampleMeans, zoneDf){
  
  #Ensure we have enough points to run this test
  if(nrow(sampleMeans)<=2)
  {
    return(TRUE)
  }
  
  for(i in 1:(nrow(sampleMeans)-2)){
    triggered <- 0
    for(j in i:i+2){
      #Are we in the upper zone A?
      if(!(in.interval.lo(sampleMeans[j],zoneDf$B[1],zoneDf$A[1]))){
        triggered <- triggered + 1
      }
      
      #Are we in the lower zone A? 
      else if(!(in.interval.lo(sampleMeans[j],zoneDf$A[2],zoneDf$B[2]))){
        triggered <- triggered + 1
      }
      else{
        triggered <- 0
      }
      
      if(triggered==2){
        return(j)
      }
    }
  }
  return(TRUE)
}


#Rule 2: Out of 5 consecutive points, 4 consecutive points lie in zone A or B
thirdCheck <- function(gpAve, sampleMeans, zoneDf){
  
  #Ensure we have enough points to run this test
  if(nrow(sampleMeans)<=4)
  {
    return(TRUE)
  }
  
  for(i in 1:(nrow(sampleMeans)-2)){
    triggered <- 0
    for(j in i:i+4){
      #Are we in the upper zone A?
      if(!(in.interval.lo(sampleMeans[j],zoneDf$B[1],zoneDf$A[1])) ||
         !(in.interval.lo(sampleMeans[j],zoneDf$C[1],zoneDf$B[1]))){
        triggered <- triggered + 1
      }
      
      #Are we in the lower zone A? 
      else if(!(in.interval.lo(sampleMeans[j],zoneDf$A[2],zoneDf$B[2])) ||
              !(in.interval.lo(sampleMeans[j],zoneDf$B[2],zoneDf$C[2]))){
        triggered <- triggered + 1
      }
      else{
        triggered <- 0
      }
      
      if(triggered==4){
        return(j)
      }
    }
  }
  return(TRUE)
} 