## This file is a simulation of two stage simon design
## redefine the parameters below accordingly. 
## This simulation is used in the function `changDes`, but they are
## defined as "unplanned" parameters in `changDes`. i.e the parameters are
## probably renamed in the function. 
## this was used for trial and error purposes. 

###########################	
## type I error simulation
###########################
r1 <- 8
rt <- 21
n1 <- 19
nt <- 43
p0 <- .4
p1 <- .6


## under null
results1 <- c()
results2 <- c()
rejectNull <- 0
totalResponse <- NULL
totalStage1   <- NULL
totalStage2   <- NULL

for(j in 1:1000){
  
  ## set up first stage
  for(i in 1:n1){
    ## get a number of responses
    results1[i] <- rbinom(1, 1, p0)
  }
  
  totalStage1 <- sum(results1)
  #print(totalStage1)
      if(totalStage1 <= r1){
        rejectNull <- rejectNull + 0
      }
  
  if(totalStage1 > r1){ ## go to second stage
    
    ## enroll n2 patients more
    for(k in 1:(nt-n1)){
      results2[k] <- rbinom(1, 1, p0)
    }
    totalStage2 <- sum(results2)
    totalResponse <- totalStage1 + totalStage2
    rejectNull <- ifelse(totalResponse > rt, rejectNull + 1, rejectNull + 0)
  }
}

rejectNull/1000



###########################
## Power simulation
## under alternative
############################
## under null
results1      <- c()
results2      <- c()
rejectNull    <- 0
totalResponse <- NULL
totalStage1   <- NULL
totalStage2   <- NULL

for(j in 1:1000){
  
  ## set up first stage
  for(i in 1:n1){
    ## get a number of responses
    results1[i] <- rbinom(1, 1, p1)
  }
  
  totalStage1 <- sum(results1)
  #print(totalStage1)
      if(totalStage1 <= r1){
        rejectNull <- rejectNull + 0
      }
  
  if(totalStage1 > r1){ ## go to second stage
    
    ## enroll n2 patients more
    for(k in 1:(nt-n1)){
      results2[k] <- rbinom(1, 1, p1)
    }
    totalStage2 <- sum(results2)
    totalResponse <- totalStage1 + totalStage2
    rejectNull <- ifelse(totalResponse > rt, rejectNull + 1, rejectNull + 0)
  }
}

rejectNull/1000