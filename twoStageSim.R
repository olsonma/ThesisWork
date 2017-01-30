###########################
r1 <- 7
rt <- 21
n1 <- changDes()$n1star
nt <- changDes()$nstar
p0 <- changDes()$p0
p1 <- changDes()$p1


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
  print(totalStage1)
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
  print(totalStage1)
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