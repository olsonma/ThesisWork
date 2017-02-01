## This file is used for exploratory, trial/error, and investigatory purposes as to not lose 
## valuable progress on the original file. When "trial" is complete, changUnplannedSS.R is updated
## with the results of this file. 

## currently, these files are matching. 


changDes <- function(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
                     n1  = 17,  nt = 41, 
                     n1a = 17, nta = 41,
                     p0  = 0.4, p1 = 0.6){
  
    pet0 <- pbinom(a, n1, p0) ## probability of early termination under the null
    pet1 <- pbinom(a, n1, p1)

  m     = n1a
  astar = NULL
  cstar = NULL
  
  
  ## defining beta, equation 9 in paper
  beta1 <- pbinom(a, n1, p1)
  
  betaM <- NULL
  
  if(m <= n1){
    betaM <- beta1*(m/n1)
  }
  if(m > n1){
    betaM <- beta1 + ((beta-beta1)*(m-n1))/(nt-n1)
  }	
  
  
  ## find Astar
  aRight 	  <- NULL
  probRight <- NULL
  aLeft     <- NULL
  probLeft  <- NULL
  
  for(i in 1:n1a){
    if(pbinom(i, n1a, p1) > betaM){
      aRight    <- i
      aLeft     <- i-1
      probRight <- pbinom(i, n1a, p1)
      probLeft  <- pbinom(i-1, n1a, p1)
      
      break
    }
  }
  
  astar <- ifelse( abs(probRight - betaM) < abs(probLeft - betaM), aRight, aLeft)
  
  ## find C star
  
  ## sum from a to n1* P(Y2star > rt* - y1*) < alpha
  x1       <- 0:n1a
  y0       <- dbinom(x1,n1a,p0)   ## P(X1 = x1 | n1, p0)
  type1    <- NULL
  powerObs <- NULL
  
  
  for(i in astar:nta){
    cp0 <- 1-pbinom(i-x1, nt-n1, p0)  ## conditional type I error
    cp0[x1 <= astar] <- 0 
    cp0[x1 > i]      <- 1  
    
    type1 <- sum( cp0 * y0) #sum[P(Y2* > rt*-y1*) * P(Y1* = y1*)]
    
    
    if(type1 < alpha){
      cstar <- i
      break
    }
  }
  
  ## calculate power
  y1       <- dbinom(x1,n1a,p1)   ## P(X1 = x1 | n1, p0)
  
  cp1 <- 1-pbinom(i-x1, nt-n1, p1)  ## conditional type I error
  cp1[x1 <= astar] <- 0 
  cp1[x1 > i]      <- 1  
  
  powerObs <- sum( cp1 * y1)
  
  
  ## other characteristics
  pet0star <- pbinom(astar, n1a, p0) ## probability of early termination under the null
  pet1star <- pbinom(astar, n1a, p1) ## probability of early termination under the alternative
  
  EN0star <- n1a + (1-pet0star) * (nta-n1a) ## expected sample size under null
  ## n1 + n2 * sum_r1+1^n1 dbinom(x1,n1,p0)
  EN1star <- n1a + (1-pet1star) * (nta-n1a) ## expected sample size under alternative
  
  
  ## include simulation results
  ###########################	
  ## type I error simulation
  ###########################
  r1 <- astar
  rt <- cstar
  n1sim <- n1a
  ntsim <- nta
  p0 <- p0
  p1 <- p1
  
  
  ## under null
  sims          <- 10000
  results1      <- c()
  results2      <- c()
  rejectNull    <- 0
  totalResponse <- NULL
  totalStage1   <- NULL
  totalStage2   <- NULL
  type1Sim      <- NULL
  
  for(j in 1:sims){
    
    ## set up first stage
    for(i in 1:n1sim){
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
      for(k in 1:(ntsim-n1sim)){
        results2[k] <- rbinom(1, 1, p0)
      }
      totalStage2 <- sum(results2)
      totalResponse <- totalStage1 + totalStage2
      rejectNull <- ifelse(totalResponse > rt, rejectNull + 1, rejectNull + 0)
    }
  }
  
  type1Sim <- rejectNull/sims
  
  
  
  ###########################
  ## Power simulation
  ## under alternative
  ############################
  results1      <- c()
  results2      <- c()
  rejectNull    <- 0
  totalResponse <- NULL
  totalStage1   <- NULL
  totalStage2   <- NULL
  powerSim      <- NULL
  
  for(j in 1:sims){
    
    ## set up first stage
    for(i in 1:n1sim){
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
      for(k in 1:(ntsim-n1sim)){
        results2[k] <- rbinom(1, 1, p1)
      }
      totalStage2 <- sum(results2)
      totalResponse <- totalStage1 + totalStage2
      rejectNull <- ifelse(totalResponse > rt, rejectNull + 1, rejectNull + 0)
    }
  }
  
  powerSim <- rejectNull/sims
  
  
  R <- function(x){
	  round(x, 3)
  }
  
  
  ## print results in data frame
  results <- data.frame(p0 = p0, p1 = p1, n1 = n1, n = nt, a = a, c = c,
                        alpha = alpha, power = 1-beta, 
                        pet0 = R(pet0), pet1 = R(pet1),
                        n1star = n1a, nstar = nta,
                        astar = astar, cstar = cstar, 
                        type1Obs = R(type1), powerObs = R(powerObs),
                        pet0star = R(pet0star), pet1star = R(pet1star), 
                        EN0star = R(EN0star), EN1star = R(EN1star),
                        type1Sim = R(type1Sim), powerSim = R(powerSim))
  
  return(results)					 	
}

