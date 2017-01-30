changDes <- function(a   = 1,   c  = 5, beta = 0.1, alpha = 0.09,
                     n1  = 17,  nt = 29, 
                     n1a = 17, nta = 29,
                     p0  = 0.1, p1 = 0.3){
  
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
  y0       <- dbinom(x1,n1,p0)   ## P(X1 = x1 | n1, p0)
  type1    <- NULL
  powerObs <- NULL
  
  


  cp0 <- 1-pbinom(rt-x1, nt-n1, p0)  ## conditional type I error

  ## sum from rt-x1+1 to n2
  ## if X1 > rt, the CP is 1, if X1 <= rt, 0


  type1 <- sum( y0 * cp0 ) ## unconditional type I error
  
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
  y1       <- dbinom(x1,n1,p1)   ## P(X1 = x1 | n1, p0)
  
  cp1 <- 1-pbinom(i-x1, nt-n1, p1)  ## conditional type I error
  cp1[x1 <= astar] <- 0 
  cp1[x1 > i]      <- 1  
  
  powerObs <- sum( cp1 * y1)
  
  ## print results in data frame
  results <- data.frame(p0 = p0, p1 = p1, n1 = n1, n = nt, a = a, c = c,
                        alpha = alpha, power = 1-beta, 
                        n1star = n1a, nstar = nta,
                        astar = astar, cstar = cstar, 
                        type1Obs = type1, powerObs = powerObs)
  
  return(results)					 	
}
changDes()