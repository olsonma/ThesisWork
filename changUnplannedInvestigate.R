changDes <- function(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
					 n1  = 17,  nt = 41, 
					 n1a = 17, nta = 41,
					 p0  = 0.4, p1 = 0.6){
      
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
	x1       <- astar:n1a
	y1       <- 0:n1a
	type1    <- NULL
	powerObs <- NULL
	
	for(i in astar:nta){
		cp0 <- 1-pbinom(i-y1, x1, p0)  # P(Y2* > rt*-y1*)
		#cp0[y1 <= astar] <- 0
		#cp0[y1 > i]      <- 1 
		
		type1 <- sum( cp0 * dbinom(y1, n1a, p0)) #sum[P(Y2* > rt*-y1*) * P(Y1* = y1*)]

	
		if(type1 < alpha){
			cstar <- i
			break
		}
	}
	
	## calculate power	
	
	cp1 <- 1-pbinom(i-y1, x1, p1)  # P(Y2* > rt*-y1*)
	#cp1[y1 <= astar] <- 0
	#cp1[y1 > i]      <- 1 
	
	powerObs <- sum( cp1 * dbinom(y1, n1a, p1)) #sum[P(Y2* > rt*-y1*) * P(Y1* = y1*)]


	## print results in data frame
	results <- data.frame(p0 = p0, p1 = p1, n1 = n1, n = nt, a = a, c = c,
						  alpha = alpha, power = 1-beta, 
						  n1star = n1a, nstar = nta,
						  astar = astar, cstar = cstar, 
						  type1Obs = type1, powerObs = powerObs)
					  
	return(results)					 	
}
	
changDes()
