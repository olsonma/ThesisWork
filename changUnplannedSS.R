#changDes <- function(m = 0, a = aplan, beta = betaplan, alpha = alphaplan,
#                        nt = ntplan, nta = ntatt, n1 = n1plan,
#                        n1a = n1att, p0 = p0plan, p1 = p1plan){
    
	a     = 1 
	c     = 3
	beta  = 0.1
	alpha = 0.09             
	nt    = 38
	n1    = 19
	nta   = 40
	n1a   = 21
	p0    = 0.05
	p1    = 0.2   
	m     = n1a
	astar = NULL
	   	
	
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
	

results <- data.frame(p0 = p0, p1 = p1, n1 = n1, n = nt, a = a, c = c,
					  alpha = alpha, power = 1-beta, )	
	
	
	
# }
	