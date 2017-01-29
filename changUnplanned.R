changDes <- function(m = mc, a = aplan, beta = betaplan, alpha = alphaplan,
                        nt = ntplan, nta = ntatt, n1 = n1plan,
                        n1a = n1att, p0 = p0plan, p1 = p1plan){
                        	
	
	## defining beta, equation 9 in paper
	beta1 <- pbinom(a, n1, p1)
	betaM <- NULL
	
	if(m <= n1){
		betaM <- beta1*(m/n1)
	}
	if(m > n1){
		betaM <- beta1 + ((beta-beta1)*(m-n1))/(m-n1)
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
	
}
	
	
				
							
							
							
     
	 
