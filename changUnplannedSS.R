changDes <- function(){
    
	a     = 1 
	c     = 3
	beta  = 0.1
	alpha = 0.09             
	nt    = 38
	n1    = 19
	nta   = 36
	n1a   = 17
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
	
	## sum from a to n1* P(Y2star > rt* - y1*) < alpha
	x1    <- astar:n1a
	type1 <- NULL
	
	for(i in astar:nta){
		cp0 <- 1-pbinom(i-x1, nta, p0)  # P(Y2* > rt*-y1*)
		
		cp0[x1 > i] <- 1
		
		type1 <- sum( cp0 * dbinom(x1, n1a, p0)) #sum[P(Y2* > rt*-y1*) * P(Y1* = y1*)]
	
		if(type1 < alpha){
			cstar <- i
			break
		}
	}
	
	## calculate power	
	cp1 <- 1-pbinom(i-x1, nta, p1)
	cp1[x1 > i] <- 1
	
	powerObs <- sum( cp1 * dbinom(x1, n1a, p1))

	## print results in data frame
results <- data.frame(p0 = p0, p1 = p1, n1 = n1, n = nt, a = a, c = c,
					  alpha = alpha, power = 1-beta, 
					  n1star = n1a, nstar = nta,
					  astar = astar, cstar = cstar, 
					  type1Obs = type1, powerObs = powerObs)
					  
return(results)					 	
	
	
	
}
	
	
	
	
## type I error simulation
r1 <- changDes()$astar
rt <- changDes()$cstar
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
	
	if(totalStage1 <= r1){
		## stop
		rejectNull <- rejectNull + 0
	}
	
	if(totalStage1 > r1){
		## go to second stage
		
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


## under alternative
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
		results1[i] <- rbinom(1, 1, p1)
	}
	totalStage1 <- sum(results1)
	
	if(totalStage1 <= r1){
		## stop
		rejectNull <- rejectNull + 0
	}
	
	if(totalStage1 > r1){
		## go to second stage
		
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

changDes()

