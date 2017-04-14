## changDesCompare is a function that compares the properties and critical values of the chang approach and an 
## altered chang approach. Alternative Designs for Phase II
## Clinical Trials when Attained Sample Sizes are Different From Planned Sample Sizes
## the function takes a (planned stage 1 cv), c (planned stage 2 cv), 
##                    beta (type II error)  , alpha (type I error),
##                    n1 (planned stage 1 sample size), nt (planned total sample size)
##                    n1a (attained stage 1 sample size), nta (attained total sample size)
##                    p0 (null hypothesis response rate), p1 (alternative hypothesis response rate)
## The function will return the above parameters and expected sample size under the null and alternative for unplanned
## sample sizes, probability of early termination under null and alternative for planned and unplanned sample sizes, 
## closed form type 1 error and power for unplanned sample sizes, and simulated type I error and power for planned sample sizes.

## in order to run this function, must define the functions `changDes` and `changDesAlter`

changDesCompare <- function(r1   = 7,   rt  = 21, beta = 0.2, alpha = 0.05,
                     n1  = 17,  nt = 41, 
                     n1a = 17, nta = 41,
                     p0  = 0.4, p1 = 0.6, 
                     sim = TRUE){
                     	
	Design    <- matrix(c("Chang","Chang Alter",""), nrow=3)
	chang     <-     as.matrix(changDes(r1   = r1,   rt  = rt, beta = beta, alpha = alpha,
            				   n1  = n1,   nt = nt, 
            				   n1a = n1a, nta = nta,
              				   p0  = p0,   p1 = p1, sim = sim))
 	changAlter<- as.matrix(changDesAlter(r1   = r1,    rt  = rt, beta = beta, alpha = alpha,
            				   n1  = n1,   nt = nt, 
            			       n1a = n1a, nta = nta,
              			   	   p0  = p0,   p1 = p1, sim = sim)  )           					 
              					 
	dfCompare <- rbind(chang, changAlter, rep("", dim(chang)[2]))
	dfCompare <- data.frame(Design = Design, dfCompare)
	return(dfCompare)               	
}
#changDesCompare(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
#                     n1  = 17,  nt = 41, 
#                     n1a = 17, nta = 41,
#                     p0  = 0.4, p1 = 0.6)