dfAlter <- NULL

dfAlter <- changDesAlter(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
               n1  = 17,  nt = 41, 
               n1a = 17, nta = 41,
               p0  = 0.4, p1 = 0.6)

dfAlter <- rbind(dfAlter, changDesAlter(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
                         n1  = 17,  nt = 41, 
                         n1a = 19, nta = 41, ## keep original sample size the same
                         p0  = 0.4, p1 = 0.6))	

dfAlter <- rbind(dfAlter, changDesAlter(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
                         n1  = 17,  nt = 41, 
                         n1a = 19, nta = 43, ## keep original stage 2 sample size the same
                         p0  = 0.4, p1 = 0.6))	

dfAlter <- rbind(dfAlter, changDesAlter(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
                         n1  = 17,  nt = 41, 
                         n1a = 21, nta = 41, ## keep original sample size the same
                         p0  = 0.4, p1 = 0.6))	

dfAlter <- rbind(dfAlter, changDesAlter(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
                         n1  = 17,  nt = 41, 
                         n1a = 21, nta = 45, ## keep original stage 2 sample size the same
                         p0  = 0.4, p1 = 0.6))

dfAlter <- rbind(dfAlter, changDesAlter(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
                         n1  = 17,  nt = 41, 
                         n1a = 23, nta = 41, ## keep original sample size the same
                         p0  = 0.4, p1 = 0.6))	

dfAlter <- rbind(dfAlter, changDesAlter(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
                         n1  = 17,  nt = 41, 
                         n1a = 21, nta = 47, ## keep original stage 2 sample size the same
                         p0  = 0.4, p1 = 0.6))	

dfAlter <- rbind(dfAlter, changDesAlter(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
                         n1  = 17,  nt = 41, 
                         n1a = 16, nta = 41, ## keep original sample size the same
                         p0  = 0.4, p1 = 0.6))	

dfAlter <- rbind(dfAlter, changDesAlter(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
                         n1  = 17,  nt = 41, 
                         n1a = 16, nta = 40, ## keep original stage 2 sample size the same
                         p0  = 0.4, p1 = 0.6))	

dfAlter <- rbind(dfAlter, changDesAlter(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
                         n1  = 17,  nt = 41, 
                         n1a = 18, nta = 41, ## keep original sample size the same
                         p0  = 0.4, p1 = 0.6))	

dfAlter <- rbind(dfAlter, changDesAlter(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
                         n1  = 17,  nt = 41, 
                         n1a = 18, nta = 42, ## keep original stage 2 sample size the same
                         p0  = 0.4, p1 = 0.6))	

dfAlter <- rbind(dfAlter, changDesAlter(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
                         n1  = 17,  nt = 41, 
                         n1a = 20, nta = 41, ## keep original sample size the same
                         p0  = 0.4, p1 = 0.6))	

dfAlter <- rbind(dfAlter, changDesAlter(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
                         n1  = 17,  nt = 41, 
                         n1a = 20, nta = 44, ## keep original stage 2 sample size the same
                         p0  = 0.4, p1 = 0.6))	


dfAlter <- rbind(dfAlter, changDesAlter(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
                         n1  = 17,  nt = 41, 
                         n1a = 10, nta = 41, 
                         p0  = 0.4, p1 = 0.6))	
dfAlter <- rbind(dfAlter, changDesAlter(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
                         n1  = 17,  nt = 41, 
                         n1a = 10, nta = 34,
                         p0  = 0.4, p1 = 0.6))	
                         
dfAlter <- rbind(dfAlter, changDesAlter(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
                         n1  = 17,  nt = 41, 
                         n1a = 24, nta = 48, ## keep original stage 2 sample size the same
                         p0  = 0.4, p1 = 0.6))	
                         
								   