
### Instructions:
## Copy and run this whole page of code.
## It will create two functions: bin.lik and rr.lik.
## This functions will create the likelihood plots for theta and RR.
## Inputs: bin.lik(x, n)  rr.lik(x1, n1, x2, n2)

# This function will plot the likelihood for a one-sample proportion (Binomial likelihood)
bin.lik <- function(x,n,like.only=F,acc = 1,lolim=0,hilim=1,plotcol='black',
                    main1 = "Likelihoods: Binomial Model"){
  
  p <- seq(lolim, hilim, length = 1000 * acc)
  like <- exp(x*log(p) + (n-x) * log(1 - p))
  like <- like/max(like)
  
  if (like.only==T) {
    plot(p, like, type = "n", xlab = "Probability", ylab = "", main = main1)
    lines(p,like,type="l", col=plotcol)
  }
  if (like.only==F) {
    plot(p, like, type = "n", xlab = "Probability", ylab = "", main = main1)
    lines(p,like,type="l", col=plotcol)
    
    p1 <- p[like >= 1/8]
    p2 <- p[like >= 1/32]
    i1 <- rep(1/8, length(p1))
    i2 <- rep(1/32, length(p2))
    
    lines(p1, i1, type = "l", col=plotcol)
    lines(p2, i2, type = "l", col=plotcol)
    
    whr <- if(p[like == max(like)] <= (lolim + hilim
    )/2) quantile(p, 0.8) else quantile(p, 0.2)
    text(whr, 0.95, paste("Max at", signif(c(p[like == 
                                                 max(like)]), digits = 2)),cex=.8)
    text(whr, 0.91, paste("1/8 SI (", round(min(p1), 
                                            digits = 2), ",", round(max(p1), digits = 2), 
                          ")"),cex=.8)
    text(whr, 0.87, paste("1/32 SI (", round(min(p2), 
                                             digits = 2), ",", round(max(p2), digits = 2), 
                          ")"),cex=.8)}
}




# This function will plot the likelihood for a two-sample relative risk (Negative Binomial likelihood)
rr.lik <- function(x,m,y,n,lolim=0,hilim=6, like.only=F,acc=1,plotcol='black',
                   main1="Conditional Likelihood: Relative Risk (Probability Ratio)"){
  
  ## contional likelihood on total number of successes; negative Binomial
  ## x successes out of m trials and y successes out of n trials
  ## lolim is the lower limit of the relative risk axis
  ## hilim is the lower limit of the relative risk axis
  
  if((m - x) * (n - y) == 0)
    "Conditional model requires at least one failure in each group."
  else {
    z <- seq(lolim, hilim, len = 1000 * acc)
    like <- matrix(1,nrow=length(z), ncol=1) 
    j <- seq(m-x,m+y,1)	## summation index
    Mx <- matrix(z, nrow = length(j), ncol = length(z), byrow=T)
    Mx <- Mx^(j - m)
    
    one=lgamma(m+n-j-1+1)-lgamma(n-y-1+1)-lgamma((m+n-j-1)-(n-y-1)+1)
    two=lgamma(j-1+1)-lgamma(m-x-1+1)-lgamma((j-1)-(m-x-1)+1)
    co=exp(one-max(one))*exp(two-max(two))
    
    ## old coefficient function: co <- choose(m+n-j-1,(n-y)-1)*choose(j-1,(m-x)-1)
    
    like <- 1/(t(Mx) %*% co)
    
    if(y== 0) like <- like * exp(co[length(j)])
    else like <- like/max(like)
    
    rrhat <- z[like == max(like)]
    rrhat <- round(rrhat, digits = 2)
    
    if(like[length(z)] == max(like)) rrhat <- NA
    if(like[1] == max(like)) rrhat <- NA
    
    L1 <- round(min(max(like)/like[abs(z - 1) == min(abs(z - 1))],1000000), digits = 1)
    
    z2 <- z[like >= 1/8]
    z3 <- z[like >= 1/32]
    i2 <- rep(1/8, length(z2))
    i3 <- rep(1/32, length(z3)) 
    
    if (like.only==T) 
    {
      plot(z, like, type = "n", xlab = "Relative Risk ", ylab ="Likelihood",main=main1) 
      lines(z, like, lty=1,col=plotcol)
    }
    
    if (like.only==F) {
      plot(z, like, type = "n", xlab = "Relative Risk ", ylab ="Likelihood",main=main1)
      lines(z, like, lty=1,col=plotcol)
      lines(z2, i2, type = "l",col=plotcol)
      lines(z3, i3, type = "l",col=plotcol)
      text(z[.85*1000*acc], 0.94, paste("Max at ", rrhat),cex=0.8)
      
      text(z[.85*1000*acc], 0.9, paste("1/8 SI (", 
                                       if(min(z2) == z[1]) "NA" else round(min(z2), digits = 2), ",",
                                       if(max(z2) == z[1000]) "NA" else round(max(z2), digits = 2), ")"),cex=0.8)
      
      text(z[.85*1000*acc], 0.86, paste("1/32 SI (", 
                                        if(min(z3) == z[1]) "NA" else round(min(z3), digits = 2), ",",
                                        if(max(z3) == z[1000]) "NA" else round(max(z3), digits = 2), ")"),cex=0.8)
      
      text(z[.85*1000*acc], 0.81, paste("L(", rrhat, ")/L(1)=", L1),cex=0.8)}
  }
}




# Examples:
# Results from the 2009 M&Ms experiment
bin.lik(86, 405) # milk chocolate
bin.lik(179, 1178) # minis
bin.lik(57, 417) # dark
rr.lik(86, 405, 179, 1178) # milk vs minis
rr.lik(86, 405, 57, 417) # milk vs dark

# To restrict the x-axis in the plot use the lolim and hilim options.
rr.lik(84,357,223,1127,lolim=0.5,hilim=2)

# To plot two likelihoods on the same plot use like.only=T and par(new=T).
rr.lik(14, 200, 28, 200,lolim=0,hilim=4, like.only=T, plotcol='blue')
par(new=T)
rr.lik(56, 200, 28, 200,lolim=0,hilim=4, like.only=T, plotcol='red')

# likewise for bin.lik
bin.lik(84, 357,lolim=0.1,hilim=0.5, like.only=T, plotcol='blue')
par(new=T)
bin.lik(223, 1127,lolim=0.1,hilim=0.5, like.only=T, plotcol='red')

# you can also add generic lines as reference for the 1/8th and 1/32nd support intervals
lines( c(-100, 100), c(1/8, 1/8), lty="dotted", col="grey" )
lines( c(-100, 100), c(1/32, 1/32), lty="dotted", col="grey" )



