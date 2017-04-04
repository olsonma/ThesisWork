#########################################
## Lab 3: Quasi-likelihood
## 
## January 2014
## J. Blume
#########################################

#### generate count data from two groups with different means
nsample=25
y.0=rpois(nsample,4)
y.1=rpois(nsample,8)

## Put together in one vector; use indicator for group to track
y=c(y.0,y.1)
x=c(rep(0,nsample),rep(1,nsample))

## Summary Statistics (all, grp 0, grp 1)
c(mean(y),mean(y.0),mean(y.1))
c(var(y),var(y.0),var(y.1))

############ Regression models

#### Entire group; one model for y (i.e., ignore grouping)
## log(mu)=b0
pmod=glm(y~1,family=poisson(link=log))
summary(pmod)
exp(pmod$coeff) ## exp(b0)

## check model
round(exp(pmod$coeff),4)==round(mean(y),4)

## Fit Quasi-likelihood model (two ways to fit it)
## Same as glm(y~1,family=quasi(link=log,variance="mu"))
qmod=glm(y~1,family=quasipoisson(link=log))
summary(qmod)
exp(qmod$coeff) ## exp(b0)

## check model
round(exp(qmod$coeff),4)==round(mean(y),4)

## Assumed dispersion of the two models
summary(pmod)$dispersion
summary(qmod)$dispersion

## Which dispersion factor is correct?
var(y)/mean(y)

######################

#### Group Zero only; one model for y.0 (i.e., ignore data from 2nd group)
## log(mu)=b0
pmod.0=glm(y.0~1,family=poisson(link=log))
summary(pmod.0)
exp(pmod.0$coeff) ## exp(b0)

## check model
round(exp(pmod.0$coeff),4)==round(mean(y.0),4)

## Fit Quasi-likelihood model 
qmod.0=glm(y.0~1,family=quasipoisson(link=log))
summary(qmod.0)
exp(qmod.0$coeff)

## Assumed dispersion of the two models
summary(pmod.0)$dispersion
summary(qmod.0)$dispersion

## Which dispersion factor is correct?
var(y.0)/mean(y.0)

## much closer to one: Ohhhhh...Ahhhhh...

######################
#### plots to illustrate
beta=seq(0.75,2,0.001)

## compute log-likelihood from Poisson model
loglik=sum(y.0)*(beta)-length(y.0)*exp(beta)
loglik=loglik-max(loglik)
lik=exp(loglik)

## compute quasi-likelihood
qloglik=(sum(y.0)*(beta)-length(y.0)*exp(beta))/summary(qmod.0)$dispersion
qloglik=qloglik-max(qloglik)
qlik=exp(qloglik)

## Log-scale
plot(beta,lik,type="n",ylab=" ",xlab="Log-scale Mean (log lambda_0)")
lines(beta,lik,lty=1,col="black")
lines(beta,qlik,lty=1,col="red")
abline(v=qmod.0$coeff,lty=2,lwd=0.5,col="black")

## True scale
plot(exp(beta),lik,type="n",ylab=" ",xlab="Mean (lambda_0)")
lines(exp(beta),lik,lty=1,col="black")
lines(exp(beta),qlik,lty=1,col="red")
abline(v=exp(qmod.0$coeff),lty=2,lwd=0.5,col="black")

qmd=round(summary(qmod.0)$dispersion,4)
legend("topright",c("Poisson Likelihood","Quasi-Likelihood",
paste("QM Dispersion = ",qmd,sep=""))
,lty=c(1,1,0),col=c("Black","Red","NA"),bty='n')

## Invariance is wonderful, huh? Going b/w scales is easy....

## run plot several times to see what happens as var(y.0)/mean(y.0) varies
## Notice, in particular, what happens when 
## summary(qmod.0)$dispersion > 1 and summary(qmod.0)$dispersion < 1
## You should see the curves flip (change order)

############################################

## Full model or 'true' model (i.e., account for grouping)
pmod.f=glm(y~1+x,family=poisson(link=log))
summary(pmod.f)
exp(pmod.f$coeff)  # exp(b0), exp(b1)
c(mean(y.0),mean(y.1)/mean(y.0))

## check model
round(exp(pmod.f$coeff),4)==round(c(mean(y.0),mean(y.1)/mean(y.0)),4)

## predicted rate for 2nd group
exp(sum(pmod.f$coeff))
mean(y.1)

## Quasi-likelihood full model (i.e., account for grouping)
qmod.f=glm(y~1+x,family=quasipoisson(link=log))
summary(qmod.f)
exp(qmod.f$coeff) # exp(b0), exp(b1)
exp(sum(qmod.f$coeff))

## Same fit as above, but now the dispersion is accounted for properly
summary(pmod.f)$dispersion
summary(qmod.f)$dispersion

############################################

####
###
##
#