
like.2s=function(p1=0.60,p0=0.40,n.i=17,n=41,ka.i=(1/4),kb.i=4,ka=(1/4),kb=4,simon=TRUE,r1=7,r=21,output=FALSE){
######################################
## Author: Jeffrey D. Blume
## Date:   June 2012
## Version 2.0 (revise Nov 1, 2012)
##
## Name: like.2s
## Description: Give operating characteristics of 
## the Likelihood 2-stage design (Extension of Simon's Optimal)
#######################################

#######################################	
## Function Inputs:
## p1       is response rate under H1
## p0       is response rate under H0
## n.i      is sample size for Stage 1
## n	    is Total sample size (Stage 1+ Stage 2)
## simon	is indicator to match simon's optimal design 
##			   overwrites LR limits (ka.i,kb.i,ka,kb) to match Simon's
## r1	    is number of successes in simon's Stage 1
## r	    is number of successes in simon's Final stage
##
## LR and FDR computations assume flat prior
#######################################	
#p1=0.60;p0=0.40;n.i=16;n=41;ka.i=(1/4);kb.i=4;ka=(1/4);kb=4;simon=TRUE;r1=7;r=21;output=TRUE

## returns odds ratio
or=(p1*(1-p0))/((p0)*(1-p1))

if (simon==TRUE) {
ka.i=1/3.375 #(or^(r1))*((1-p1)/(1-p0))^(n.i)
kb.i=Inf
ka=1         #(or^(r))*((1-p1)/(1-p0))^(n)
kb=Inf
}

#######################################	
## LR bounds translated to successes
#######################################	

top.i=round((log(kb.i)-n.i*log((1-p1)/(1-p0)))/log(or),10)
bot.i=round((log(ka.i)-n.i*log((1-p1)/(1-p0)))/log(or),10)
  ## if our r1 (bot.i) isn't an integer, change accordingly. The LR will then change, so change that too. 
if(floor(bot.i) < bot.i & bot.i < ceiling(bot.i)){
  bot.i <- floor(bot.i)
  ka.i  <- (or^(bot.i))*((1-p1)/(1-p0))^(n.i)
}

top=round((log(kb)-n*log((1-p1)/(1-p0)))/log(or),10)
bot=round((log(ka)-n*log((1-p1)/(1-p0)))/log(or),10)
#print(c(top.i,bot.i,top,bot))
#######################################	
## Interim Stage 
#######################################

pstr.i0=pbinom(floor(bot.i),size=n.i,prob=p0)
pmis.i0=1-pbinom(floor(top.i),size=n.i,prob=p0)
pwek.i0=(pbinom(floor(top.i),size=n.i,prob=p0)-pbinom(floor(bot.i),size=n.i,prob=p0))

pstr.i1=1-pbinom(floor(top.i),size=n.i,prob=p1)
pmis.i1=pbinom(floor(bot.i),size=n.i,prob=p1)
pwek.i1=(pbinom(floor(top.i),size=n.i,prob=p1)-pbinom(floor(bot.i),size=n.i,prob=p1))

## Expected sample size
ess.0=n.i+(pwek.i0)*(n-n.i)
ess.1=n.i+(pwek.i1)*(n-n.i)

#######################################	
## Final Stage 
#######################################	

## Identify data (successes) that allow continuation (=weak evi at interim stage)
x=floor((bot.i+1)):min(n.i,top.i)  

pwek.0=sum(dbinom(x,size=n.i,prob=p0)*(pbinom((top-x),size=(n-n.i),prob=p0)-pbinom((bot-x),size=(n-n.i),prob=p0)))
pstr.0=pstr.i0+sum(dbinom(x,size=n.i,prob=p0)*(pbinom((bot-x),size=(n-n.i),prob=p0)))
pmis.0=pmis.i0+sum(dbinom(x,size=n.i,prob=p0)*(1-pbinom((top-x),size=(n-n.i),prob=p0)))

pwek.1=sum(dbinom(x,size=n.i,prob=p1)*(pbinom((top-x),size=(n-n.i),prob=p1)-pbinom((bot-x),size=(n-n.i),prob=p1)))
pstr.1=pstr.i1+sum(dbinom(x,size=n.i,prob=p1)*(1-pbinom((top-x),size=(n-n.i),prob=p1)))
pmis.1=pmis.i1+sum(dbinom(x,size=n.i,prob=p1)*(pbinom((bot-x),size=(n-n.i),prob=p1)))

## Notice pwek.i0=sum(dbinom(x,size=n.i,prob=p0)) for ease of computation

##################
## Combine results for return (if needed)
##################

inter=rbind(c(1,0,pstr.i0,pmis.i0,pwek.i0,pstr.i0+pmis.i0+pwek.i0,n.i),c(1,1,pstr.i1,pmis.i1,pwek.i1,pstr.i1+pmis.i1+pwek.i1,n.i))
final=rbind(c(2,0,pstr.0,pmis.0,pwek.0,pstr.0+pmis.0+pwek.0,ess.0),c(2,1,pstr.1,pmis.1,pwek.1,pstr.1+pmis.1+pwek.1,ess.1))
results=rbind(inter,final)
dimnames(results)[[2]]=c("Stage","Hyp","Str","Mis","Weak","All","ESS")

##################
## LR and FDR computations
##################

lri.1=results[2,'Str']/results[1,'Mis']
fdri.1=(1+lri.1)^-1
lri.0=results[1,'Str']/results[2,'Mis']
fndi.0=(1+lri.0)^-1

lrf.1=results[4,'Str']/results[3,'Mis']
fdrf.1=(1+lrf.1)^-1
lrf.0=results[3,'Str']/results[4,'Mis']
fndf.0=(1+lrf.0)^-1

lr=c(lri.0,lri.1,lrf.0,lrf.1)
fdr=c(fndi.0,fdri.1,fndf.0,fdrf.1)

results=cbind(results,unname(lr),unname(fdr))
dimnames(results)[[2]]=c("Stage","Hyp","Str","Mis","Weak","All","ESS","LR","FDR")

##################
## Output
##################

if (output==FALSE) {
cat("#########################################################################\n")
if (simon==FALSE ) {cat("##  Likelihood Two-Stage Design \n")}
else (cat("##  Simon's Optimal Two-Stage Design (Likelihood display) \n"))
cat("## ---------------------------------------------------------------\n")
cat("##  r.i (Simon) : ",bot.i,"\n")
cat("##  r   (Simon) : ",ceiling(bot),"\n")
cat("##  Hypotheses  : H0: p = ",round(p0,2)," ;  H1: p = ",round(p1,2),"  (OR =",round(or,2),")\n",sep="")
cat("##  Interim     : SS = ",sprintf("%3.0f",round(n.i,1)),"    ;  continue if 1/", round(1/ka.i,2)," < LR < ",round(kb.i,2),"\n",sep="")
cat("##  Final       : SS = ",sprintf("%3.0f",round(n,1)),  "    ;  weak ev  if 1/", round(1/ka,2)," < LR < ",round(kb,2),"\n",sep="")
cat("##  Expected SS : H0:",sprintf("%5.2f",round(ess.0,2)),"  ;  H1:",sprintf("%5.2f",round(ess.1,2)),"\n")
#cat("## ---------------------------------------------------------------\n")
cat("## -------------------------------\n")
cat("##  Interim          H0       H1 \n")
cat("## -------------------------------\n")
cat("##  Continue     ",sprintf("%4.4f",round(pwek.i0,4)), ";",sprintf("%4.4f",round(pwek.i1,4)),"\n")
cat("##  Strong Ev    ",sprintf("%4.4f",round(pstr.i0,4)), ";",sprintf("%4.4f",round(pstr.i1,4)),"\n")
cat("##  Mislead Ev   ",sprintf("%4.4f",round(pmis.i0,4)), ";",sprintf("%4.4f",round(pmis.i1,4)),"\n")
cat("##  LR (Str Ev)   ",sprintf("%4.2f",round(lri.0,4)), "; ",sprintf("%4.2f",round(lri.1,4)),"\n")
cat("##  FDR          ",sprintf("%4.4f",round(fndi.0,4)), ";",sprintf("%4.4f",round(fdri.1,4)),"\n")
cat("## -------------------------------\n")
#cat("## -------------------------------\n")
cat("##  Final            H0       H1 \n")
cat("## -------------------------------\n")
cat("##  Weak Ev      ",sprintf("%4.4f",round(pwek.0,4)), ";",sprintf("%4.4f",round(pwek.1,4)),"\n")
cat("##  Strong Ev    ",sprintf("%4.4f",round(pstr.0,4)), ";",sprintf("%4.4f",round(pstr.1,4)),"\n")
cat("##  Mislead Ev   ",sprintf("%4.4f",round(pmis.0,4)), ";",sprintf("%4.4f",round(pmis.1,4)),"\n")
cat("##  LR (Str Ev)   ",sprintf("%4.2f",round(lrf.0,4)), "; ",sprintf("%4.2f",round(lrf.1,4)),"\n")
cat("##  FDR          ",sprintf("%4.4f",round(fndf.0,4)), ";",sprintf("%4.4f",round(fdrf.1,4)),"\n")
#cat("## -------------------------------\n")
cat("#########################################################################\n")
}
results <- as.data.frame(results)
if (output==TRUE) {results}
}
#like.2s()
#(get=like.2s(output=TRUE))


####
###
##
#