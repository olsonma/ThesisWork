## grab the likelihood 2 stage code: Function: like.2s
source("~/Documents/Vanderbilt/Masters_Thesis/ThesisWork/likelihood 2 stage (v3).R")
## arguments:
## p1=0.40,p0=0.20,n.i=17,n=37,ka.i=(1/4),kb.i=4,ka=(1/4),kb=4,simon=FALSE,r1=3,r=10,output=FALSE

n1Vec <- NULL
cvVec <- NULL
vecCount <- 1

for(n1 in 16:23){
  n1Vec[vecCount] <- n1
  design <- NULL
  counter <- 1
  for(cv in 1:15){
    results <- like.2s(p1 = 0.6, p0 = 0.4, n.i = n1, n = 41, ka.i = (1/4), kb.i = 4, ka=(1/4), kb = 4, simon = TRUE, r1 = cv, r = 21, output = TRUE)
    design[counter] <- results[results$Stage == 1 & results$Hyp == 0,]$Str
    counter <- counter+1
  }
  ## which critical value is closest to PET
  planned.PET=0.641
  cvVec[vecCount] <- which(abs(design-planned.PET)==min(abs(design-planned.PET)))
  vecCount <- vecCount + 1
}

n1Vec
cvVec

## could try expand.grid(ss, cv)
## run an apply function on that
# df <- expand.grid(ss, cv)
# 
# design <- apply(df, 1, function(x){ ## 1 means apply this function to all of the rows MARGIN  = 1, 2 means column
#   ## to all of the rows apply the following function 
#   ## each individual x will be a row
#   results <- like.2s(args, x[1], x[2])
#   with(results, results[Stage == 1 & Hyp == 0,]$Str)
# })

like.2s(p1 = 0.6, p0 = 0.4, n.i = n1, n = 41, ka.i = (1/4), kb.i = 4, ka=(1/4), kb = 4, simon = TRUE, r1 = cv, r = 21, output = TRUE)
