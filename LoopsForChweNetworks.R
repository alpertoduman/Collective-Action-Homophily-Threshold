



library(igraph)

fixedPoint <- function(func, init){
  # Efe Postalci (efepostalci@gmail.com)
  # 5 Apr 2014
  # Returns fixed point of function funct, starting iteration at init
  while(T){
    res <- func(init);
    if (identical(res,init)) return(res);
    init <- res;
  }	}

revoltFunc <- function(set, neighbors, thresholds){
  # aggSet = set;
  # print(aggSet);
  # res <-  set[sapply(set,(function(x) checkCond(x, aggSet, neighbors,thresholds)))];
  # print(aggSet);
  # print(res);
  # res
  setSize = length(set);
  res <- numeric();
  for (i in set){
    cond = FALSE;
    intSet <- intersect(neighbors[[i]], set);
    tVal = thresholds[i];
    if(length(intSet) == setSize){cond = setSize >= tVal;}
    else{cond = length(revoltSet(intSet,neighbors,thresholds))>= tVal;}
    if(cond) res <- c(res,i)
  }
  return(res);
}

checkCond <- function(agent, set, neighbors, thresholds){
  intSet = intersect(neighbors[[agent]],set);
  tVal = thresholds[agent];
  setSize = length(set);
  if(length(intSet) == setSize) setSize >= tVal
  else length(revoltSet(intSet, neighbors, thresholds)) >= tVal
}
revoltSet <- function(set, neighbors, thresholds) 
  fixedPoint((function(x) revoltFunc(x, neighbors, thresholds)),set)


### 
g <- list()
neigh <- list()
# vt <- c(3,7)
# thres <- rep(vt,10)
L <- c(1:10)
H <- c(11:20)
#Lt <- rep(L,50)
#Ht <- rep(H,50)

thres <- list()
for (k in 1:10) {
  thres[[k]] <- c(rep(L[k],70), rep(H[k],30))
  thres
}

REV <- list()
p <- seq(0.1, 1, by=0.1)
RED <- matrix(NA, 10,10)

for (j in 1:10){
for(i in 1:10)
{
  g[[i]] <- sample_islands(10,10,p[j],1)
  neigh[[i]] <- neighborhood(g[[i]], 1, 1:vcount(g[[i]]))
  REV[[i]] <-revoltFunc(c(1:100), neigh[[i]], thres[[i]])
  RED[i,j] <-mean(length(REV[[i]])/vcount(g[[i]]))
  RED[i,j]
}
RED
}



RevoltRate48 <- RED

RevoltRate39 <- RED

plot(p, RevoltRate48, ylab="Revolt Rate")
lines(p, RevoltRate39, col="red")





g <- list()
neigh <- list()
 vt <- c(3,9)
 thres <- rep(vt,50)
# L <- 3
# H <- 9
# Lt <- rep(L,50)
# Ht <- rep(H,50)

# thres <- c(Lt, Ht)

REV <- list()
p <- seq(0.1, 1, by=0.01)
RED <- c()
for (j in 1:91){
  for(i in 1:10)
  {
    g[[i]] <- sample_islands(10,10,p[j],1)
    neigh[[i]] <- neighborhood(g[[i]], 1, 1:vcount(g[[i]]))
    REV[[i]] <-revoltFunc(c(1:100), neigh[[i]], thres)
    RED[j] <-mean(length(REV[[i]])/vcount(g[[i]]))
    RED[j]
  }
  RED
}

RevoltRate48mix <- RED

RevoltRate39mix <- RED

plot(p, RevoltRate48mix, ylab="Revolt Rate")
lines(p, RevoltRate39mix, col="red")













###

g <- list()
neigh <- list()
thres <- c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14,15,15)
REV <- list()
for(i in 1:100)
{
  g[[i]] <- watts.strogatz.game(1,30,1.5,0)
  neigh[[i]] <- neighborhood(g[[i]], 1, 1:vcount(g[[i]]))
  REV[[i]] <-revoltFunc(c(1:150), neigh[[i]], thres)
  RED <-mean(length(REV[[i]])/vcount(g[[i]]))
  RED
}

####

g <- list()
neigh <- list()
thres <- rep(8,30)
REV <- list()
for(i in 1:10)
{
  g[[i]] <- watts.strogatz.game(1,30,2, 0.4)
  neigh[[i]] <- neighborhood(g[[i]], 2, 1:vcount(g[[i]]))
  REV[[i]] <-revoltFunc(c(1:30), neigh[[i]], thres)
  RED <-mean(length(REV[[i]])/vcount(g[[i]]))
  RED
}









g <- list()
neigh <- list()
thres <- rep(4, 100)
REV <- list()
for(i in 1:100)
{
  g[[i]] <- erdos.renyi.game(100,0.1)
  neigh[[i]] <- neighborhood(g[[i]], 1, 1:vcount(g[[i]]))
  REV[[i]] <-revoltFunc(c(1:100), neigh[[i]], thres)
  RED <-mean(length(REV[[i]])/vcount(g[[i]]))
  RED
}

### In random networks (N=100, p=0.1), the ratio of rebels decline abruptly 
# from 98% when the thres=3 to
# only 4% when the thres=4

g <- list()
neigh <- list()
thres <- rep(6, 100)
REV <- list()
for(i in 1:100)
{
  g[[i]] <- watts.strogatz.game(dim=1,100,4,p=0.05)
  neigh[[i]] <- neighborhood(g[[i]], 1, 1:vcount(g[[i]]))
  REV[[i]] <-revoltFunc(c(1:100), neigh[[i]], thres)
  RED <-mean(length(REV[[i]])/vcount(g[[i]]))
  RED
}


# In small-world networks (100, 4, 0.05), the ratio is 67% if thres=5, and drops to zero
# when thres=6

## Once we increase the neighbourhood ball size to 2 in small world (20, 2,0.05), with
# a thres=6 the rebel ratio rises from 0 to  55-60 %.

g <- list()
neigh <- list()
thres <- rep(6, 20)
REV <- list()
for(i in 1:20)
{
  g[[i]] <- watts.strogatz.game(dim=1,20,2,p=0.05)
  neigh[[i]] <- neighborhood(g[[i]], 2, 1:vcount(g[[i]]))
  REV[[i]] <-revoltFunc(c(1:20), neigh[[i]], thres)
  RED <-mean(length(REV[[i]])/vcount(g[[i]]))
  RED
}


#### Coloring the dynamics
g <- list()
neigh <- list()
thres <- c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14,15,15)
REV <- list()
# set.seed(4589)

for(i in 1:1)
{
  g[[i]] <- erdos.renyi.game(30,0.1)
  neigh[[i]] <- neighborhood(g[[i]], 1, 1:vcount(g[[i]]))
  REV[[i]] <-revoltFunc(c(1:30), neigh[[i]], thres)
  RED <-mean(length(REV[[i]])/vcount(g[[i]]))
  RED
  col <- rep("yellow",30)
col[REV[[i]]] <- "red"
plot(g[[i]], vertex.label=NA, vertex.color=col)
}


