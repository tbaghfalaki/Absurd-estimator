library(R2jags)
NN=1000
M=CR=matrix(0,NN,2)
n=100

for(k in 1:NN){ 
x=rbinom(n,10,0.3)

model.file <-  "/Users/taban/Desktop/Taban/absurb/posi.txt"
#file.show(model.file)

data <- list ("x","n")
inits <- function(){
  list(theta=.5,m=12)
}


parameters <- c( "m","theta")


sim <- jags(data, inits, parameters,
            n.iter=10000, model.file=model.file)

M[k,]=sim$BUGSoutput[10]$summary[2:3,1]
L=sim$BUGSoutput[10]$summary[2:3,3]
U=sim$BUGSoutput[10]$summary[2:3,7]
Realpara=c(10,.3)
for(kk in 1:2){
  if(Realpara[kk]>L[kk] & Realpara[kk]< U[kk])(CR[k,kk]=1)
}
print(k)
}


apply(CR,2,mean)


AA=matrix(0,NN,length(Realpara))
for(k in 1:NN){ 
  for(kk in 1:2){
    AA[k,kk]=(M[k,kk]-Realpara[kk])^2
  }}


A=cbind(Realpara,apply(M,2,mean),apply(M,2,sd),(apply(M,2,mean)-Realpara)/Realpara,
        sqrt(apply(AA,2,mean)),apply(CR,2,mean))
colnames(A)=c("real","est","sd","relative bias","rmse","cr")
print(A)
