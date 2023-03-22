eta=seq(0,1,.001)

L1=L2=rep(length(eta))
for(k in 1:length(eta)){ 
  theta=eta[k]
x=1:100000
#sum(((1/x)-theta)^2*theta*(1-theta)^x)/(theta*(1-theta))

I=function(x){
  y=rep(0,length(x))
  for(i in 1:length(x)){ 
  if(x[i]==1)(y[i]=1)}
  ;y
}
L1[k]=sum((I(x)-theta)^2*theta*(1-theta)^(x-1)) /(theta*(1-theta))

L2[k]=sum(((1/x)-theta)^2*theta*(1-theta)^(x-1))/(theta*(1-theta))
}

par(mfrow=c(1,1),bg="gray90", mar=c(5,5,5,4))
plot(eta,L1,type="l",ylim=c(0,1.8),xlab=expression(theta),
     ylab=expression(R(delta(x),theta)),main="Risk Functions",lwd=2,col=4)
lines(eta,L2,type="l",col=2,lty=3,lwd=2)
legend("topright",col=c(4,2),lty=c(1,3),c("Minimax","Bayes estimator"),lwd=2:2)
    
       