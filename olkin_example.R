library(R2jags)
x <- c(16, 18, 22, 25, 27)
# x=c(16, 18, 22, 25 , 28)
n <- 5
model.file <- "/Users/taban/Desktop/Taban/absurb/posi.txt"
# file.show(model.file)

data <- list("x", "n")
inits <- function() {
  list(theta = 0.5, m = 30)
}

parameters <- c("m", "theta")
sim <- jags(data, inits, parameters,
  n.iter = 100000, model.file = model.file
)
print(sim)

  
  x=c(16, 18, 22, 25 , 28)
  
  model.file <-  "/Users/taban/Desktop/Taban/absurb/posi.txt"
  #file.show(model.file)
  
  data <- list ("x","n")
  inits <- function(){
    list(theta=.5,m=30)
  }
  
  
  parameters <- c( "m","theta")
  
  
  sim <- jags(data, inits, parameters,
              n.iter=100000, model.file=model.file)
  
  
  print(sim)
  mu.vect sd.vect   2.5%    25%    50%    75%  97.5%  Rhat n.eff
  m         42.652   9.693 30.000 36.000 40.000 47.000 66.000 1.001  3000
  theta      0.532   0.108  0.321  0.456  0.536  0.613  0.729 1.001  3000
  deviance  31.705   2.027 29.385 30.260 31.186 32.588 36.754 1.001  2100
  
  