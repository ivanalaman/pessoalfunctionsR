segment_expl <- function(x,b0,k,b1,X0){
  (b0*exp(x/k))*(x<=X0)+
  (b0*exp(X0/k) -b1*X0 + b1*x)*(x>X0)
} 
