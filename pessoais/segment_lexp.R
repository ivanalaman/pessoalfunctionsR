segment_lexp <- function(x,b0,b1,k,X0){
  (b0 + b1*x)*(x <= X0) + 
    (((b0 + b1*X0)/exp(k*X0))*exp(k*x))*(x>X0)
}  
