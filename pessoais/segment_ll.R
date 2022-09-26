segment_ll <- function(x,b0,b1,b2,X0){
  (b0+b1*x)*(x<=X0)+
  (b0+X0*(b1-b2)+b2*x)*(x>X0)
}  
