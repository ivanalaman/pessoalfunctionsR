segment_lll <- function(x,b0,b1,b2,b3,X0,X1){
  (b0 + b1*x)*(x<=X0)+
  (b0 + X0*(b1-b2) + b2*x)*(X0<x & x<=X1)+
  (b0 + X0*(b1-b2) + X1*(b2-b3) + b3*x)*(x>X1)
}  