segment_pll <- function(x,b0,b1,b2,X0,X1){
  (b0)*(x<=X0)+
  (b0 - X0*b1 + b1*x)*(X0<x & x<=X1)+
  (b0 - X0*b1 + X1*(b1-b2) + b2*x)*(x>X1)
}  