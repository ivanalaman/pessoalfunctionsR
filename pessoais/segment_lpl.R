segment_lpl <- function(x,b0,b1,b2,X0,X1){
   (b0+b1*x)*(x<=X0)+
   (b0+b1*X0)*(X0<x & x<X1)+
   (b0+b1*X0-b2*(X1-x))*(x>=X1)
}  
