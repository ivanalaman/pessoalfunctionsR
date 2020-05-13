segment_lp <- function(x,b0,b1,X0){
   (b0+b1*x)*(x<=X0)+
   (b0+b1*X0)*(x>X0)
}  
