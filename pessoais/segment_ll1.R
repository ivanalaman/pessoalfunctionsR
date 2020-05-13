segment_ll1 <- function(x,b0,b1,b2,b3){
  (b0+b1*x)*(x<=((b3-b0)/(b1-b2)))+
  (b3+b2*x)*(x>((b3-b0)/(b1-b2)))
}  
