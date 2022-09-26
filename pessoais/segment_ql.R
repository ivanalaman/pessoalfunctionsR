segment_ql <- function(x,b0,b1,b2,b3){
 x0 = -0.5*b1/b2
 (b0 + b1*x + b2*I(x^2)) * (x <= X0) + 
 (b3*x + b0 + X0*(b1-b3) + b2*(X0)^2) * (x > X0)
}
