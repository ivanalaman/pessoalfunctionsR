segment_qp <- function(x,b0,b1,b2){
  (b0 + b1 * x + b2 * I(x^2)) * (x <= -0.5 * b1/b2) + 
    (b0 + I(-b1^2/(4 * b2))) * (x > -0.5 * b1/b2)
}  
