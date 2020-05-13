circle <- function(xorig, yorig, radius, add, ...){
 
  x <- seq(-radius, radius, length.out = 1000)
 
  # Euclidian distance to the origin
  y <- sapply(x, function(z) sqrt(radius^2 - z^2))
 
  if(add == TRUE){
 
    lines(xorig + c(x, rev(x)), c(yorig + y, yorig + rev(-y)),
          type = "l", ...)
 
   } else {
 
   plot(xorig + c(x, rev(x)), c(yorig + y, yorig + rev(-y)),
        type = "l", ...)
 
   }
}
