gomp  <- function(x, Asym, B, k){
   Asym * exp(-B * exp(-k * x))
}
