vonB <- function(x, Asym, B, k){
    Asym * (1 - B * exp(-k * x))^3
}
