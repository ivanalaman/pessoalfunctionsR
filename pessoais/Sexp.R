Sexp <- function(predictor, a , k){
  expModel <- function(predictor, a, k){
    a * exp(predictor * k)
  }
  expModelInit <- function(mCall, LHS, data){
    xy <- sortedXyData(mCall[["predictor"]], LHS, data)
    lmFit <- lm(log(xy[,"y"]) ~ xy[,"x"])
    coefs <- coef(lmFit)
    a <- exp(coefs[1])
    k <- coefs[2]
    value <- c(a, k)
    names(value) <- mCall[c("a","k")]
  }
  sexp <- selfStart(expModel, expModelInit, c("a", "k"))
  return(sexp)
} 

