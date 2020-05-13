R2 <- function(modelo){
 if(is.language(modelo$data)){
  da <- eval(modelo$data)
  resp.name <- all.vars(summary(modelo)$formula)[1]
  names(da)[which(names(da)==resp.name)] <- "y"
  sqe <- deviance(modelo)
  sqn <- deviance(lm(y ~ 1, da))
  (1-(sqe/sqn))*100
 } else {
  y <- eval(parse(text=all.vars(modelo$call)[1]))
  sqe <- deviance(modelo)
  sqn <- deviance(lm(y ~ 1))
  (1-(sqe/sqn))*100
}
}
