bx <- function(x, data, lambda = seq(-2,2,1/10),plotit=TRUE,round=3,...){ #um objeto da classe lm, aov ou formula
  if(!require(MASS)){
   install.packages('MASS')
  }

 aux <- MASS::boxcox(object = x,data = data, lambda = lambda,plotit=plotit,...)
 aux2 <- do.call('cbind',aux)
 aux3 <- which(aux2 == max(aux2[,2]), arr.ind=TRUE)
 aux4 <- aux2[aux3[1,1],]
 lambda <- aux4[1]

 if(class(x) == 'formula'){
 
    oldvar <- data[[all.vars(x)[1]]] 
 
 } else {

   oldvar <- x$model$res 
 
 }
 
 if(lambda == 0){
 newvar <- log(oldvar)
 }else{
 newvar <- round((oldvar^lambda - 1)/lambda,round)
 }
 res <- list(resp = newvar,
             lambda = lambda)
 return(res)
}
