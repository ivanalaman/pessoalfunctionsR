propz_test <- function(x,y,success=NULL,alternative=c('two.sided','less','greater'),pi0=0,conf.level=0.95){

if(is.null(success)){
  p1 <- x[1]
  n1 <- x[2]
  p2 <- y[1]
  n2 <- y[2]
  } else { 
  p1 <- sum(x%in%success)/length(x)
  n1 <- length(x)
  p2 <- sum(y%in%success)/length(y)
  n2 <- length(y)
}

  p1_menos_p2 = p1 - p2

  if(pi0 == 0){

    p12 <- (n1*p1 + n2*p2)/(n1+n2)

    erropadiff <- sqrt(p12*(1-p12)*(1/n1 + 1/n2))

  } else {

    erropadiff <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)

  }

  Z <- (p1_menos_p2 - pi0)/erropadiff 

  switch(match.arg(alternative),

         two.sided = {

           pvalor <- 2*pnorm(-abs(Z))
           zteorico <- abs(qnorm((1-conf.level)/2))

         },

         less = {

           pvalor <- pnorm(Z)
           zteorico <- qnorm((1-conf.level))

         },

         greater = {

           pvalor <- pnorm(Z,
                           lower.tail = FALSE)
           zteorico <- qnorm((1-conf.level),
                             lower.tail = FALSE)

         })

  res <- list('Z statistic' = Z,
              'Z quantil' = zteorico,
              'P - valor'  = pvalor,
              'Estimate of diference' = p1_menos_p2,
              'Standard error' = erropadiff,
              'Pooled proportion' = p12)
  return(res)

}

