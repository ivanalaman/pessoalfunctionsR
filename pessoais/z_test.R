##### Teste de hipótese para média - sigma conhecido
z_test <- function(x, 
                   y = NULL,
                   success = NULL,
                   alternative = c('two.sided','less','greater'),
                   mu0 = 0,
                   pi0 = 0,
                   sigma = NULL, 
                   conf.level=0.95,
                   na.rm = TRUE){
 if(is.numeric(x)){
  if(is.null(y)){
    
    if(is.null(sigma)) stop('Você dever fornecer um valor para o sigma!!!')

    n <- length(x)
    xbarra <- mean(x) 
    erropad <- sigma/sqrt(n)
    z <- (xbarra - mu)/erropad
    
    switch(match.arg(alternative),
           two.sided = {
             zteorico <- qnorm((1-conf.level)/2)
             pvalue <- pnorm(abs(z),lower.tail=FALSE)*2
             ifelse(z<0 & z < zteorico | z>0 & z > zteorico,
                    res <- 'Rejeita-se H0',
                    res <- 'Não rejeita-se H0'
                    )
           },
           less = {
             zteorico <- qnorm((1-conf.level))
             pvalue <- pnorm(z)
             ifelse(z<zteorico,
                    res <- 'Rejeita-se H0',
                    res <- 'Não rejeita-se H0')
           },
           greater = {
             zteorico <- qnorm((1-conf.level),
                               lower.tail=FALSE)
             pvalue <- pnorm(z,lower.tail=FALSE)
             ifelse(z>zteorico,
                    res <- 'Rejeita-se H0',
                    res <- 'Não rejeita-se H0')
           }
             ) 
    rval <- list(zcalculado = z,
                 zteorico = zteorico,
                 pvalor = pvalue,
                 estimate = xbarra,
                 standard.error = erropad,
                 null.value=mu,
                 alternative = match.arg(alternative),
                 conclusao = res)
    return(rval)
  }
}else{#qualitative variable!
  if(is.null(success)) stop("You must be to give a success!")
  nsuccess = sum(x%in%success,na.rm=na.rm)
  n        = length(na.omit(x))
  pbarra = round(nsuccess/n,4)
  erropad = sqrt((pi0*(1-pi0))/n)
  z = (pbarra - pi0)/erropad

  switch(match.arg(alternative),
         two.sided = {
           zteorico <- qnorm((1-conf.level)/2)
           pvalue <- pnorm(abs(z),lower.tail=FALSE)*2
           ifelse(z > abs(zteorico),
                  res <- 'Rejeita-se H0',
                  res <- 'Não rejeita-se H0'
           )
         },
         less = {
           zteorico <- qnorm((1-conf.level))
           pvalue <- pnorm(z)
           ifelse(z < zteorico,
                  res <- 'Rejeita-se H0',
                  res <- 'Não rejeita-se H0')
         },
         greater = {
           zteorico <- qnorm((1-conf.level),
                             lower.tail=FALSE)
           pvalue <- pnorm(z,lower.tail=FALSE)
           ifelse(z > zteorico,
                  res <- 'Rejeita-se H0',
                  res <- 'Não rejeita-se H0')
         }
  )  
   rval <- list(zcalculado     = z,
                zteorico       = zteorico,
                pvalor         = pvalue, 
                estimate       = pbarra,
                standard.error = erropad,
                null.value     = pi0,
                alternative    = match.arg(alternative),
                conclusao      = res)
   return(rval) 
 }
}
