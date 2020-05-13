conf.mean <- function(x, sigma=NULL, n=NULL, conf.level=0.95,...){
  
  format.perc <- function(probs, digits){
   ## Not yet exported, maybe useful in other contexts:
   ## quantile.default() sometimes uses a version of it
   paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits),
	  "%")
}   
  
  if(length(x) == 1){# não temos uma amostra, e xbarra deve ser fornecido!
  
    xbarra = x

    if(is.null(sigma)) stop('Não há como estimar sigma!!')
    if(is.null(n)) stop('O tamanho da amostra deve ser fornecido!!')

    aux_a = (1-conf.level)/2
    a = c(aux_a, 1 - aux_a)
    pct = format.perc(a,3)
    fac = qnorm(a)
    erro_padrao = sigma/sqrt(n)  
    res <- list(est_pontual = xbarra,
                erro_pad = erro_padrao,
                quantil = fac,
                intervalo = xbarra + erro_padrao * fac) 
    res
  } else {
  
    xbarra = mean(x, ...)
    aux_a = (1-conf.level)/2
    a = c(aux_a, 1 - aux_a)
    pct = format.perc(a,3)
    n = length(na.omit(x))
    if(is.null(sigma)){
    
             sigma = sd(x, ...)
             fac = qt(a,n-1)

           } else {

           fac = qnorm(a)

           }

    erro_padrao = sigma/sqrt(n)  
    res <- list(est_pontual = xbarra,
                erro_pad = erro_padrao,
                quantil = fac,
                intervalo = xbarra + erro_padrao * fac) 
    res 
  }
}
