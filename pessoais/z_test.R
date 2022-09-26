# =============================================================================
# Título : Teste de hipótese para médias, proporções, diferenças entre
#          médias e proporções - sigma conhecido
# Autor  : Laboratório de Estatística Computacional - LEC
# Data   : 26/08/2021 13:05:03
# Tutores: Ivan Bezerra Allaman
# =============================================================================

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                          Descrição dos argumentos                           #
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# x: um escalar numérico que se refere a média ou proporção ou um vetor com o 
#    rol de dados brutos que pode ser numérico (se o interesse for a média) 
#    ou strings (se o interesse for a proporção).
# y: um escalar numérico que se refere a média ou proporção ou um vetor com o 
#    rol de dados brutos que pode ser numérico (se o interesse for a média) 
#    ou strings (se o interesse for a proporção). USE ESTE ARGUMENTO SE O 
#    INTERESSE ESTIVER NA DIFERENÇA ENTRE DUAS POPULAÇÕES.
# success: qual categoria é considerado o sucesso dado os vetores x e/ou y 
#    se o parâmetro de interesse for a proporção? 
# alternative: um escalar string que reflete a hipótese a ser testada. O valor
#    default é two.sided (bilateral)
# mu0: um escalar numérico referente ao valor hipotético a ser testado caso o 
#    parâmetro de interesse seja a média. O valor default é zero.
# pi0: um escalar numérico referente ao valor hipotético a ser testado caso o 
#    parâmetro de interesse seja a proporção. O valor default é NULL. 
# sigma: um escalar numérico que se refere ao desvio padrão da população ou um 
#    vetor de tamanho dois caso sejam duas populações e y diferente de NULL. É
#    utilizado tanto para testes de hipóteses apenas para média.
# n: um escalar numérico ou um vetor de tamanho dois (no caso de comparar duas
#    populações) que se refere ao tamanho da(s) amostra(s) caso não seja 
#    fornecido o rol de dados brutos nos argumentos x e/ou y.
# conf.level: um escalar numérico que se refere ao nível de significância 
#    adotado na pesquisa.
# na.rm: um valor booleano caso tenha NAs no rol de dados. O valor default é 
#    NULL
z_test <- function(x,
                   ...) UseMethod('z_test')

z_test.default <- function(x, 
                           y = NULL,
                           success = NULL,
                           alternative = c('two.sided','less','greater'),
                           mu0 = 0,
                           pi0 = NULL,
                           sigma = NULL,
                           n = NULL, 
                           conf.level = 0.95,
                           na.rm = TRUE,
                           ...){

  alt <- match.arg(alternative)

  obj <- list(x = x,
              y = y,
              success = success,
              alternative = alt,
              mu0 = mu0,
              pi0 = pi0,
              sigma = sigma,
              n = n,
              conf.level = conf.level,
              na.rm = na.rm)

  if(!is.null(pi0)){#o parâmetro de interesse é pi, seja uma ou duas populações
    ifelse(is.null(y),
           {
             class(obj) <- 'proportions'
             res <- z_test(obj)
             return(res)
           },
           {
             class(obj) <- 'differences_proportions'
             res <- z_test(obj)
             return(res)
           }
    )
  }
  #caso contrário o parâmetro de interesse é mu, seja uma ou duas populações

  if(!is.null(y)){
    class(obj) <- 'differences_means'
    res <- z_test(obj)
    return(res)
  }
  #caso contrário será executado a função default que é teste de hipótese para uma média com sigma conhecido

  #############Código da função default: teste de hipótese para uma média com sigma conhecido##################

  if((length(x) == 1) & is.null(n)){#aqui o usuário fornece diretamente a média
    stop("Forneça o tamanho da amostra ou um vetor com o dados brutos!")
  }

  if(is.null(sigma)){
    stop("Forneça o valor de sigma pois o teste parte do princípio que o desvio padrão é conhecido!")
  }

  ifelse(length(x) == 1,
         {
           n <- n
           xbarra <- x
         },
         {
           n <- length(x)
           xbarra <- mean(x) 
         })

  erropad <- sigma/sqrt(n)
  z <- (xbarra - mu0)/erropad

  reshypo <- hypothesis(alternative = alt,
                        conf.level = conf.level,
                        z = z)

  res <- list(zcalculado     = z,
              zteorico       = reshypo$zteorico,
              pvalor         = reshypo$pvalor,
              estimate       = xbarra,
              standard.error = erropad,
              null.value     = mu0,
              alternative    = match.arg(alternative),
              conclusao      = reshypo$conclusao)
  return(res) 
} 

z_test.differences_means <- function(obj,
                                     ...){

  x <- obj$x
  y <- obj$y
  alternative <- obj$alternative
  mu0 <- obj$mu0
  sigma <- obj$sigma
  n <- obj$n
  conf.level <- obj$conf.level
  na.rm <- obj$na.rm

  if(length(sigma)!= 2){
    stop("Forneça o tamanho para a primeira e segunda amostra respectivamente como um vetor!")
  }

  ifelse((length(x) == 1 & length(y) == 1),
         {
           xbarra <- x
           ybarra <- y

           if(length(n) != 2){
             stop("Forneça o tamanho para a primeira e segunda amostra respectivamente como um vetor!")
           }
           nx <- n[1]
           ny <- n[2] 
         },
         {
           xbarra <- mean(x)
           ybarra <- mean(y)
           nx <- length(x)
           ny <- length(y)
         })

  difmedias <- xbarra - ybarra
  erropad <- sqrt(sigma[1]^2/nx + sigma[2]^2/ny)
  z <- (difmedias - mu0)/erropad

  reshypo <- hypothesis(alternative = alternative,
                        conf.level = conf.level,
                        z = z)

  res <- list(zcalculado     = z,
              zteorico       = reshypo$zteorico,
              pvalor         = reshypo$pvalor,
              estimate       = difmedias,
              standard.error = erropad,
              null.value     = mu0,
              alternative    = alternative,
              conclusao      = reshypo$conclusao)
  return(res)   
}  

z_test.proportions <- function(obj,
                               ...){
  x <- obj$x
  y <- obj$y
  success <- obj$success
  alternative <- obj$alternative
  pi0 <- obj$pi0
  n <- obj$n
  conf.level <- obj$conf.level
  na.rm <- obj$na.rm 

  ifelse(is.null(success),
         {
           pbarra = x
         },
         {
           nsuccess <- sum(x%in%success, na.rm=na.rm)
           n        <- length(na.omit(x))
           pbarra   <- round(nsuccess/n,4)
         })

  erropad <- sqrt((pi0*(1-pi0))/n)
  z <- (pbarra - pi0)/erropad

  reshypo <- hypothesis(alternative = alternative,
                        conf.level = conf.level,
                        z = z)

  res <- list(zcalculado     = z,
              zteorico       = reshypo$zteorico,
              pvalor         = reshypo$pvalor,
              estimate       = pbarra,
              standard.error = erropad,
              null.value     = pi0,
              alternative    = alternative,
              conclusao      = reshypo$conclusao)
  return(res)  
} 

z_test.differences_proportions <- function(obj,
                                           ...){
  x <- obj$x
  y <- obj$y
  success <- obj$success
  alternative <- obj$alternative
  pi0 <- obj$pi0
  n <- obj$n
  conf.level <- obj$conf.level
  na.rm <- obj$na.rm 

  ifelse(is.null(success),
         {
           pbarrax <- x
           pbarray <- y

           if(length(n) != 2){
             stop("Forneça o tamanho para a primeira e segunda amostra respectivamente como um vetor!")
           }
           nx <- n[1]
           ny <- n[2]
         },
         {
           nsuccessx <- sum(x%in%success, na.rm=na.rm)
           nsuccessy <- sum(y%in%success, na.rm=na.rm)
           nx <- length(na.omit(x))
           ny <- length(na.omit(y))
           pbarrax <- round(nsuccessx/nx,4)
           pbarray <- round(nsuccessy/ny,4)
         })

  pbarra <- round(pbarrax - pbarray,
                  4)

  ifelse(pi0 == 0,
         {
           pgrouped <- (nx*pbarrax + ny*pbarray)/(nx + ny)
           erropad <- sqrt((pgrouped*(1-pgrouped))*(1/nx + 1/ny))
         },
         {
           erropad <- sqrt(pbarrax*(1-pbarrax)/nx + pbarray*(1-pbarray)/ny)
         })

  z <- (pbarra - pi0)/erropad

  reshypo <- hypothesis(alternative = alternative,
                        conf.level = conf.level,
                        z = z)

  res <- list(zcalculado     = z,
              zteorico       = reshypo$zteorico,
              pvalor         = reshypo$pvalor,
              estimate       = pbarra,
              standard.error = erropad,
              null.value     = pi0,
              alternative    = alternative,
              conclusao      = reshypo$conclusao)
  return(res)   
}

hypothesis <- function(alternative,
                       conf.level,
                       z){
  switch(alternative,
         two.sided = {
           zteorico <- qnorm((1-conf.level)/2)
           pvalue <- pnorm(abs(z),
                           lower.tail=FALSE)*2
           ifelse(z < 0 & z < zteorico | z > 0 & z > zteorico,
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
           pvalue <- pnorm(z,
                           lower.tail=FALSE)
           ifelse(z > zteorico,
                  res <- 'Rejeita-se H0',
                  res <- 'Não rejeita-se H0')
         }
  )
  resp <- list(zteorico = zteorico,
               pvalor = pvalue,
               conclusao = res)
}

