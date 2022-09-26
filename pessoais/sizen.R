sizen <- function(parameter=c('mean','proportion'),
                  sigma2 = NULL,
                  pii = NULL,
                  me = NULL,
                  N = NULL,
                  conf.level = 0.95){
  # parameter: escolha o parâmetro alvo. Por padrão, será escolhido a média.
  # sigma2: é a variância. Deve ser fornecido caso o parâmetro seja a média.
  # pii: é a proporção. Deve ser fornecido caso o parâmetro seja a proporção.
  # me: é a margem de erro.
  # N: é o tamanho da população. Caso não seja fornecido, será considerado para os cálculos uma população infinita.
  # conf.level: é o nível de confiança desejável.
  a <- (1-conf.level)/2

  switch(match.arg(parameter),
         mean = {
           if(is.null(sigma2)|is.null(me))stop("It's necessary a value!")
           if(is.null(N)){
           n <- (qnorm(a)^2*sigma2)/(me^2)
           n <- round(n)
           }else{
            n <- (qnorm(a)^2*sigma2*N)/(me^2*(N-1) + qnorm(a)^2*sigma2) 
           n <- round(n)
           }
         },
         proportion = {
           if(is.null(pii)|is.null(me))stop("It's necessary a value!")
           if(is.null(N)){
           n <- (qnorm(a)^2*pii*(1-pii))/(me^2)
           n <- round(n)
           }else{
             n <- (qnorm(a)^2*pii*(1-pii)*N)/(me^2*(N-1) + qnorm(a)^2*pii*(1-pii))
             n <- round(n) 
           }
         })
  return(n)
}
