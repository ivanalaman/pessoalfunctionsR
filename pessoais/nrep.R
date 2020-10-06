nrep <- function(r_ini=2L,
 alpha=0.05,
 k,
 power,
 delta,
 sigma2,
 design=c('CRD','RCBD','LSD'),
 type=c('SIMPLE','FE','SPE')){
 #r_ini -> é o chute inicial para o número de repetições
 #alpha -> é o nível de significância do pesquisador
 #k     -> é o número de tratamentos que serão utilizado

 toe <- match.arg(type)
 des <- match.arg(design)
 option <- paste(des,
  toe,
  sep='_')
 switch(option,
  CRD_SIMPLE = {
   v1 <- k - 1
   pidelta <- 0
   r <- r_ini
   reps <- NULL
   powers <- NULL
   i <- 0
   while(pidelta <= power){
    i <- i + 1
    v2 <- k*(r - 1)
    delta2 <- (r*delta^2)/(2*sigma2)
    falpha <- qf(1-alpha,v1,v2)
    beta <- pf(falpha,v1,v2,delta2)
    pidelta <- 1 - beta
    reps[i] <- r
    powers[i] <- pidelta

    r <- r + 1 
   }
  },
  CRD_FE = {},
  CRD_SPE = {},
  RCBD_SIMPLE = {},
  RCBD_FE = {},
  RCBD_SPE = {},
  LSD_SIMPLE = {},
  LSD_FE = {},
  LSD_SPE = {})		

 res = cbind(r=reps,power=powers)
 colnames(res) = c('Repetition','Power')
 return(res)
}

