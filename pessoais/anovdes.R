anovdes <- function(x,block,plots,subplot,data,rownamess){
  form <- as.formula(paste('x~block*plots + plots*subplot'))
  lmm <- lm(form,data=data)
  mod <- Anova(lmm,type='III',singular.ok=TRUE)
  tab <- matrix(nrow=6,ncol=5)
  tab[1,] <- c(mod$Df[2],
               mod$'Sum Sq'[2],
               mod$'Sum Sq'[2]/mod$Df[2],
               (mod$'Sum Sq'[2]/mod$Df[2])/(mod$'Sum Sq'[5]/mod$Df[5]),
               (1-pf((mod$'Sum Sq'[2]/mod$Df[2])/(mod$'Sum Sq'[5]/mod$Df[5]),mod$Df[2],mod$Df[5])))
  tab[2,] <- c(mod$Df[3],
               mod$'Sum Sq'[3],
               mod$'Sum Sq'[3]/mod$Df[3],
               (mod$'Sum Sq'[3]/mod$Df[3])/(mod$'Sum Sq'[5]/mod$Df[5]),
               (1-pf((mod$'Sum Sq'[3]/mod$Df[3])/(mod$'Sum Sq'[5]/mod$Df[5]) ,mod$Df[3],mod$Df[5])))
  tab[3,] <- c(mod$Df[5],
               mod$'Sum Sq'[5],
               mod$'Sum Sq'[5]/mod$Df[5],
               NA,
               NA)
  tab[4,] <- c(mod$Df[4],
               mod$'Sum Sq'[4],
               mod$'Sum Sq'[4]/mod$Df[4],
               mod$'F value'[4],
               (1-pf(mod$'F value'[4],mod$Df[4],mod$Df[7])))
  tab[5,] <- c(mod$Df[6],
               mod$'Sum Sq'[6],
               mod$'Sum Sq'[6]/mod$Df[6],
               mod$'F value'[6],
               (1-pf(mod$'F value'[6],mod$Df[6],mod$Df[7])))
  tab[6,] <- c(mod$Df[7],
               mod$'Sum Sq'[7],
               mod$'Sum Sq'[7]/mod$Df[7],
               NA,
               NA)

  colnames(tab) <- c('Gl','SQ','QM','Valor F','Pr(>F)')
  rownames(tab) <- rownamess
  #cat("Analise de variancia","\n")
  tab <- list(tab)
  #return(tab)
}                         
