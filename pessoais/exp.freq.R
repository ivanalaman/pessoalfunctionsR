exp.freq <- function(x){
 if(!is.table(x)) stop('O objeto deve ser uma tabela')
 t.linhas <- rowSums(x)
 t.colunas <- colSums(x)
 t.total <- sum(x)
 categories <- attr(x, 'dimnames')
 ncategories.linhas <- length(unlist(categories[1]))
 ncategories.colunas <- length(unlist(categories[2]))
 expfreq <- matrix(NA,
                   ncol = ncategories.colunas,
                   nrow = ncategories.linhas)

 for(i in 1:ncategories.linhas){
  for(j in 1:ncategories.colunas){
   expfreq[i,j] <- t.linhas[i] * t.colunas[j]/t.total
  }
 }
 colnames(expfreq) <- unlist(categories[2])
 rownames(expfreq) <- unlist(categories[1])
 return(expfreq)
}
