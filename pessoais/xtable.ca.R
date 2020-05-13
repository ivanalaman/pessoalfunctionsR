xtable.ca <- function(x,...){
  aux <- summary(x)
  namerow <- aux$rows$name
  namecol <- aux$columns$name
  retencao <- round(aux$scree[1:2,3],2)
  tab <- estmod <- rbind(aux$rows[,c(6:7,9:10)]/10,
                         aux$columns[,c(6:7,9:10)]/10)
  rownames(tab) <- c(as.character(namerow),
                     as.character(namecol))
  names(tab) <- paste(names(tab),
                      c(retencao[1],
                        '',
                        retencao[2],
                        ''),
                      sep='')

  res <- xtable(tab,...)
  class(res) <- c('xtable.ca','xtable','data.frame')
  return(res)
}
