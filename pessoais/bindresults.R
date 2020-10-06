bindresults <- function(x,columns){
 newcolumns <- matrix(columns,ncol=2)
 res <- list()
 for(i in 1:nrow(newcolumns)){
  res[[i]] <- apply(x[,newcolumns[i,]],1,function(x)paste(x,collapse=''))
 }
 ress <- do.call('cbind',res)
 colnames(ress) <- newcolumns[,1]
 return(ress)
}

