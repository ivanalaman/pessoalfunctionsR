experiment_test <- function(formula, variables, data, by){

  vari    <- names(variables)

  levelss <- levels(data[[by]])

  res_aux <- list()
  res_aux2 <- list()
  qmes <- list()
  glerro <- list()

  for(i in 1:length(levelss)) {

    res_aux[[i]] <- apply(data[data[[by]]==levelss[i],][,vari],
                          2,
                          function(y) aov(eval(parse(text=formula)),
                                          data[data[[by]]==levelss[i],]))
    res_aux2[[i]] <- lapply(res_aux[[i]],
                            summary)
    qmes[[i]] <- lapply(res_aux2[[i]],
                        function(x) x[[1]][3,3])

    glerro[[i]] <- lapply(res_aux2[[i]],
                          function(x) x[[1]][3,1]) 
  }
  fcalc <- mapply(function(x,y) {ifelse(x>y,x/y,y/x)},
                  qmes[[1]],
                  qmes[[2]]
                  )

  k <- 2-1 # número de variâncias independentes
  n <- unique(unlist(glerro))[1] # número de graus de liberdade associado ao erro da variância
  pvalor <- sapply(fcalc,
                   function(x)pf(x,
                                 k,
                                 n,
                                 lower.tail=F)) 
  res <- list(qmes,
              pvalor)

  return(res)

}

