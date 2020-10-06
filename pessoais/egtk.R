egtk <- function(x){
	#x a object of TukeyC class
	if(inherits(x,'TukeyC')){
		resul <- x$out$Result
		rowname <- row.names(resul)
		condition <- suppressWarnings(is.na(unique(as.numeric(rowname))))
		ifelse(unique(condition),{
		      newname <- rowname
                      ordresul <- resul[order(newname),]
                      },
		      {
		      newname <- as.numeric(rowname)
		      ordresul <- resul[as.character(sort(newname)),]
		      })
		letter <- ordresul[,-1]
		if(ncol(resul)==2){
		 res <- data.frame(g=ordresul[,-1])
		 row.names(res) <- row.names(ordresul)
		}else{
		res <- tidyr::unite(letter,'g',names(letter),sep='')
		}
		return(res)
	} else {
	        resul <- x$groups
		ordresul <- resul[order(row.names(resul)),]
		res <- as.matrix(ordresul$groups)
		return(res)
	}
}

