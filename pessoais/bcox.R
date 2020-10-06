bcox <- function(x,...){
	UseMethod('bcox')
}

bcox.default <- function(x,...){
	stop(paste("class", 
		   class(x), 
		   "objects are not valid for bcox" ))
}


bcox.aov <- function(x, data, lambda = seq(-2,2,1/10),plotit=TRUE,round=3,...){ #um objeto da classe lm, aov ou formula
	aux <- MASS::boxcox(object = x,data = data, lambda = lambda,plotit=plotit,...)
	aux2 <- do.call('cbind',aux)
	aux3 <- which(aux2 == max(aux2[,2]), arr.ind=TRUE)
	aux4 <- aux2[aux3[1,1],]
	lambda <- aux4[1]

	oldvar <- x$model$res 

	if(lambda == 0){
		newvar <- log(oldvar)
	}else{
		newvar <- round((oldvar^lambda - 1)/lambda,round)
	}
	res <- list(resp = newvar,
		    lambda = lambda)
	return(res)
}

bcox.formula <- function(x, data, lambda = seq(-2,2,1/10),plotit=TRUE,round=3,...){ #um objeto da classe lm, aov ou formula
	aux_vari <- gsub('()','',x[2])
        #auxformula <- gsub(aux_vari,eval(parse(text=aux_vari)),x)
	#auxf2 <- auxformula[-1]
	#newformula <- as.formula(paste(auxf2,collapse='~'))
	newformula <- x
	aux <- MASS::boxcox(object = newformula,data = data, lambda = lambda,plotit=plotit,...)
	aux2 <- do.call('cbind',aux)
	aux3 <- which(aux2 == max(aux2[,2]), arr.ind=TRUE)
	aux4 <- aux2[aux3[1,1],]
	lambda <- aux4[1]

	#oldvar <- data[eval(parse(text=aux_vari))] 
        oldvar<- data[aux_vari]
	
	if(lambda == 0){
		newvar <- log(oldvar)
	}else{
		newvar <- round((oldvar^lambda - 1)/lambda,round)
	}
	res <- list(resp = newvar,
		    lambda = lambda)
	return(res)
}

