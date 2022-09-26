ep <- function(x,...){
	UseMethod('ep')
}

ep.default <- function(x,which=NULL,type=c('original','delta_ln','delta_log','delta_sqrt','delta_boxcox'),lambda=NULL,...){
 #x is a vector or an object from aov, lm or lmerMod class.
 #data is a set of data of experiment. Use if x is aov, lm or lmerMod class.
 #which is a string. Is the treatment you want to calculate the standard error.
 #lambda is a scalar. Use when the type is "delta_boxcox".

 #  types <- match.arg(type)
 #         switch(types,
 simple = sd(x, ...)/sqrt(length(x[!is.na(x)]))
 #                design = {
 #                        aux_factors <- unlist(strsplit(which,':'))
 #                        ifelse(length(aux_factors)==1,
 #                               factors <- aux_factors,
 #                               {
 #                               comtra <- gsub(':',',',which)
 #                               factors <- paste('list(',comtra,')',sep='')
 #                               })
 #                        varis <- with(data,tapply(x,eval(parse(text=factors)),var,na.rm=TRUE))
 #                        reps <- with(data,tapply(x,eval(parse(text=factors)),length))
 #                        repsmean <- mean(rowSums(as.matrix(reps),na.rm=TRUE))
 #                        varismean <- rowMeans(as.matrix(varis),na.rm=TRUE)
 #                        res <- sqrt(mean(varismean)/repsmean)
 #                        return(res)
 #                }
}

ep.lm <- function(x,which=NULL,type=c('original','delta_ln','delta_log','delta_sqrt','delta_boxcox'),lambda=NULL,...){
 types <- match.arg(type)
 
 switch(types,
  original = {
   factors <- attr(x$model,'names')[-1] 
   completedata <- eval(getCall(x)$data)
   index <- which(!names(completedata)%in%factors)
   datawithoutfactors <- completedata[,index]
   aux_repi <- aggregate(datawithoutfactors,by=list(completedata[[which]]),length)
   repi <- unique(aux_repi[,2])
   sqrt(sigma(x)^2/repi)
  },
  delta_boxcox = {
   labelybox <- attr(x$model,'names')[1]
   factors <- attr(x$model,'names')[-1] 
   completedata <- eval(getCall(x)$data)
   index <- which(!names(completedata)%in%factors)
   response <- completedata[,index]
   aux_repi <- aggregate(response,by=list(completedata[[which]]),length)
   repi <- unique(aux_repi[,2])
   standarderrorbox <- sqrt(sigma(x)^2/repi)
   ybox <- completedata[[labelybox]]
   yorig <- (lambda*ybox + 1)^(1/lambda)
   averageapprox <- mean(yorig,na.rm=TRUE)
   averageapprox/averageapprox^lambda * standarderrorbox
  }
 )
}

ep.lmerMod <- function(x,which=NULL,type=c('original','delta_ln','delta_log','delta_sqrt','delta_boxcox'),lambda=NULL,...){
 types <- match.arg(type)

 switch(types,
  original = {
   auxfactors <- attr(terms(x),'variables')
   factors <- as.character(auxfactors)[-c(1:2)]
   completedata <- getData(x)
   index <- which(!names(completedata)%in%factors)
   dd <- completedata[,index]
   aux_repi <- aggregate(dd,by=list(completedata[[which]]),length)
   repi <- unique(aux_repi[,2])
   sqrt(sigma(x)^2/repi)
  },
  delta_boxcox = {
   auxfactors <- attr(terms(x),'variables')
   factors <- as.character(auxfactors)[-c(1:2)]
   labelybox <- as.character(auxfactors)[2]
   completedata <- getData(x)
   index <- which(!names(completedata)%in%factors)
   dd <- completedata[,index]
   aux_repi <- aggregate(dd,by=list(completedata[[which]]),length)
   repi <- unique(aux_repi[,2])
   standarderrorbox <- sqrt(sigma(x)^2/repi)
   ybox <- completedata[[labelybox]]
   yorig <- (lambda*ybox + 1)^(1/lambda)
   averageapprox <- mean(yorig,na.rm=TRUE)
   averageapprox/averageapprox^lambda * standarderrorbox
  })
}

