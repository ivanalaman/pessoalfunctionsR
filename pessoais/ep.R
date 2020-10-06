ep <- function(x,...){
	UseMethod('ep')
}

ep.default <- function(x,data=NULL,which=NULL,type=c('simple','design'),...){
	#If type=design, then to input data and which. If design be block or factorial i.e, then use which='trat:block'
	types <- match.arg(type)
	switch(types,
	       simple = sd(x, ...)/sqrt(length(x[!is.na(x)])),
	       design = {
		       aux_factors <- unlist(strsplit(which,':'))
		       ifelse(length(aux_factors)==1,
			      factors <- aux_factors,
			      {
			      comtra <- gsub(':',',',which)
			      factors <- paste('list(',comtra,')',sep='')
			      })
		       varis <- with(data,tapply(x,eval(parse(text=factors)),var,na.rm=TRUE))
		       reps <- with(data,tapply(x,eval(parse(text=factors)),length))
		       repsmean <- mean(rowSums(as.matrix(reps),na.rm=TRUE))
		       varismean <- rowMeans(as.matrix(varis),na.rm=TRUE)
		       res <- sqrt(mean(varismean)/repsmean)
		       return(res)
	       }
	)
}

ep.lm <- function(x,which=NULL,...){
	factors <- attr(x$model,'names')[-1] 
	aux_dd <- eval(getCall(x)$data)
	index <- which(!names(aux_dd)%in%factors)
	dd <- aux_dd[,index]
	aux_repi <- aggregate(dd,by=list(aux_dd[[which]]),length)
	repi <- unique(aux_repi[,2])

	sqrt(sigma(x)^2/repi)
}

