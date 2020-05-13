ep <- function(x, which=NULL,...){
  if(is.vector(x)){

    sd(x, ...)/sqrt(length(x[!is.na(x)]))
 
 } else {
    if(class(x)[1] == 'aov' | class(x)[1] == 'lm'){

      factors <- attr(x$model,'names')[-1] 
      aux_dd <- eval(getCall(x)$data)
      dd <- aux_dd[names(aux_dd)!=factors]
      aux_repi <- aggregate(dd,by=list(aux_dd[[which]]),length)
      repi <- unique(aux_repi[,2])

      sqrt((deviance(x)/df.residual(x))/repi)

    } else {
      stop('What do you want??')
    }
  }
}



