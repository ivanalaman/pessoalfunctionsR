plot.ic <- function(x, ...) UseMethod('plot.ic') 

plot.ic.lm <- function(model,
                       sig.level=0.95,
                       col.lines='gray',
                       col.fitted='red',
                       ic = TRUE,
                       args.legend = NULL,
		       asline=FALSE, 
                       ...){

  aux.form1 <- formula(model)
  aux.form2 <- strsplit(as.character(aux.form1), '~')
  aux.y <- aux.form2[[2]]
  aux.x <- aux.form2[[3]]
  
  dad <- model$model
  y <- dad[[aux.y]]
  x <- dad[[aux.x]]
  
  model <- lm(y ~ x)

  newd <- data.frame(x)
  estimados <- predict(model,
                       newd,
                       interval='confidence',
                       level=sig.level)
  if(!asline){
  plot(x,
       y,
       ...)
  }else{
  points(x,
	 y,
	 ...)
  }
  lines(spline(x,
               fitted(model),
               n=1e3),
        col=col.fitted,
  ...)
  if(ic){
    lines(spline(x,
                 estimados[,2],
                 n=1e3),
          lty=1,
          col=col.lines)
    lines(spline(x,
                 estimados[,3],
                 n=1e3),
          lty=1,
          col=col.lines)
  }

  if(is.null(args.legend)){

    args.2Kl <- list(x        = 'topleft',
                     legend   = c('Pontos observados',
                                  'Regressão estimada',
                                  paste('Intervalo de confiança de ',sig.level*100,'%',sep='')),
                     pch      = c(1,NA,NA),
                     lty      = c(NA,1,1),
                     col      = c(1, col.fitted, col.lines))

  } else {

    args.2Kl <- list(x        = 'topleft',
                     legend   = c('Pontos observados',
                                  'Regressão estimada',
                                  paste('Intervalo de confiança de ',sig.level*100,'%',sep='')),
                     pch      = c(1,NA,NA),
                     lty      = c(NA,1,1), 
                     col      = c(1, col.fitted, col.lines)) 

    args.2Kl[names(args.legend)] <- args.legend     

  }       

  do.call('legend',
          args.2Kl) 

}

plot.ic.lsmobj <- function(model,
                           x = x,
                           sig.level=0.95,
                           col.lines='gray',
                           col.fitted='red',
                           ic = TRUE,
			   asline=FALSE,
                           ...){

  y <- summary(model)$lsmean
  x <- x 

  linf <- summary(model)$`lower.CL`
  lsup <- summary(model)$`upper.CL`
  
  if(!asline){
  plot(x,
       y,
       ...)
  }else{
  points(x,
	 y,
	 ...)
  }
  lines(spline(x,
               y,
               n=1e3),
        col=col.fitted,
  ...)
  if(ic){
    lines(spline(x,
                 linf,
                 n=1e3),
          lty=1,
          col=col.lines)
    lines(spline(x,
                 lsup,
                 n=1e3),
          lty=1,
          col=col.lines)
  }

  if(is.null(args.legend)){

    args.2Kl <- list(x        = 'topleft',
                     legend   = c('Pontos observados',
                                  'Regressão estimada',
                                  'Intervalo de confiança de 95%'),
                     pch      = c(1,NA,NA),
                     lty      = c(NA,1,1),  
                     col = c(1, col.fitted, col.lines)) 

  } else {

    args.2Kl <- list(x        = 'topleft',
                     legend   = c('Pontos observados',
                                  'Regressão estimada',
                                  'Intervalo de confiança de 95%'),
                     pch      = c(1,NA,NA),
                     lty      = c(NA,1,1),     
                     col = c(1, col.fitted, col.lines))  

    args.2Kl[names(args.legend)] <- args.legend     

  }       

  do.call('legend',
          args.2Kl) 

}

plot.ic.nls <-  function(model,
                         sig.level=0.95,
                         col.lines='gray',
                         col.fitted='red',
                         ic = TRUE,
                         args.legend = NULL,
			 asline = FALSE, 
                         ...){ 
  aux.form1 <- formula(model)
  aux.form2 <- strsplit(as.character(aux.form1), '~')
  aux.y <- aux.form2[[2]]
  aux.x <- attr(model$dataClass,'names')

  dad <- eval(model$data)
  y <- dad[[aux.y]]
  x <- dad[[aux.x]]

  newd <- data.frame(x)
  names(newd) <- aux.x
  estimados <- predictNLS(object  = model,
                          newdata = newd,
                          level=sig.level)
  
  if(!asline){
  plot(x,
       y,
       ...)
  }else{
  points(x,
	 y,
	 ...)
  }
  lines(spline(x,
               estimados[,1],
               n=1e3),
        col=col.fitted,
  ...)
  if(ic){
    lines(spline(x,
                 estimados[,6],
                 n=1e3),
          lty=1,
          col=col.lines)
    lines(spline(x,
                 estimados[,7],
                 n=1e3),
          lty=1,
          col=col.lines)
  }

  if(is.null(args.legend)){

    args.2Kl <- list(x        = 'topleft',
                     legend   = c('Pontos observados',
                                  'Regressão estimada',
                                  paste('Intervalo de confiança de ',sig.level*100,'%',sep='')),
                     pch      = c(1,NA,NA),
                     lty      = c(NA,1,1),
                     col      = c(1, col.fitted, col.lines))

  } else {

    args.2Kl <- list(x        = 'topleft',
                     legend   = c('Pontos observados',
                                  'Regressão estimada',
                                  paste('Intervalo de confiança de ',sig.level*100,'%',sep='')),
                     pch      = c(1,NA,NA),
                     lty      = c(NA,1,1), 
                     col      = c(1, col.fitted, col.lines)) 

    args.2Kl[names(args.legend)] <- args.legend     

  }       

  do.call('legend',
          args.2Kl) 

} 
