plotCVLarS <-
function(cv.lars.object,se=TRUE,xlab=xlab,ylab=ylab,...){
  mode=cv.lars.object$mode
  if(is.null(xlab)){
  xlab=switch(mode,
    fraction="Fraction of final L1 norm",
    step="Number of steps"
    )
  }
  if(is.null(ylab)) ylab = 'Cross-Validated MSE'
  index=cv.lars.object$index
  cv=cv.lars.object$cv
  cv.error=cv.lars.object$cv.error
      plot(index, cv, type = "b", ylim = range(cv, cv + cv.error, 
                                     cv - cv.error),xlab=xlab,ylab=ylab)
    if(se)
      error.bars(index, cv + cv.error, cv - cv.error, 
                 width = 1/length(index))
invisible()
}


