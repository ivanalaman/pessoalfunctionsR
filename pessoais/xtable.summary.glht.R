xtable.summary.glht <- function(x,
                                caption = NULL, 
                                label = NULL, 
                                align = NULL, 
                                digits = NULL, 
                                display = NULL, 
                                ...){
  ob <- x[[9]]
  ob2 <- do.call('cbind',ob)
  ob3 <- cbind(comparisons=rownames(ob2),ob2[,3:6])
  rownames(ob3) <- NULL
  return(xtable(ob3,caption=caption,label=label,align=align,digits=digits,display=display,...))
}
 
