fdt_cross <- function(x,y,start,end,k,h){
 # x: the numeric variable
 # y: the categorical variable
 require(fdth)
 if(is.numeric(x)){
  auxx1 <- fdt(x,k=k,start=start,end=end,h=h)$breaks
  auxx2 <- cut(x,
               br=seq(auxx1[1],
                      auxx1[2],
                      auxx1[3]),
               right=auxx1[4],
               dig.lab=nchar(as.character(round(max(auxx1[2]),
                                                2))))     
 } else stop('x should be numeric!')

 res <- table(y,auxx2)
 return(res)
} 
