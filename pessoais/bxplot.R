bxplot <- function(x,probs = seq(0,1,0.25),coef = 1.5, type = 2,...){
# Este boxplot é mais flexível do que o default da graphics, pois permite ao usuário utilizar o tipo de quantil que quiser com base na função quantile.
  res <- list()
  xord <- sort(x)
  qts <- quantile(x,type=type,probs=probs)
  IQR <- qts[4] - qts[2]
  minimo <- min(x)
  maximo <- max(x)
  minnonout <- qts[2] - coef*IQR
  maxnonout <- qts[4] + coef*IQR
  lwhisker <- max(minimo, sort(x)[sort(x) >= minnonout][1])
  uwhisker <- min(maximo, sort(x,decreasing=T)[sort(x,decreasing=T) <= maxnonout][1]) 
  n <- length(x)
  confmedian <- c(median(x)-(1.58*IQR/sqrt(n)),
                  median(x)+(1.58*IQR/sqrt(n)))
  
  outliers <- c(xord[xord < lwhisker], xord[xord > uwhisker])
  groups <- rep(1,length(outliers))

  res$stats <- matrix(c(lwhisker,qts[2:4],uwhisker),ncol=1)
  res$n <- n
  res$conf <- confmedian
  res$out <- outliers
  res$group <- groups
  res$names <- ""

  bxp(res,...)

}
 
