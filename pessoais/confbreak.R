confbreak <- function(betaa,x,npars,sqe,sig.level=0.05){
  sqx <- sum((x - mean(x))^2)
  sbeta <- sqe/sqrt(sqx)
  glib <- length(x) - npars
  li <- betaa - abs(sbeta*qt(sig.level/2,glib))
  lsu <- betaa + abs(sbeta*qt(sig.level/2,glib))
  res <- c(betaa,li,lsu)
  names(res) <- c('Estimativa','LI','LS')
  res
}
