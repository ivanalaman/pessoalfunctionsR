cor.prob <- function(X, dfr=nrow(X)-2, round=2){
  R <- round(cor(X,use='na.or.complete'),round)
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2*dfr/(1-r2)
  R[above] <- round(1-pf(Fstat, 1, dfr),round)
  R
}
