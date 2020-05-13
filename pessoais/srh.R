#Função para fatorial 2x2 não paramétrico. # Sokal RR, Rohlf FJ, 1995. Biometry, 3rd ed. New York: Freeman.
srh <-function(r, pf, sf, n=3){
  abn = (nlevels(pf)*nlevels(sf)*n)
  lm_rank <- lm(rank(r)~pf*sf)
  # the variance must be correct if there are ties
  aov_lm <- anova(lm_rank)
  ans <-  aov_lm[1:3,1:3]
  ans[,4] <- ans[,2]/(length(r)*(length(r)+1)/abn)
  ans[,5] <- (1-pchisq(ans[,4],ans[,1]))
  colnames(ans)[4:5] <- c("H","p-value")
  ans
}
