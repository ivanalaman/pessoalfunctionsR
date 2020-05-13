conf.prop = function(x, success, conf.level = 0.95,na.rm=TRUE){
   if(is.numeric(x)) stop('Esta função é para variável categórica cabeção!!')
   if(!is.character(success)) stop('Quem é o sucesso? Coloque o nome entre aspas')
   format.perc <- function(probs, digits){
   ## Not yet exported, maybe useful in other contexts:
   ## quantile.default() sometimes uses a version of it
   paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits),
	  "%")
}   
   nsuccess = sum(x%in%success,na.rm=na.rm)
   n        = length(na.omit(x))
   pbarra = round(nsuccess/n,4)
   names(pbarra) = 'Proporção amostral'
   parm = names(pbarra)
   erropadrao = sqrt( (pbarra*(1-pbarra))/n)
   a = (1-conf.level)/2
   a = c(a, 1 - a)
   pct = format.perc(a,3)
   fac = qnorm(a)
   res <- list(est_pontual = pbarra[parm],
               erro_pad = erropadrao,
               quantil = fac,
               intervalo = pbarra[parm] + erropadrao * fac)
   res
}    
