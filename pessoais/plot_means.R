plot_means <- function(means,standarderror,letter=NULL,nrep,sig.level=0.05,xlabel=NULL,ylab=NULL,main=NULL,subtitle=NULL,size_numbers_ic=1,with_numbers=TRUE,type=c('Tukey','Dunnett'),...){
 #means - um vetor numérico com as médias
 #standarderror - um vetor numérico com os erros padrões da média
 #letter - um vetor de strings com os resultados do teste de médias (Tukey por exemplo)
 #nrep - o número de repetições do experimento
 #sig.level - o nível de significância para ser construído o intervalo de confiança para cada média.
  nmedias <- length(means)
  me <- standarderror*abs(qt(sig.level/2,nrep-1))
  
  switch(match.arg(type),
         Tukey = {
                  li <- means - me
                  lss <- means + me
                  limites <- seq(min(li)-0.02*min(li),max(lss)+0.02*max(lss),l=5)
                  if(is.null(letter))stop('Você deve fornecer um vetor com as letras')
                  par(mar=c(7.1,4.1,4.1,4))
                  plot(1:nmedias,means,axes=F,ylim=c(limites[1],limites[5]),pch=16,xlab="",ylab=ylab,...)
                  axis(1,at=1:nmedias,labels=names(means),...) 
                  axis(2,at=round(limites,1))
                  axis(3,at=1:nmedias,labels=letter)
                  segments(1:nmedias,li,1:nmedias,lss)
                  if(with_numbers==TRUE) text(1:nmedias,c(li,means,lss),round(c(li,means,lss),2),pos=4,col='gray4',xpd=TRUE,cex=size_numbers_ic)
                  mtext(main,side=3,line=3,font=2) 
                  mtext(xlabel,line=-30) 
                  mtext(subtitle,side=3,line=2,cex=0.8)                   
                },
         Dunnett = {
                    meansbruto <- means
                    clean1 <- sapply(meansbruto,function(x)gsub("\\D+$","",x))
                    clean2 <- as.numeric(clean1)
                    tadicional <- clean2[nmedias]
                    names(tadicional) <- names(meansbruto)[nmedias]
                    others1 <- clean2[-nmedias]
                    names(others1) <- names(meansbruto)[-nmedias]
                    others <- sort(others1,decreasing=FALSE)
                    li <- c(tadicional,others) - me
                    lss <- c(tadicional,others) + me 
                    limites <- seq(min(li)-0.02*min(li),max(lss)+0.02*max(lss),l=5)
                    #if(with_numbers==TRUE) text(1:nmedias,c(li,means,lss),round(c(li,means,lss),2),pos=4,col='gray4',xpd=TRUE,cex=size_numbers_ic)
                    letters1 <- sapply(meansbruto,function(x)gsub("([0-9]+.[0-9]+)","",x))
                    numlabels <- others1
                    names(numlabels) <- letters1[-nmedias]
                    ordletters <- sort(numlabels,decreasing=FALSE)
                    labletters <- names(ordletters)
                    
                    ifelse(is.null(letter),letter <- labletters,letter)
                    
                    par(mar=c(7.1,4.1,4.1,4))
                    plot(1:nmedias,c(tadicional,others),axes=F,ylim=c(limites[1],limites[5]),pch=16,xlab="",ylab=ylab,...)
                    axis(1,at=1:nmedias,labels=c(names(tadicional),names(others)),...) 
                    axis(2,at=round(limites,1))
                    axis(3,at=1:nmedias,labels=c("",letter))
                    segments(1:nmedias,li,1:nmedias,lss) 
                    if(with_numbers==TRUE) text(1:nmedias,c(li,means,lss),round(c(li,means,lss),2),pos=4,col='gray4',xpd=TRUE,cex=size_numbers_ic)
                    mtext(main,side=3,line=3,font=2)
                    mtext(xlabel,line=-30) 
                    mtext(subtitle,side=3,line=2,cex=0.8)  
                    }
         )
}
