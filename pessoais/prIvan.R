#suponhamos que temos o valor de x e queremos então encontrar o valor de y correspondente de acordo com a função acima. Se olharmos no gráfico existem infinitas soluções podemos então encontrar a menor de todas depois é só somar pi. Por exemplo; escolhemos um valor para x, marcamos no eixo das ordenadas(vertical) e, a partir dele, fazemos uma reta horizontal. O primeiro ponto onde esta reta encontra o gráfico da função, marcamos uma reta vertical; o ponto onde esta reta encontra a abcissa(eixo horizontal) é o valor de y que queremos.

prIvan <- function(v){ # v é o valor de y*sin(y) e queremos encontrar o valor de y
 if(v>0){
  k= v%/%(pi/2) 
  #
  i <- 0
  while((k*pi/2) < v){
   i <- i+1
   k <- (v+i)%/%(pi/2) 
  }
  # k
  if(k%%2==0&(k+1)%%4==1)k1 <- k+1
  #
  if(k%%2==0&(k+1)%%4==3)k1 <- k+3
  #
  if(k%%4==3)k1 <- k+2 
  #
  if(k%%4==1)k1 <- k
  #
  repeat{
   while((k1*(pi/2)*sin(k1*(pi/2))-v)>0) k1 <- k1-1e-2
   k1 <- k1+1e-2
   while((k1*(pi/2)*sin(k1*(pi/2))-v)>0) k1 <- k1-1e-4 
   k1 <- k1+1e-4
   while((k1*(pi/2)*sin(k1*(pi/2))-v)>0) k1 <- k1-1e-6
   break
  }
  #
  #
 }else{
  k= -v%/%(pi/2) 
  #
  i <- 0
  while(k*pi/2 < -v){
   i <- i+1
   k <- (-v+i)%/%(pi/2) 
  }
  # k
  if(k%%2==0&(k+1)%%4==1)k1 <- k+3
  #
  if(k%%2==0&(k+1)%%4==3)k1 <- k+1
  #
  if(k%%4==3)k1 <- k 
  #
  if(k%%4==1)k1 <- k+2 
  #
  repeat{
   while((k1*(pi/2)*sin(k1*(pi/2))-v)<0) k1 <- k1+1e-2
   k1 <- k1+1e-2
   while((k1*(pi/2)*sin(k1*(pi/2))-v)<0) k1 <- k1+1e-4 
   k1 <- k1+1e-4
   while((k1*(pi/2)*sin(k1*(pi/2))-v)<0) k1 <- k1+1e-6
   break
  }
 }   
 #
 y=k1*(pi/2)

 #ifelse(k1*(pi/2)>0, y=k1*(pi/2), y=-k1*(pi/2))
 # x <- y*sin(y)
 # r <- abs(x-v)
 #  
 #  res <- list(y,       
 #              x,
 #              v,
 #              r)
 #
 #  names(res) <- c('Valor de y tal que y*sin(y)=v',
 #                  'Valor aproximado de v; valor de y*sin(y)',
 #                  'Valor de v, valor do qual se quer a inversa',
 #                  'residual; |x-v|')
 return(y) 

}

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#FAZENDO O PLOT DA FUNÇÃO
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# pri <- prIvan(v=20.43)
# pri
# pri1 <- prIvan(v=-15.27)
# pri1[[1]]
# 
# n <- 52
# y <- seq(0,n, by=pi/8)
# x <- sin(y)*y 
# plot(y,round(x, 2),type='l')
# abline(h=20.43,v=pri[[1]],col=2)
# abline(h=0,col=3)
# abline(h=-15.27,v=pri1[[1]],col=4)
