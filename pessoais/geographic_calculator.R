geographic_calculator <- function(a_semieixomaior = NULL,
                                  b_semieixomenor = NULL,
                                  fuso = NULL,
                                  hemisferio = NULL,
                                  coordenadas = NULL){
###### Conversor de coordenadas geográficas  WGS84
### De UTM para graus
  # a_semieixomaior -> um escalar numérico
  # b_semieixomenor -> um escalar numérico
  # fuso            -> um escalar numérico
  # hemisferio      -> um escalar string
  # coordenadas     -> um vetor de comprimento igual a dois iguais a c(X,Y). A ordem deve ser obrigatoriamente X, Y.
  options(digits=15)
  
  if(is.null(a_semieixomaior)) a_semieixomaior <- 6378137
  if(is.null(b_semieixomenor)) b_semieixomenor <- 6356752.314
  if(is.null(fuso)) stop('Forneça um fuso horário')
  if(is.null(hemisferio)) stop('Forneça um hemisfério')
  if(is.null(coordenadas)) stop('Forneça um vetor com as coordenadas UTM X e Y')
  
  excentricidade <- sqrt(a_semieixomaior^2 - b_semieixomenor^2)/a_semieixomaior
  segundaexcentri <- sqrt(a_semieixomaior^2 - b_semieixomenor^2)/b_semieixomenor
  segundaexcentriquadrado <- segundaexcentri^2
  raio_polar_curvatura <- a_semieixomaior^2/b_semieixomenor
  
  y_al_sur_del_equador <- function(hemisferio,Y){
    res <- ifelse(hemisferio == 'S', Y[2] - 1e7, Y[2]) 
    return(unlist(res))                              
  }
  
  yalequad <- y_al_sur_del_equador(hemisferio=hemisferio,Y=coordenadas)
  
  filinha <- yalequad/(6366197.724*0.9996)
  ni <- (raio_polar_curvatura/(1 + segundaexcentriquadrado*(cos(filinha))^2)^(1/2))*0.9996
  
  meridiano_central <- 6*fuso - 183
  
  a <- (coordenadas[1]-5*1e5)/ni
  
  A1 <- sin(2*filinha)
  A2 <- A1*(cos(filinha)^2)
  J2 <- filinha+A1/2
  J4 <- (3*J2 + A2)/4
  J6 <- (5*J4 + A2*(cos(filinha)^2))/3
  alfa <- 3/4*segundaexcentriquadrado
  betaa <- 5/3*alfa^2
  gammaa <- 35/27*alfa^3
  Bfi <- 0.9996*raio_polar_curvatura*(filinha-(alfa*J2)+(betaa*J4)-(gammaa*J6))
  b <- (yalequad - Bfi)/ni
  zeta <- ((segundaexcentriquadrado*a^2)/2)*(cos(filinha)^2)
  xi <- a*(1-(zeta/3))
  eta <- b*(1-zeta) + filinha
  senhxi <- (exp(xi) - exp(-xi))/2
  delta_lam <- atan(senhxi/cos(eta))
  tau <- atan(cos(delta_lam)*tan(eta))
  firadianos <- filinha+(1+segundaexcentriquadrado*(cos(filinha))^2-(3/2)*segundaexcentriquadrado*sin(filinha)*cos(filinha)*(tau-filinha))*(tau-filinha)
  
  latitude <- (firadianos/pi)*180
  longitude <- (delta_lam/pi)*180 + meridiano_central
  
  res <- c(latitude,longitude)
  names(res) <- c('latitude','longitude')
  return(res)
}



