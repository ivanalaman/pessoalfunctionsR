linear_distance <- function(origem,destino){
 #As coordenadas devem estar em graus e não em UTM!
 #origem -> é um vetor do tipo c(latitude,longitude)
 #destino -> é um vetor do tipo c(latitude,longitude)
 lat_o <- origem[1]
 lon_o <- origem[2]
 lat_d <- destino[1]
 lon_d <- destino[2]
 res <- (6371*acos(cos((90-(lat_d))*(pi/180))*cos((90-lat_o)*(pi/180))+sin((90-(lat_d))*(pi/180))*sin((90-lat_o)*(pi/180))*cos((lon_o-(lon_d))*(pi/180))))
 return(res)
}