#ADMINISTRACION 3015
#Administrador srvsafety14

Mayusculas <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
        sep = "", collapse = " ")
}

library(RMySQL)
library(stringr)

xgest <- dbConnect(MySQL(), user="XGESTEVO", password="PASSWORD", db="xgestevo", host="X.X.X.X", port=3307)

articulos <- dbGetQuery(xgest, "select * from fcart001")
familias <- dbGetQuery(xgest, "select * from fcfam001")

dbDisconnect(xgest)

articulos <- articulos[-1,]

for(i in 1:dim(articulos)[1]){
  if(!is.na(articulos$AFAMILIA[i])){
    if(articulos$AFAMILIA[i] %in% c(1, 2, 11, 20)){
      articulos$id[i] <- "__export__.product_category_4"
    } else if(articulos$AFAMILIA[i] == 3){
      articulos$id[i] <- "__export__.product_category_5"
    } else if(articulos$AFAMILIA[i] == 4){
      articulos$id[i] <- "__export__.product_category_6"
    } else if(articulos$AFAMILIA[i] %in% c(5, 13, 16, 19)){
      articulos$id[i] <- "__export__.product_category_7"
    } else if(articulos$AFAMILIA[i] == 6){
      articulos$id[i] <- "__export__.product_category_8"
    } else if(articulos$AFAMILIA[i] == 7){
      articulos$id[i] <- "__export__.product_category_9"
    } else if(articulos$AFAMILIA[i] == 8){
      articulos$id[i] <- "__export__.product_category_10"
    } else if(articulos$AFAMILIA[i] == 15){
      articulos$id[i] <- "__export__.product_category_11"
    } else{
      articulos <- articulos[-i,]
    }
  } else {
    articulos <- articulos[-i,]
  }
}

for(i in 1:dim(articulos)[1]){
  if(articulos$ANOINVENT[i] == "S") articulos$type[i] <- "Consumible"
  else articulos$type[i] <- "Almacenable"
  
  if(articulos$ABLOQUEADO[i] == "N") articulos$active[i] <- "True"
  else articulos$active[i] <- "False"
} 

splitted <- strsplit(gsub(" ", "", articulos$ASTAND), "\\.")
for(i in 1:dim(articulos)[1]){
  if(length(splitted[[i]]) != 0){
    articulos$pasillo[i] <- splitted[[i]][1]
    articulos$columna[i] <- splitted[[i]][2]
  } else {
    articulos$pasillo[i] <- ""
    articulos$columna[i] <- ""
  }
}

for(i in 1:dim(articulos)[1]){
  articulos$ADESCR[i] <- Mayusculas(articulos$ADESCR[i])
}


stock <- cbind(code=articulos$ACODAR, quantity=articulos$ASTOCK)

# Ojo en EM: Inversi?n Sujeto Pasivo
articulos <- cbind(idcatego= articulos$id, name=articulos$ADESCR, ref=articulos$ACODAR,
                   priceSELL=articulos$APVP1,
                   stock=articulos$ASTOCK, aviso=articulos$AAVISO, aviso2=articulos$AAVISO, 
                   type=articulos$type,
                   active=articulos$active, priceBUY=articulos$APRCOS, row=articulos$pasillo,
                   rack=articulos$columna)



write.csv(articulos, "C:\\Users\\Sergio\\Desktop\\Scripts-R\\articulos.csv")
write.csv(stock, "C:\\Users\\Sergio\\Desktop\\Scripts-R\\stock.csv")


