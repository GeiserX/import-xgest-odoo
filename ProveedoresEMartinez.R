##################
###################
### TRANSCRIBIR SCRIPT

Mayusculas <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
        sep = "", collapse = " ")
}

xgest <- dbConnect(MySQL(), user="tecnico", password="PASSWORD", db="xgestevo", host="X.X.X.X", port=3307)
suppliers <- dbGetQuery(xgest, "select * from fcpro001")
for(i in 1:dim(suppliers)[2]){
  print(i)
  `Encoding<-`(as.character(suppliers[,i]), "latin1")
  
}

suppliers <- suppliers[c(-17, -21, -26),]
for(i in 1:dim(suppliers)[1]){
  suppliers$PPAIS[i] <- Mayusculas(suppliers$PPAIS[i])
}
suppliers$PPAIS <- str_replace_all(suppliers$PPAIS, "Malaga", "M?laga")
suppliers$PPAIS <- str_replace_all(suppliers$PPAIS, "Cieza", "Murcia")
suppliers$PPAIS <- str_replace_all(suppliers$PPAIS, "Girona", "Gerona")
suppliers$PPAIS <- str_replace_all(suppliers$PPAIS, "Lleida", "L?rida")
suppliers$PPAIS <- str_replace_all(suppliers$PPAIS, "Cadiz", "C?diz")
suppliers$PPAIS <- str_replace_all(suppliers$PPAIS, "NANA", "La Coru?a") #OJO que en la posici?n 732 y 750 es Espa?a
suppliers$PPAIS <- str_replace_all(suppliers$PPAIS, "Castellon", "Castell?n")
suppliers$PPAIS <- str_replace_all(suppliers$PPAIS, "Guipuzkoa", "Guipuzcoa")
suppliers$PPAIS <- str_replace_all(suppliers$PPAIS, "Cartagena", "Murcia")
suppliers$PPAIS <- str_replace_all(suppliers$PPAIS, "Guipuzkoa", "Guipuzcoa")
suppliers$PPAIS <- str_replace_all(suppliers$PPAIS, "Murica", "Murcia")
suppliers$PPAIS <- str_replace_all(suppliers$PPAIS, "Bizkaia", "Vizcaya")
suppliers$PPAIS <- str_replace_all(suppliers$PPAIS, "Ceuti", "Murcia")
suppliers$PPAIS <- str_replace_all(suppliers$PPAIS, "Francia", "")
suppliers$PPAIS <- str_replace_all(suppliers$PPAIS, "Ireland", "")
suppliers$PPAIS <- str_replace_all(suppliers$PPAIS, "China", "")
suppliers$PPAIS <- str_replace_all(suppliers$PPAIS, "Lancashire", "")
suppliers$PPAIS <- str_replace_all(suppliers$PPAIS, "Belgium", "")

write.csv(x = suppliers, file = "/var/www/html/suppliers.csv", fileEncoding = "ASCII")
