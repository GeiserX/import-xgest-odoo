ptm <- proc.time()

Mayusculas <- function(v1) {
  gsub("\\b(.)", "\\U\\1", tolower(v1), perl=TRUE)
}
       
library(BBmisc)
library(RMySQL)
library(stringr)

xgest <- dbConnect(MySQL(), user="tecnico", password="PASSWORD", db="xgestevo", host="X.X.X.X", port=3307)
clientes <- dbGetQuery(xgest, "select * from fccli001")
dbDisconnect(xgest)


clientes <- clientes[c(-1),]

clientes$CNOM <- Mayusculas(clientes$CNOM)
clientes$CDOM <- Mayusculas(clientes$CDOM)
clientes$CPAIS <- Mayusculas(clientes$CPAIS)
clientes$CPOB <- Mayusculas(clientes$CPOB)


# library(parallel)
# ptm <- proc.time()
# # Calculate the number of cores
# no_cores <- detectCores()
# # Initiate cluster
# cl <- makeCluster(no_cores)
# clusterExport(cl=cl, varlist = c("Mayusculas", "clientes", "str_replace_all"))
# 
# 
# invisible({
#   parLapply(cl, 1:dim(clientes)[1], function(i) {
#     clientes$CNOM[i] <- Mayusculas(clientes$CNOM[i])
#     clientes$CDOM[i] <- Mayusculas(clientes$CDOM[i])
#     clientes$CPAIS[i] <- Mayusculas(clientes$CPAIS[i])
#     
#     
#     
#     
#     clientes$CPOB[i] <- Mayusculas(clientes$CPOB[i])
#     
#   })
# })
# stopCluster(cl)
# tiempoAdoptCluster <- proc.time() - ptm

for(i in 1:dim(clientes)[1]){
  clientes$CTEL1[i] <- paste(clientes$CTEL1[i], clientes$CTEL2[i], sep = "/")
  
  if(clientes$CDNI[i] != ""){
    clientes$CDNI[i] <- paste0("ES",clientes$CDNI[i])
    clientes$CDNI[i] <- str_replace_all(clientes$CDNI[i], "-", "")
  }
  
  if(clientes$CIBAN[i] != ""){
    clientes$CBANCO[i] <- "iban"
    clientes$CDOMBA[i] <- clientes$CIBAN[i]
  } else if(clientes$CENT[i] != ""){
    clientes$CBANCO[i] <- "iban"
    clientes$CDOMBA[i] <- paste0(clientes$CENT[i], clientes$CSUC[i], clientes$CDIG[i], clientes$CCUE[i], collapse = "")
  }
}


clientes$CPAIS <- str_replace_all(clientes$CPAIS, "Malaga", "M?laga")
clientes$CPAIS <- str_replace_all(clientes$CPAIS, "Mucia", "Murcia")
clientes$CPAIS <- str_replace_all(clientes$CPAIS, "Murica", "Murcia")
clientes$CPAIS <- str_replace_all(clientes$CPAIS, "Espa?a", "")
clientes$CPAIS <- str_replace_all(clientes$CPAIS, "Almeria", "Almer?a")
clientes$CPAIS <- str_replace_all(clientes$CPAIS, "Los Alcazares", "Murcia")
clientes$CPAIS <- str_replace_all(clientes$CPAIS, "Cartagena", "Murcia")
clientes$CPAIS <- str_replace_all(clientes$CPAIS, "Alicante", "Alicante / Alacant")
clientes$CPAIS <- str_replace_all(clientes$CPAIS, "Valencia", "Valencia / Val?ncia")


clientes$supplier <- "False"
clientes$iscompany <- "True" ## poner todos los clientes como compa
clientes$customer <- "True"
clientes$property_customer_payment_term_id <- "account.account_payment_term_immediate" ##plazo de pago cliente
clientes$property_customer_payment_method_id <- "__export__.payment_mode_1"
clientes$property_account_receivable_id <- "__export__.account_account_3301" # Cuenta contable cliente
clientes$property_account_payable_id <- "__export__.account_account_3289" # Cuenta contable proveedor


clientes$notas <- paste(clientes$CDOMENVFRA, clientes$CINC, clientes$COBS)
clientes$messagesell <- paste(clientes$CRESCAR2, clientes$CRESCAR3)


clientes$empresa <- ""

clientes$pais <- "base.es"

clientesCONCC <- clientes[which(clientes$CBANCO == "iban"),]
clientesSINCC <- clientes[which(clientes$CBANCO != "iban"),]

clientes <- clientesSINCC #clientesSINCC


clientes <- cbind(name=clientes$CNOM, `contact reference`=clientes$CCODCL, street=clientes$CDOM, zip=clientes$CCODPO,
                  city=clientes$CPOB, state=clientes$CPAIS, `country_id/id`=clientes$pais,
                  phone=clientes$CTEL1, fax=clientes$CFAX1, email=clientes$CMAIL1, tin=clientes$CDNI,
                  `banks/account_number`=clientes$CBANCO, `banks/bank_account_type`=clientes$CDOMBA,
                  notes=clientes$notas, `message for sales order`=clientes$messagesell,
                  supplier=clientes$supplier, `is a company`=clientes$iscompany,
                  customer=clientes$customer, 
                  `property_customer_payment_term/id`=clientes$property_customer_payment_term_id,
                  `property_account_receivable/id`=clientes$property_account_receivable_id, 
                  `property_account_payable/id`=clientes$property_account_payable_id, 
                  company=clientes$empresa,
                  `property_customer_payment_mode/id` = clientes$property_customer_payment_method_id)

write.csv(x = clientes, file = "C:\\Users\\Sergio\\Desktop\\Scripts-R\\clientesIL.csv")

time <- proc.time() - ptm