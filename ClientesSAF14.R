
Mayusculas <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
        sep = "", collapse = " ")
}

library(BBmisc)
library(RMySQL)
library(stringr)

xgest <- dbConnect(MySQL(), user="XGESTEVO", password="PASSWORD", db="xgestevo", host="x.x.x.x", port=3307)

clientes <- dbGetQuery(xgest, "select * from fccli001")
dbDisconnect(xgest)

clientes <- clientes[c(-1,-303,-305,-620, -606),]

for(i in 1:dim(clientes)[1]){
  clientes$CPAIS[i] <- Mayusculas(clientes$CPAIS[i])
}

for(i in 1:dim(clientes)[1]){
  if(clientes$CDNI[i] != ""){
    clientes$CDNI[i] <- paste0("ES",clientes$CDNI[i])
    
  }
  clientes$CPOB[i] <- Mayusculas(clientes$CPOB[i])
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

for(i in 1:dim(clientes)[1]){
  if(length(clientes$CDNI[i]) > 5){
    clientes$CDNI[i] <- paste0("ES", clientes$CDNI[i])
    clientes$CDNI[i] <- str_replace_all(clientes$CDNI[i], "-", "")
  }
  else{
    clientes$CDNI[i] <- ""
  }
  clientes$CPOB[i] <- Mayusculas(clientes$CPOB[i])
}

clientes$supplier <- "False"
clientes$iscompany <- "True" ## poner todos los clientes como compa??as
clientes$customer <- "True"
clientes$property_customer_payment_term_id <- "account.account_payment_term_immediate" ##plazo de pago cliente
clientes$property_customer_payment_method_id <- "__export__.payment_mode_1"
clientes$property_account_receivable_id <- "__export__.account_account_3301" # Cuenta contable cliente
clientes$property_account_payable_id <- "__export__.account_account_3289" # Cuenta contable proveedor

# for(i in 1:dim(clientes)[1]){
#   if(!is.null(clientes$PFORPA[i])){
#     if(clientes$PFORPA[i] == 1) clientes$property_payment_term_id[i] <- "__export__.account_payment_term_5"
#     else if(clientes$PFORPA[i] == 2) clientes$property_payment_term_id[i] <- "__export__.account_payment_term_7"
#     else if(clientes$PFORPA[i] == 12) clientes$property_payment_term_id[i] <- "__export__.account_payment_term_8"
#     else if(clientes$PFORPA[i] == 5) clientes$property_payment_term_id[i] <- "__export__.account_payment_term_9"
#     else if(clientes$PFORPA[i] == 13) clientes$property_payment_term_id[i] <- "__export__.account_payment_term_6"
#     else clientes$property_payment_term_id[i] <- "__export__.account_payment_term_5"
#   }
# }

clientes$notas <- paste(clientes$CDOMENVFRA, clientes$CINC, clientes$COBS)
clientes$messagesell <- paste(clientes$CRESCAR2, clientes$CRESCAR3)


clientes$state <- ""
clientes$CDOMBA <- ""


clientes$empresa <- ""

clientes$pais <- "base.es"
for(i in 1:dim(clientes)[1]){
  clientes$CNOM[i] <- Mayusculas(clientes$CNOM[i])
  clientes$CCODCL[i] <- paste0("SAF", clientes$CCODCL[i])
}

clientesCONCC <- data.frame(matrix(ncol = dim(clientes)[2], nrow = 0))
clientesSINCC <- data.frame(matrix(ncol = dim(clientes)[2], nrow = 0))
for(i in 1:dim(clientes)[1]){
  if(clientes$CIBAN[i] != ""){
    clientes$CBANCO[i] <- "iban"
    clientes$state[i] <- "/"
    clientes$CDOMBA[i] <- clientes$CIBAN[i]
    clientesCONCC <- rbind(clientesCONCC, clientes[i,])
  } else if(clientes$CENT[i] != ""){
    clientes$CBANCO[i] <- "iban"
    clientes$CDOMBA[i] <- paste0(clientes$CENT[i], clientes$CSUC[i], clientes$CDIG[i], clientes$CCUE[i], collapse = "")
    clientes$state[i] <- "/"
    clientesCONCC <- rbind(clientesCONCC, clientes[i,])
  } else clientesSINCC <- rbind(clientesSINCC, clientes[i,])
}


clientes <- clientesSINCC #clientesSINCC


clientes <- cbind(name=clientes$CNOM, `contact reference`=clientes$CCODCL, street=clientes$CDOM, `zip`=clientes$CCODPO,
                  city=clientes$CPOB, state=clientes$CPAIS, `country_id/id`=clientes$pais,
                  phone=clientes$CTEL1, fax=clientes$CFAX1, email=clientes$CMAIL1,
                  cbanco=clientes$CBANCO, 
                  cdomba=clientes$CDOMBA, notes=clientes$notas, `message for sales order`=clientes$messagesell,
                  supplier=clientes$supplier, `is a company`=clientes$iscompany,
                  customer=clientes$customer, 
                  property_customer_payment_term_id=clientes$property_customer_payment_term_id,
                  property_account_receivable_id=clientes$property_account_receivable_id, 
                  property_account_payable_id=clientes$property_account_payable_id, 
                  company=clientes$empresa,
                  property_customer_payment_mode_id = clientes$property_customer_payment_method_id)

write.csv(x = clientes, file = "C:\\Users\\Sergio\\Desktop\\Scripts-R\\clientesS14.csv")




