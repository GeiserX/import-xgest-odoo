Mayusculas <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
        sep = "", collapse = " ")
}

## Plazos de pago: __export__.account_payment_term_8
# Prepago __5
# Recibo a 30 dias f/f __7
# Recibo a 15 dias f/f __8
# Pagar? a 30 d?as f/f __9
# Recibo a la vista __6
library(stringr)
library(RMySQL)
xgest <- dbConnect(MySQL(), user="XGESTEVO", password="PASSWORD", db="xgestevo", host="x.x.x.x", port=3307)
#formapago <- dbGetQuery(xgest, "select GCODFP, GDESFP from fcfpg001")

proveedores <- dbGetQuery(xgest, "select * from fcpro001")
dbDisconnect(xgest)

for(i in 1:dim(proveedores)[1]){
  proveedores$PPAIS[i] <- Mayusculas(proveedores$PPAIS[i])
}
proveedores$PPAIS <- str_replace_all(proveedores$PPAIS, "Valencia", "Valencia / Val?ncia")
proveedores$PPAIS <- str_replace_all(proveedores$PPAIS, "Lleida", "L?rida / Lleida")
proveedores$PPAIS <- str_replace_all(proveedores$PPAIS, "Espa?a", "Barcelona")

proveedores <- proveedores[c(-1,-52),]
for(i in 1:dim(proveedores)[1]){
  if(proveedores$PDNI[i] != ""){
    proveedores$PDNI[i] <- paste0("ES",proveedores$PDNI[i])
    proveedores$PDNI[i] <- str_replace_all(proveedores$PDNI[i], "-", "")
  }
  proveedores$PPOB[i] <- Mayusculas(proveedores$PPOB[i])
}
proveedores$PDNI[48] <- ""

proveedores$supplier <- "True"
proveedores$iscompany <- "True"
proveedores$customer <- "False"
proveedores$property_payment_term_id <- "" ##plazo de pago cliente
proveedores$property_supplier_payment_term_id <- "account.account_payment_term_immediate" ##plazo de pago proveedor
proveedores$property_supplier_payment_method_id <- "__export__.payment_mode_1"
proveedores$property_account_receivable_id <- "__export__.account_account_3301" # Cuenta contable cliente
proveedores$property_account_payable_id <- "__export__.account_account_3289" # Cuenta contable proveedor


# for(i in 1:dim(proveedores)[1]){
#   if(proveedores$PFORPA[i] == 1) proveedores$property_supplier_payment_term_id[i] <- "__export__.account_payment_term_5"
#   else if(proveedores$PFORPA[i] == 2) proveedores$property_supplier_payment_term_id[i] <- "__export__.account_payment_term_7"
#   else if(proveedores$PFORPA[i] == 12) proveedores$property_supplier_payment_term_id[i] <- "__export__.account_payment_term_8"
#   else if(proveedores$PFORPA[i] == 5) proveedores$property_supplier_payment_term_id[i] <- "__export__.account_payment_term_9"
#   else if(proveedores$PFORPA[i] == 13) proveedores$property_supplier_payment_term_id[i] <- "__export__.account_payment_term_6"
#   else proveedores$property_supplier_payment_term_id[i] <- "__export__.account_payment_term_5"
# }


proveedores$empresa <- ""
for(i in 1:dim(proveedores)[1]){
  proveedores$PNOM[i] <- Mayusculas(proveedores$PNOM[i])
}
proveedores$paisempresa <- "Espa?a"

##############################################################################################################################
proveedores$notas <- paste(proveedores$POBSE, proveedores$PMAILENVIO, proveedores$PACTIVID)
proveedoresCONCC <- data.frame(matrix(ncol = dim(proveedores)[2], nrow = 0))
proveedoresSINCC <- data.frame(matrix(ncol = dim(proveedores)[2], nrow = 0))
proveedores$state <- ""
proveedores$PDOMBA <- ""


for(i in 1:dim(proveedores)[1]){
  if(proveedores$PIBAN[i] != ""){
    proveedores$PBANCO[i] <- "iban"
    proveedores$state[i] <- "/"
    proveedores$PDOMBA[i] <- proveedores$PIBAN[i]
    proveedoresCONCC <- rbind(proveedoresCONCC, proveedores[i,])
  } else if(proveedores$PENT[i] != ""){
    proveedores$PBANCO[i] <- "iban"
    proveedores$PDOMBA[i] <- paste0(proveedores$PENT[i], proveedores$PSUC[i], proveedores$PDIG[i], proveedores$PCUE[i], collapse = "")
    proveedores$state[i] <- "/"
    proveedoresCONCC <- rbind(proveedoresCONCC, proveedores[i,])
  } else proveedoresSINCC <- rbind(proveedoresSINCC, proveedores[i,])
}



proveedores <- proveedoresSINCC #proveedoresCONCC

proveedores <- cbind(name=proveedores$PNOM, contactreference=proveedores$PCOD , street=proveedores$PDOM, zip=proveedores$PCODPO,
                     city=proveedores$PPOB, state=proveedores$PPAIS, phone=proveedores$PTEL1,
                     mobile=proveedores$PTEL2, fax=proveedores$PFAX1, email=proveedores$PMAIL1,
                     bankstate=proveedores$PBANCO, tradename=proveedores$PNOMCOMERC, 
                     accountnumber=proveedores$PDOMBA, notes=proveedores$notas, messagepurchaseorder=proveedores$PRESCAR3,
                     supplier=proveedores$supplier, isacompany=proveedores$iscompany,
                     customer=proveedores$customer, company=proveedores$empresa, country=proveedores$paisempresa,
                     property_supplier_payment_term_id=proveedores$property_supplier_payment_term_id,
                     property_account_receivable_id=proveedores$property_account_receivable_id, 
                     property_account_payable_id=proveedores$property_account_payable_id,
                     tin=proveedores$PDNI, property_supplier_payment_method_id = proveedores$property_supplier_payment_method_id)
##############################################################################################################################

write.csv(x = proveedores, file = "C:\\Users\\Sergio\\Desktop\\Scripts-R\\proveedoresSAF.csv")
