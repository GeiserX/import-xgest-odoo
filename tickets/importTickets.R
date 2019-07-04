setwd("C:/Users/Sergio/Desktop/Scripts-R/tickets")
tickets <- read.table("WHD_Tickets.tsv", sep = "\t", stringsAsFactors = F, header = T, fileEncoding = "windows-1252")

tickets <- read.delim("WHD_Tickets.tsv", header = T, stringsAsFactors = F, fileEncoding = "UTF-8")

