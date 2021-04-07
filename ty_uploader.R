#!/usr/bin/env Rscript

library(redcapAPI)
library(WordR)
library(officer)
library(Hmisc)
library(tidyverse)

pdb <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                        token = "CBF02E285BFC1874F0EAF11D3F4E2842", conn, project = 23166)
#pdb_data <- exportReports(pdb, 253531)
try(pdb_data <- exportReports(pdb, 253531), silent = TRUE)
if (exists("pdb_data")==FALSE) {print("No Updates")} else {
  
  path_in <- "C:/Users/sweelyb/Documents/resources/Templates/Thank You/Thank You Template.docx"
  
  for (i in 1:3) {
    e <- 5
    map_id <- as.character(pdb_data[i,"map_id"])
    if (nchar(map_id)==1) {input <- paste0("00",map_id)} else if (nchar(map_id)==2) {input <- paste0("0",map_id)} else {input <- map_id}
    vmac_id <- as.character(pdb_data[i,"vmac_id"])
    if (nchar(vmac_id)==1) {record <- paste0("0000",vmac_id)} else if (nchar(vmac_id)==2) {record <- paste0("000",vmac_id)} else if (nchar(vmac_id)==3) {record <- paste0("00",vmac_id)} else if (nchar(vmac_id)==4) {record <- paste0("0",vmac_id)} else {record <- vmac_id}
    last_name <- pdb_data[i, "last_name"]
    salutation <- as.character(pdb_data[i,"salutation"])
    
    epoch_conv <- c("enrollment","18 month","3 year","5 year","7 year","9 year","11 year","13 year")
    epoch <<- epoch_conv[e]
    epoch_next <<- epoch_conv[e+1]
    ep_conv <- c("18mos","36mos","60mos","7yr","9yr","11yr","13yr")
    ep_next <- ep_conv[e]
    date_next <- paste0("fu_date_estimate_",ep_next)
    date_ty <<- format(as.Date(pdb_data[i, date_next]), "%B %Y")
    
    output <- paste0("C:/Users/sweelyb/Documents/resources/Output/MAP_",input,"_ty_letter.docx")
    renderInlineCode(path_in, output)
    
    importFiles(rcon = pdb, file = output, record = record, field = "thank_7yr",
                overwrite = TRUE, repeat_instance = 1)
  }
}