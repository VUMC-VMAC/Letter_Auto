#!/usr/bin/env Rscript

library(redcapAPI)
library(WordR)
library(officer)
library(Hmisc)
library(tidyverse)

pdb <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                         token = "CBF02E285BFC1874F0EAF11D3F4E2842", conn, project = 23166)
#pdb_data <- exportReports(pdb, 252698)
try(pdb_data <- exportReports(pdb, 252698), silent = TRUE)
if (exists("pdb_data")==FALSE) {print("No Updates")} else {
  pdb_data[which(is.na(pdb_data[,"proxy_diff_address"])),"proxy_diff_address"]<- "No"
  
  #for (i in 1:nrow(pdb_data)) {
  i<-1
  e <- 5
  Epoch_conv <- c("Enrollment","18-Month","3-Year","5-Year","7-Year","9-Year","11-Year","13-Year")
  Epoc_conv <- c("enrollment","18-month","3-year","5-year","7-year","9-year","11-year","13-year")
  Epoch <- Epoch_conv[e]; Epoc <- Epoc_conv[e]
  epoch_conv <- c("18mos","36mos","60mos","7yr","9yr","11yr","13yr")
  epoch <- epoch_conv[e-1]
  
  map_id <- as.character(pdb_data[i,"map_id"])
  if (nchar(map_id)==1) {input <- paste0("00",map_id)} else if (nchar(map_id)==2) {input <- paste0("0",map_id)} else {input <- map_id}
  vmac_id <- as.character(pdb_data[i,"vmac_id"])
  if (nchar(vmac_id)==1) {record <- paste0("0000",vmac_id)} else if (nchar(vmac_id)==2) {record <- paste0("000",vmac_id)} else if (nchar(vmac_id)==3) {record <- paste0("00",vmac_id)} else if (nchar(vmac_id)==4) {record <- paste0("0",vmac_id)} else {record <- vmac_id}
  
  first_name <- pdb_data[i,"first_name"] #change to preferred name
  last_name <- pdb_data[i, "last_name"]
  street_address <- pdb_data[i, "street_address"]
  city <- pdb_data[i, "city"]
  state <- pdb_data[i, "state"]
  zipp <- pdb_data[i, "zip"]
  salutation <- as.character(pdb_data[i,"salutation"])
  
  if (pdb_data[i,"proxy_diff_address"] == "Yes") {
    proxy_address <<- pdb_data[i, "proxy_address"]
    proxy_city <<- pdb_data[i, "proxy_city"]
    proxy_state <<- pdb_data[i, "proxy_state"]
    proxy_zip <<- pdb_data[i, "proxy_zip"]
  } else {
    proxy_address <<- pdb_data[i, "street_address"]
    proxy_city <<- pdb_data[i, "city"]
    proxy_state <<- pdb_data[i, "state"]
    proxy_zip <<- pdb_data[i, "zip"]
  }
  
  fu_visit_type_7yr <- as.integer(pdb_data[i,"fu_visit_type_7yr"])
  
  add_day3<-""
  add_day2<-""
  location_day2<-""
  location_day3<-""
  map_7yr_date1 <- format(as.Date(pdb_data[i, "fu_date_7yr"]), "%m/%d/%Y")
  fu_time_7yr <- as.character(pdb_data[i,"fu_time_7yr"])
  fu_time_7yr <- paste0(gsub(":00$","",fu_time_7yr), "am")
  fu_hrs_7yr <- as.character(pdb_data[i,"fu_hrs_7yr"])
  location_day1 <- ""
  #fu_time_7yr <- sub("0","",fu_time_7yr)
  if (fu_visit_type_7yr==1 | fu_visit_type_7yr==3){
  map_7yr_date2 <- format(as.Date(pdb_data[i, "fu_date2_7yr"]), "%m/%d/%Y")
  fu_time2_7yr <- as.character(pdb_data[i,"fu_time2_7yr"])
  fu_time2_7yr <- paste0(gsub(":00$","",fu_time2_7yr),"am")
  fu_hrs2_7yr <- as.character(pdb_data[i,"fu_hrs2_7yr"])
  location_day2 <- ""
  add_day2 <- paste0("The second day of your visit is scheduled for ",map_7yr_date2," at ",fu_time2_7yr," and will last approximately ",fu_hrs2_7yr)
  #fu_time2_7yr <- sub("0","",fu_time2_7yr)
  if (fu_visit_type_7yr==3) {
  map_7yr_date3 <- format(as.Date(pdb_data[i, "fu_date3_7yr"]), "%m/%d/%Y")
  fu_time3_7yr <- as.character(pdb_data[i,"fu_time3_7yr"])
  fu_time3_7yr <- paste0(gsub(":00$","",fu_time3_7yr),"am")
  fu_hrs3_7yr <- as.character(pdb_data[i,"fu_hrs3_7yr"])
  location_day3 <- ""
  add_day3 <- paste0("The third day of your visit is scheduled for ",map_7yr_date3," at ",fu_time3_7yr," and will last approximately ",fu_hrs3_7yr)
  #fu_time3_7yr <- sub("0","",fu_time3_7yr)
  }
  if (fu_visit_type_7yr==4) {
    map_7yr_date4 <- format(as.Date(pdb_data[i, "fu_date4_7yr"]), "%m/%d/%Y")
    fu_time4_7yr <- as.character(pdb_data[i,"fu_time4_7yr"])
    fu_time4_7yr <- paste0(gsub(":00$","",fu_time4_7yr),"am")
    fu_hrs4_7yr <- as.character(pdb_data[i,"fu_hrs4_7yr"])
    location_day4 <- ""
    add_day4 <- paste0("The third day of your visit is scheduled for ",map_7yr_date4," at ",fu_time4_7yr," and will last approximately ",fu_hrs4_7yr)
  }
  }
  cdrq<-"";envel<-""
  if (fu_visit_type_7yr==7) {
    cdrq <- "a.	Because you will only be completing a phone interview and questionnaires, most of this consent document does not apply to you. There is a note on the first page of the document stating that you will be completing the questionnaires and interview by phone only. 
  b.	We have already marked \"No\" for each optional piece in the document because these items do not apply to you. 
  c.	We have enclosed two copies of the consent form labeled \'RETURN\' and \'KEEP\' on the top of the first page. 
  d.	Once you have read the consent form, on the version labeled \"RETURN\", place your initials on pages 2-4 and sign on pages 8 and 11 where indicated. Please send this version back to us using the enclosed envelope. 
  e.	The copy labeled \'KEEP\' is for you to keep for your records. 
  "
    envel <- "Stamped/Addressed Envelope. We have included a stamped and pre-addressed envelope for you to mail back your paperwork."
  }
  if (fu_visit_type_7yr==8) {
    cdrq <- "a.	Because you will only be completing a phone interview and questionnaires, most of this consent document does not apply to you. There is a note on the first page of the document stating that you will be completing the questionnaires and interview by phone only. 
  b.	We have already marked \"No\" for each optional piece in the document because these items do not apply to you. 
  c.	We have enclosed two copies of the consent form labeled \'RETURN\' and \'KEEP\' on the top of the first page. 
  d.	Once you have read the consent form, on the version labeled \"RETURN\", place your initials on pages 2-4 and sign on pages 8 and 11 where indicated. Please send this version back to us using the enclosed envelope. 
  e.	The copy labeled \'KEEP\' is for you to keep for your records. 
  "
    envel <- "Stamped/Addressed Envelope. We have included a stamped and pre-addressed envelope for you to mail back your paperwork."
  }
  if (fu_visit_type_7yr==9) {
    cdrq <- "a.	Because you will only be completing a phone interview and questionnaires, most of this consent document does not apply to you. There is a note on the first page of the document stating that you will be completing the questionnaires and interview by phone only. 
  b.	We have already marked \"No\" for each optional piece in the document because these items do not apply to you. 
  c.	We have enclosed two copies of the consent form labeled \'RETURN\' and \'KEEP\' on the top of the first page. 
  d.	Once you have read the consent form, on the version labeled \"RETURN\", place your initials on pages 2-4 and sign on pages 8 and 11 where indicated. Please send this version back to us using the enclosed envelope. 
  e.	The copy labeled \'KEEP\' is for you to keep for your records. 
  "
    envel <- "Stamped/Addressed Envelope. We have included a stamped and pre-addressed envelope for you to mail back your paperwork."
  }
  
  sex <- as.character(pdb_data[i, "sex"])
  gender <- ""
  if (sex=="Female") {gender<- "women"} else {gender <- "men"}
  if (sex=="Female") {gender_cap<- "Women"} else {gender_cap <- "Men"}
  proxy_sex <- as.character(pdb_data[i,"proxy_sex"])
  pronoun_conv <- c("Female" = "she", "Male" = "he")
  pronoun_conv_obj <- c("Female" = "her", "Male" = "him")
  pronoun_conv_poss <- c("Female" = "her", "Male" = "his")
  pronoun_conv_poss_cap <- c("Female" = "Her", "Male" = "His")
  pronoun <- pronoun_conv[sex]
  pronoun_obj <- pronoun_conv_obj[sex]
  pronoun_poss <- pronoun_conv_poss[sex]
  pronoun_poss_cap <- pronoun_conv_poss_cap[sex]
  
  
  proxy_pronoun <- pronoun_conv[proxy_sex]
  proxy_first_name <- pdb_data[i,"proxy_first_name"]
  proxy_last_name <- pdb_data[i,"proxy_last_name"]
  proxy_salutation <- as.character(pdb_data[i,"proxy_salutation"])
  proxy_sex <- pdb_data[i,"proxy_sex"]
  
  fu_transport_7yr <- pdb_data[i,"fu_transport_7yr"]
  if (is.na(fu_transport_7yr)) {fu_transport_7yr<-"No"}
  if (fu_transport_7yr=="Yes") {t_need <- "Your appointment will be held at the Vanderbilt University Medical Center.  We will be providing you with transportation to and from your visit with Jeff Cornelius. Jeff\'s number is (615) 604-1502 in case you need to contact him."
    t_need_proxy <- paste0(pronoun_poss_cap," appointment will be held at the Vanderbilt University Medical Center.  We will be providing transportation to and from the visit with Jeff Cornelius. Jeff\'s number is (615) 604-1502 in case you need to contact him.")
  } else {t_need <- ""; t_need_proxy <- ""}
  
  fu_proxy_7yr <- pdb_data[i,"fu_proxy_7yr"]
  if (is.na(fu_proxy_7yr)) {fu_proxy_7yr<-"No"}
  if (fu_proxy_7yr == "Yes") {p_req <- "Your presence is required for all study visits. Feel free to bring a book or other form of entertainment to keep you occupied during your waiting period."} else {
    p_req <- "Your presence is not required however you are welcome to attend."
  }
  
  fu_hotel_7yr <- pdb_data[i,"fu_hotel_7yr"]
  if (is.na(fu_hotel_7yr)) {fu_hotel_7yr<-"No"}
  if (fu_hotel_7yr=="Yes") {hotel <- "You will be residing at XX - located at XX - on the nights of DAY, MONTH DATE, YEAR and DAY, MONTH DATE, YEAR. Your hotel confirmation is: XXXXXXXX."
    hotel_proxy <- "We reserved a hotel room at XX - located at XX - on the nights of DAY, MONTH DATE, YEAR and DAY, MONTH DATE, YEAR. The hotel confirmation is: XXXXXXXX."
  } else {hotel <- "";hotel_proxy<-""}
  
  output <- paste0("C:/Users/sweelyb/Documents/resources/Output/MAP_",input,"_ptp_letter.docx")
  path_in <- paste0("C:/Users/sweelyb/Documents/resources/Templates/Follow Up/MAP_7yr_template.docx")
  renderInlineCode(path_in, output)
  importFiles(rcon = pdb, file = output, record = record, field = "fu_letter_7yr",
              overwrite = TRUE, repeat_instance = 1)
  
  if (is.na(proxy_first_name)==FALSE) {
    output_proxy <- paste0("C:/Users/sweelyb/Documents/resources/Output/MAP_",input,"_proxy_letter.docx")
    path_in_proxy <- paste0("C:/Users/sweelyb/Documents/resources/Templates/Follow Up/MAP_7yr_proxy_template.docx")
    renderInlineCode(path_in_proxy, output_proxy)
    importFiles(rcon = pdb, file = output_proxy, record = record, field = "fu_letter_7yr_proxy",
                overwrite = TRUE, repeat_instance = 1)
  }
  
  #}
}