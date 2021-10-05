ty_uploader <- function(epoch,vmac) {
  
  library(redcapAPI)
  library(WordR)
  library(officer)
  library(Hmisc)
  library(tidyverse)
  
  pdb <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "7E1DC8A562246EC4F7043579B863706C", conn, project = 136242)
  pdb_data <- exportReports(pdb, 267464)
  
  path_in <- "/srv/shiny-server/resources/Templates/Thank You/Thank You Template.docx"
  
  events <- c("eligibility_arm_1","enrollmentbaseline_arm_1","18month_followup_arm_1","3year_followup_arm_1","5year_followup_arm_1")
  pdb_datas <- pdb_data[which(pdb_data[,"redcap_event_name"]== events[epoch+1]),]
  ii <- which(pdb_datas["vmac_id"]==as.integer(vmac)) #need to find i for map id
  pdb_data <- pdb_datas[ii,]
  
  i <- 1
  e <- epoch
  map_id <- as.character(pdb_data[i,"map_id"])
  if (nchar(map_id)==1) {input <- paste0("00",map_id)} else if (nchar(map_id)==2) {input <- paste0("0",map_id)} else {input <- map_id}
  #if (nchar(map_id)==1) {input <- paste0("00",map_id)} else if (nchar(map_id)==2) {input <- paste0("0",map_id)} else {input <- map_id}
  vmac_id <- as.character(pdb_data[i,"vmac_id"])
  if (nchar(vmac_id)==1) {record <- paste0("0000",vmac_id)} else if (nchar(vmac_id)==2) {record <- paste0("000",vmac_id)} else if (nchar(vmac_id)==3) {record <- paste0("00",vmac_id)} else if (nchar(vmac_id)==4) {record <- paste0("0",vmac_id)} else {record <- vmac_id}
  last_name <<- pdb_data[i, "last_name"]
  salutation <<- as.character(pdb_data[i,"salutation"])
  
  epoch_conv <- c("enrollment","18 month","3 year","5 year","7 year","9 year","11 year","13 year")
  epoch <<- epoch_conv[e]
  epoch_next <<- epoch_conv[e+1]
  ep_n_conv <- c("","3yr_","5yr_","7yr_","9yr_","11yr_","13yr_")
  ep_conv <- c("18mos","3yr","5yr","7yr","9yr","11yr","13yr")
  ep_next <- ep_n_conv[e]
  ep <- ep_conv[e-1]
  
  
  
  date_next <- paste0("visit_estimate_",ep_next,"date")
  date_ty <<- format(as.Date(pdb_data[i, date_next]), "%B %Y")
  
  output <- paste0("/app/Output/MAP_",input,"_",ep,"_ty_letter.docx")
  renderInlineCode(path_in, output)
  
  importFiles(rcon = pdb, file = output, record = record, field = "card_thank_letter", event = pdb_data[,"redcap_event_name"],
              overwrite = TRUE, repeat_instance = 1)
  }
  
#}
