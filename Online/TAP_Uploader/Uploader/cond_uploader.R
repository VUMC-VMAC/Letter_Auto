cond_uploader <- function(epoch,vmac) {
  
  library(redcapAPI)
  library(WordR)
  library(officer)
  library(Hmisc)
  library(tidyverse)
  
  pdb <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "0E16F65FB0A51C570781384D91AA1A78", conn, project = 137402)
  pdb_datas <- exportReports(pdb, 270431)
  
  i <- which(pdb_datas["vmac_id"]==as.integer(vmac))
  
  pdb_data <- pdb_datas[i,]
  
  events <- c("enrollmentbaseline_arm_1","18month_followup_arm_1","3year_followup_arm_1","5year_followup_arm_1","7year_followup_arm_1")
  
  e <- epoch
  
  vmac_id <- as.character(pdb_data$vmac_id)
  if (nchar(vmac_id)==1) {record <- paste0("0000",vmac_id)} else if (nchar(vmac_id)==2) {record <- paste0("000",vmac_id)} else if (nchar(vmac_id)==3) {record <- paste0("00",vmac_id)} else if (nchar(vmac_id)==4) {record <- paste0("0",vmac_id)} else {record <- vmac_id}
  input <- record
  
  first_name <<- pdb_data$first_name
  last_name <<- pdb_data$last_name
  salutation <<- as.character(pdb_data$salutation)
  
  epoch_conv <- c("enrollment","18 month","3 year","5 year","7 year","9 year","11 year","13 year")
  epoch <<- epoch_conv[e]
  epoch_next <<- epoch_conv[e+1]
  ep_conv <- c("base","1yr","2yr","3yr","4yr")
  ep_next <- ep_conv[e+1]
  ep <- ep_conv[e]
  
  sex <<- as.character(pdb_data$sex)
  gender <- ""
  if (sex=="Female") {gender<<- "women"} else {gender <<- "men"}
  if (sex=="Female") {gender_cap<<- "Women"} else {gender_cap <<- "Men"}
  pronoun_conv<<- c("Female" = "she", "Male" = "he")
  pronoun_conv_obj<<- c("Female" = "her", "Male" = "him")
  pronoun_conv_poss<<- c("Female" = "her", "Male" = "his")
  pronoun_conv_poss_cap<<- c("Female" = "Her", "Male" = "His")
  pronoun<<- pronoun_conv[sex]
  pronoun_obj<<- pronoun_conv_obj[sex]
  pronoun_poss<<- pronoun_conv_poss[sex]
  pronoun_poss_cap<<- pronoun_conv_poss_cap[sex]
  
  proxy_sex <<- as.character(pdb_data[i,"proxy_sex"])
  proxy_pronoun <<- pronoun_conv[proxy_sex]
  proxy_first_name <<- pdb_data[i,"proxy_first_name"]
  proxy_last_name <<- pdb_data[i,"proxy_last_name"]
  proxy_salutation <<- as.character(pdb_data[i,"proxy_salutation"])
  
  el_conv <- c("year", "2 years", "3 years", "4 years", "5 years")
  enroll_length <<- el_conv[e]
  
  path_in <- paste0("/srv/shiny-server/resources/Templates/Condolence/TAP_cond_temp.docx")
  
  output <- paste0("/app/Output/TAP_",input,"_condolence_letter.docx")
  renderInlineCode(path_in, output)
  
  importFiles(rcon = pdb, file = output, record = record, field = "card_cond_letter", event = events[e],
              overwrite = TRUE, repeat_instance = 1)
  }
  
#}
