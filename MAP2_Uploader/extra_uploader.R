extra_uploader <- function(epoch,vmac) {
  
  library(redcapAPI)
  library(WordR)
  library(officer)
  library(Hmisc)
  library(tidyverse)
  
  # Global Pathing
  local <- 1
  online <- 0
  if (local) {
    # Add Local Paths Here
    out_path <- "C:/Users/sweelyb/Documents/output/"
    main_path <- "C:/Users/sweelyb/Documents/Letter_Auto/"
    
  } else if (online) {
    # Add Global Paths Here
    out_path <- "/app/"
    main_path <- "/srv/shiny-server/"
    
  }
  
  pdb <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "496ED1BD518B29CB96B5CFD9C48844FE", conn, project = 136242)
  EDC <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "09B0A6B7F207F51C6F656BAE567FA390", conn, project = 119047)
  
  pdb_datas <- exportReports(pdb, 267464)
  cond_datas <- exportReports(EDC,284613)
  
  path_in <- paste0(main_path,"resources/Templates/Thank You/Thank You Template.docx")
  
  events <- c("eligibility_arm_1","enrollmentbaseline_arm_1","18month_followup_arm_1","3year_followup_arm_1","5year_followup_arm_1","7year_followup_arm_1")
  pdb_datas <- pdb_datas[which(pdb_datas[,"redcap_event_name"]== events[epoch+1]),]
  ii <- which(pdb_datas["vmac_id"]==as.integer(vmac)) #need to find i for map id
  pdb_data <- pdb_datas[ii,]
  map_id <- pdb_data$map_id
  cond_datas <- cond_datas[which(cond_datas[,"redcap_event_name"]== events[epoch+1]),]
  cond_data <- cond_datas[which(cond_datas["map_id"]==as.integer(map_id)),]
  cond_data_brain <- cond_data[2,]
  cond_data <- cond_data[1,]
  
  # Finish This!!!
  #visit_sched = '1' & any one of EDC variables (np_complete, echo_complete, blood_complete, brain_complete, cmr_complete, int_ptp_complete) = '1' 
  
  nc <- cond_data$np_complete; if (is.na(nc)) {nc <- "No"}
  ec <- cond_data$echo_complete; if (is.na(ec)) {ec <- "No"}
  bc <- cond_data$blood_complete; if (is.na(bc)) {bc <- "No"}
  brc <- cond_data_brain$brain_complete; if (is.na(brc)) {brc <- "No"}
  cmr <- cond_data$cmr_complete; if (is.na(cmr)) {cmr <- "No"}
  ipc <- cond_data$int_ptp_complete; if (is.na(ipc)) {ipc <- "No"}
  
  #if (length(visit_sched)==0) {vs <- "No"} else {vs <- visit_sched}
  if (nc != "Yes" & ec != "Yes" & bc != "Yes" & brc != "Yes" & cmr != "Yes" & ipc != "Yes") {err <<- "Insufficient data for thank you letter."} else {
    err <<- ""
    
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
    ep_conv <- c("elig","enroll","18mos","3yr","5yr","7yr","9yr","11yr","13yr")
    ep_next <- ep_n_conv[e]
    ep <- ep_conv[e+1]
    
    if (e==0){
      el <-  redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                              token = "BFEF4C303E170DDAA850D56C4FB9594A", conn, project = 134345)
      el_data <- exportReports(el, 271784)
      
      ii <- which(el_data["vmac_id"]==as.integer(vmac)) #need to find i for map id
      
      elig_out <- el_data[ii,"elig_outcome"]
      enroll_stat <- el_data[ii,"enrollment_status"]
      
      path_in <- paste0(main_path,"resources/Templates/Thank You/Thank You Template_e.docx")
    } else {
      path_in <- paste0(main_path,"resources/Templates/Thank You/Thank You Template.docx")
      
      fu <<- "To date, your participation has helped generate more than 130 scientific publications and conference presentations, and it has provided significant teaching opportunities for more than 40 investigators and clinicians-in-training."
    }
    
    date_next <- paste0("visit_estimate_",ep_next,"date")
    date_ty <<- format(as.Date(pdb_data[i, date_next]), "%B %Y")
    
    output <- paste0(out_path,"MAP_",input,"_",ep,"_ty_letter.docx")
    renderInlineCode(path_in, output)
    
    importFiles(rcon = pdb, file = output, record = record, field = "card_thank_letter", event = pdb_data[,"redcap_event_name"],
                overwrite = TRUE, repeat_instance = 1)
  }
  
  return(err)
}
