blood_uploader <- function(epochh,vmac) {
  
  library(redcapAPI)
  library(WordR)
  library(officer)
  library(Hmisc)
  library(tidyverse)
  
  # Global Pathing
  local <- 0
  online <- 1
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
                          token = "0E16F65FB0A51C570781384D91AA1A78", conn, project = 137402)
  pdb_datas <- exportReports(pdb, 307079)
  
  events <- c("enrollmentbaseline_arm_1","18month_followup_arm_1","3year_followup_arm_1","5year_followup_arm_1","7year_followup_arm_1")
  
  pdb_datas <- pdb_datas[which(as.integer(as.factor(pdb_datas[,"redcap_event_name"]))== epochh),]
  
  
  i <- which(pdb_datas["vmac_id"]==as.integer(vmac))
  
  pdb_data <- pdb_datas[i,]
  
  
  # NP Norm Scores
  dde <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "0676EDF59CA88377654227BB56028EEE", conn, project = 124389)
  dde2 <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                           token = "F009A86ED43B42332AC26C21411F37BC", conn, project = 140071)
  
  edc <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "A2F023358C81C065E1D98575795DCD5B", conn, project = 135160)
  np <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                         token = "79321D56C26DD34A7F03A318096D77F4", conn, project = 136221)
  np_datas <- exportReports(np, 280482)
  edc_datas <- exportReports(edc, 280483)
  
  edc_datas <- edc_datas[which(edc_datas["vmac_id"]==as.integer(vmac)),]
  
  edc_data <- edc_datas[which(is.na(edc_datas$redcap_repeat_instrument)),]
  brain_data <- edc_datas[which(is.na(edc_datas$redcap_repeat_instrument)==FALSE),]
  
  np_datas <- np_datas[grep(vmac,np_datas$record_id),]
  
  np_data <- np_datas[grep("--1",np_datas$record_id),]
  
  bc <- edc_data$blood_complete; if (is.na(bc)) {bc <- "No"}
  ec <- edc_data$echo_complete; if (is.na(ec)) {ec <- "No"}
  nc <- edc_data$np_complete; if (is.na(nc)) {nc <- "No"}
  
  
  
  
  #map_id <- pdb_data$map_id
  #if (nchar(map_id)==1) {inp <- paste0("00",map_id)} else if (nchar(map_id)==2) {inp <- paste0("0",map_id)} else {inp <- map_id}
  
  err <- ""
  
  e <- epochh
  Epoch_conv <- c("Enrollment","1-Year","3-Year","5-Year","7-Year","9-Year","11-Year","13-Year")
  Epoc_conv <- c("enrollment","1-year","3-year","5-year","7-year","9-year","11-year","13-year")
  Epoch <<- Epoch_conv[e]; Epoc <<- Epoc_conv[e]
  Epoch2 <<- Epoch_conv[e-2]; Epoc2 <<- Epoc_conv[e-2]
  Epoch1 <<- Epoch_conv[e-1]; Epoc1 <<- Epoc_conv[e-1]
  epoch_conv <- c("enroll","1yr","36mos","60mos","7yr","9yr","11yr","13yr")
  epoch2 <<- epoch_conv[e-2]
  epoch1 <<- epoch_conv[e-1]
  epoch <<- epoch_conv[e]
  ep_next <<- epoch_conv[e+1]
  ep <- epoch
  #date_next <<- paste0("visit_estimate_",ep_next,"_date")
  #date_ty <<- format(as.Date(pdb_data[, date_next]), "%B %Y")
  
  
  vmac_id <- as.character(pdb_data[,"vmac_id"])
  ind <- as.integer(vmac_id)
  
  i<-1
  
  enroll_date <<- format(as.Date(pdb_data$visit1_date), "%m/%d/%Y")
  
  first_name <<- pdb_data$preferred_name
  if (is.na(first_name)) {first_name <<- pdb_data$first_name}
  last_name <<- pdb_data$last_name
  street_address <<- pdb_data$street_address
  city <<- pdb_data$city
  state <<- pdb_data$state
  zipp <<- pdb_data$zip
  dob <<- format(as.Date(pdb_data$dob), "%m/%d/%Y")
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
  salutation<<- as.character(pdb_data$salutation)
  #fb_date <<- pdb_data$feedback_date
  #if (is.na(fb_date)) {feedback_date <<- "MISSING"} else {feedback_date <<- format(as.Date(fb_date), "%m/%d/%Y")}
  #feedback_location <<- as.character(pdb_data[i, "feedback_location"])
  #if (is.na(feedback_location)) {feedback_location <<- "MISSING"}
  bld_date_time <<- format(as.Date(edc_data$bld_date), "%m/%d/%Y")
  feedback_date <<- format(as.Date(pdb_data$feedback_incidental_stat_blood_date), "%m/%d/%Y")
  
  # Compiling Physician Data
  first_name_physician1<<- pdb_data[i, "feedback_incidental_stat_blood_physician1_first_name"]
  last_name_physician1<<- pdb_data[i, "feedback_incidental_stat_blood_physician1_last_name"]
  credentials1<<- pdb_data[i,"feedback_incidental_stat_blood_physician1_credentials"]
  street_address_physician1<<- pdb_data[i, "feedback_incidental_stat_blood_physician1_street_address"]
  city_physician1<<- pdb_data[i, "feedback_incidental_stat_blood_physician1_city"]
  state_physician1<<- pdb_data[i, "feedback_incidental_stat_blood_physician1_state"]
  zip_physician1<<- pdb_data[i, "feedback_incidental_stat_blood_physician1_zip"]
  first_name_physician2<<- pdb_data[i, "feedback_incidental_stat_blood_physician2_first_name"]
  last_name_physician2<<- pdb_data[i, "feedback_incidental_stat_blood_physician2_last_name"]
  credentials2<<- pdb_data[i,"feedback_incidental_stat_blood_physician2_credentials"]
  street_address_physician2<<- pdb_data[i, "feedback_incidental_stat_blood_physician2_street_address"]
  city_physician2<<- pdb_data[i, "feedback_incidental_stat_blood_physician2_city"]
  state_physician2<<- pdb_data[i, "feedback_incidental_stat_blood_physician2_state"]
  zip_physician2<<- pdb_data[i, "feedback_incidental_stat_blood_physician2_zip"]
  first_name_physician3<<- pdb_data[i, "feedback_incidental_stat_blood_physician3_first_name"]
  last_name_physician3<<- pdb_data[i, "feedback_incidental_stat_blood_physician3_last_name"]
  credentials3<<- pdb_data[i,"feedback_incidental_stat_blood_physician3_credentials"]
  street_address_physician3<<- pdb_data[i, "feedback_incidental_stat_blood_physician3_street_address"]
  city_physician3<<- pdb_data[i, "feedback_incidental_stat_blood_physician3_city"]
  state_physician3<<- pdb_data[i, "feedback_incidental_stat_blood_physician3_state"]
  zip_physician3<<- pdb_data[i, "feedback_incidental_stat_blood_physician3_zip"]
  first_name_physician4<<- pdb_data[i, "feedback_incidental_stat_blood_physician4_first_name"]
  last_name_physician4<<- pdb_data[i, "feedback_incidental_stat_blood_physician4_last_name"]
  credentials4<<- pdb_data[i,"feedback_incidental_stat_blood_physician4_credentials"]
  street_address_physician4<<- pdb_data[i, "feedback_incidental_stat_blood_physician4_street_address"]
  city_physician4<<- pdb_data[i, "feedback_incidental_stat_blood_physician4_city"]
  state_physician4<<- pdb_data[i, "feedback_incidental_stat_blood_physician4_state"]
  zip_physician4<<- pdb_data[i, "feedback_incidental_stat_blood_physician4_zip"]
  #first_name_physician5<<- pdb_data[i, "feedback_incidental_stat_blood_physician5_first_name"]
  #last_name_physician5<<- pdb_data[i, "feedback_incidental_stat_blood_physician5_last_name"]
  #street_address_physician5<<- pdb_data[i, "feedback_incidental_stat_blood_physician5_street_address"]
  #credentials5<<- pdb_data[i,"feedback_incidental_stat_blood_physician5_credentials"]
  #city_physician5<<- pdb_data[i, "feedback_incidental_stat_blood_physician5_city"]
  #state_physician5<<- pdb_data[i, "feedback_incidental_stat_blood_physician5_state"]
  #zip_physician5<<- pdb_data[i, "feedback_incidental_stat_blood_physician5_zip"]
  
  if (is.na(first_name_physician1)) {first_name_physician1<<- ""}
  if (is.na(last_name_physician1)) {last_name_physician1<<- ""}
  if (is.na(street_address_physician1)) {street_address_physician1<<- ""}
  if (is.na(city_physician1)) {city_physician1<<- ""}
  if (is.na(state_physician1)) {state_physician1<<- ""}
  if (is.na(zip_physician1)) {zip_physician1<<- ""}
  if (is.na(first_name_physician2)) {first_name_physician2<<- ""}
  if (is.na(last_name_physician2)) {last_name_physician2<<- ""}
  if (is.na(street_address_physician2)) {street_address_physician2<<- ""}
  if (is.na(city_physician2)) {city_physician2<<- ""}
  if (is.na(state_physician2)) {state_physician2<<- ""}
  if (is.na(zip_physician2)) {zip_physician2<<- ""}
  if (is.na(first_name_physician3)) {first_name_physician3<<- ""}
  if (is.na(last_name_physician3)) {last_name_physician3<<- ""}
  if (is.na(street_address_physician3)) {street_address_physician3<<- ""}
  if (is.na(city_physician3)) {city_physician3<<- ""}
  if (is.na(state_physician3)) {state_physician3<<- ""}
  if (is.na(zip_physician3)) {zip_physician3<<- ""}
  if (is.na(first_name_physician4)) {first_name_physician4<<- ""}
  if (is.na(last_name_physician4)) {last_name_physician4<<- ""}
  if (is.na(street_address_physician4)) {street_address_physician4<<- ""}
  if (is.na(city_physician4)) {city_physician4<<- ""}
  if (is.na(state_physician4)) {state_physician4<<- ""}
  if (is.na(zip_physician4)) {zip_physician4<<- ""}
  #if (is.na(first_name_physician5)) {first_name_physician5<<- ""}
  #if (is.na(last_name_physician5)) {last_name_physician5<<- ""}
  #if (is.na(street_address_physician5)) {street_address_physician5<<- ""}
  ##if (is.na(city_physician5)) {city_physician5<<- ""}
  #if (is.na(state_physician5)) {state_physician5<<- ""}
  #if (is.na(zip_physician5)) {zip_physician5<<- ""}
  
  print("Creating Data Tables")
  
  weight <- as.integer(edc_data$weight)
  df2 <- data.frame(
    Test1 = c("Cholesterol", "Cholesterol", "Cholesterol", "Cholesterol", "Blood Sugar", "Blood Sugar", "Blood Sugar", "Thyroid", "Inflammation"),
    Test2 = c("Total","HDL", "LDL", "Triglycerides", "Hemoglobin A1C", "Fasting Insulin", "Fasting Glucose", "Thyroid Stimulating Hormone (TSH)", "High Sensitivity C-Reactive Protein"),
    CR = as.character(c(edc_data$bld_c_chol, edc_data$bld_c_hdlc, edc_data$bld_c_ldlc, edc_data$bld_c_trig,edc_data$bld_c_hgba1c, edc_data$bld_c_insulin, edc_data$bld_c_glucose, edc_data$bld_c_tsh, edc_data$bld_c_crp)),
    NR = c("<200", "men >40, women >50", "<100", "<150", "4-6.5", "<17.2", "70-110", "0.3-5.0", "0.1-3.0")
  )
  
  #################################################################  
  ###### Review code below this part and merge with code above ####
  #################################################################
  #df2[df2 == "-9999" | is.na(df2)] <-"-"
  
  #if(any(which(df2=="-")==37)) {df2 <- df2[-c(5)]}
  #if(any(which(df2=="-")==28)) {df2 <- df2[-c(4)]}
  
  ii <- 3; r <- list()
  if(as.double(df2[1,ii]) > 200) {r <- c(r,1)}
  if (sex == "Female" & as.double(df2[2,ii]) < 50) {r <- c(r,2)}
  if (sex == "Male" & as.double(df2[2,ii]) < 40) {r <- c(r,2)}
  if(as.double(df2[3,ii]) > 100){r <- c(r,3)}
  if(as.double(df2[4,ii]) > 150){r <- c(r,4)}
  if(as.double(df2[5,ii]) > 6.5 | as.double(df2[5,ii]) < 4) {r <- c(r,5)}
  if(as.double(df2[6,ii]) > 17.2){r <- c(r,6)}
  if(as.double(df2[7,ii]) > 110 | as.double(df2[7,ii]) < 70){r <- c(r,7)}
  if(as.double(df2[8,ii]) > 5 | as.double(df2[8,ii]) < 0.3){r <- c(r,8)}
  if ((as.double(df2[9,ii]) > 3) | (as.double(df2[9,ii]) < 0.1)) {r <- c(r,9)}
  ir <- as.double(r)
  ft2 <- flextable(df2[ir,])
  
  
  ft2 <- set_header_labels(ft2, Test1 = "Test", Test2 = "Test", CR = paste0("Current Results ", enroll_date), NR = "Normal Range/\nCut-off*" )
  ft2 <- bg(ft2, bg="grey",part = "header")
  ft2 <- font(ft2,fontname = "Arial",part = "header")
  ft2 <- font(ft2,fontname = "Arial",part = "body")
  ft2 <- theme_box(ft2)
  
  len <- nrow(df2[ir,])
  ft2 <- merge_at(ft2, i = 1, j = 1:2, part = "header")
  #ft2 <- merge_at(ft2, i = 1:4, j = 1, part = "body")
  #ft2 <- merge_at(ft2, i = 5:7, j = 1, part = "body")
  ft2 <- bg(ft2, bg="grey",part = "header")
  ft2 <- fontsize(ft2, j=1, size = 9, part="body")
  ft2 <- fontsize(ft2, j=1:4, size = 10, part="header")
  ft2 <- fontsize(ft2, j=2:4, size = 10, part="body")
  ft2 <- theme_box(ft2)
  ft2 <- align(ft2, align = "center", part="header")
  ft2 <- align(ft2, align = "center", part="body")
  ft2 <- align(ft2, i=1:len, j=2, align="left",part="body")
  ft2 <- valign(ft2, i=1:len, j=3:4, valign="center", part="body")
  ft2 <- height(ft2, height = .4, part = "header")
  ft2 <- width(ft2, j = 1, width = .85)
  ft2 <- width(ft2, j = 2, width = 1.25)
  ft2 <- width(ft2, j = 3, width = 1.25)
  ft2 <- width(ft2, j = 4, width = 1.25)
  
  ft2 <- bold(ft2, j = 3, bold = TRUE, part = "body")
  
  ptp_path <<- paste0(main_path,"resources/Templates/Incidentals/TAP_blood_temp_ptp_e.docx")
  phys_path <<- paste0(main_path,"resources/Templates/Incidentals/TAP_blood_temp_phys_e.docx")
  
  ptp_temp <- paste0(out_path,"ptp_temp.docx")
  phys_temp <- paste0(out_path,"phys_temp.docx")
  
  FT<<- list(ft = ft2)
  body_add_flextables(ptp_path,ptp_temp, FT)
  body_add_flextables(phys_path,phys_temp, FT)
  
  vmac_id<<- as.character(pdb_data$vmac_id)
  if (nchar(vmac_id)==1) {record<<- paste0("0000",vmac_id)} else if (nchar(vmac_id)==2) {record<<- paste0("000",vmac_id)} else if (nchar(vmac_id)==3) {record<<- paste0("00",vmac_id)} else if (nchar(vmac_id)==4) {record<<- paste0("0",vmac_id)} else {record<<- vmac_id}
  input <- record
  output<- paste0(out_path,"TAP_",input,"_",ep,"_blood_incidental.docx")
  renderInlineCode(ptp_temp, output)
  
  importFiles(rcon = pdb, file = output, record = record, field = "feedback_incidental_stat_blood_letter", event = pdb_data[,"redcap_event_name"],
              overwrite = TRUE, repeat_instance = 1)
  
  num_phys <- as.integer(pdb_data$feedback_incidental_stat_blood_number_letter); if (is.na(num_phys)) {num_phys <- 0}
  
  if (num_phys > 0) {
    first_name_physician<<-first_name_physician1
    last_name_physician<<- last_name_physician1
    credentials<<- credentials1
    street_address_physician<<- street_address_physician1
    city_physician<<- city_physician1
    state_physician<<- state_physician1
    zip_physician<<- zip_physician1
    
    output <- paste0(out_path,"MAP_",input,"_",ep,"_blood_phys_incidental.docx")
    renderInlineCode(phys_temp, output)
    
    importFiles(rcon = pdb, file = output, record = record, field = "feedback_incidental_stat_blood_physician1_letter", event = events[e],
                overwrite = TRUE, repeat_instance = 1)
    
    if (num_phys > 1) {
      first_name_physician<<-first_name_physician2
      last_name_physician<<- last_name_physician2
      credentials<<- credentials2
      street_address_physician<<- street_address_physician2
      city_physician<<- city_physician2
      state_physician<<- state_physician2
      zip_physician<<- zip_physician2
      
      output <- paste0(out_path,"MAP_",input,"_",ep,"_blood_phys2_incidental.docx")
      renderInlineCode(phys_temp, output)
      
      importFiles(rcon = pdb, file = output, record = record, field = "feedback_incidental_stat_blood_physician2_letter", event = events[e],
                  overwrite = TRUE, repeat_instance = 1)
      
      if (num_phys > 2) {
        first_name_physician<<-first_name_physician3
        last_name_physician<<- last_name_physician3
        credentials<<- credentials3
        street_address_physician<<- street_address_physician3
        city_physician<<- city_physician3
        state_physician<<- state_physician3
        zip_physician<<- zip_physician3
        
        output <- paste0(out_path,"MAP_",input,"_",ep,"_blood_phys3_incidental.docx")
        renderInlineCode(phys_temp, output)
        
        importFiles(rcon = pdb, file = output, record = record, field = "feedback_incidental_stat_blood_physician3_letter",event = events[e],
                    overwrite = TRUE, repeat_instance = 1)
        
        if (num_phys > 3) {
          first_name_physician<<-first_name_physician4
          last_name_physician<<- last_name_physician4
          credentials<<- credentials4
          street_address_physician<<- street_address_physician4
          city_physician<<- city_physician4
          state_physician<<- state_physician4
          zip_physician<<- zip_physician4
          
          output <- paste0(out_path,"MAP_",input,"_",ep,"_blood_phys4_incidental.docx")
          renderInlineCode(phys_temp, output)
          
          importFiles(rcon = pdb, file = output, record = record, field = "feedback_incidental_stat_blood_physician4_letter",event = events[e],
                      overwrite = TRUE, repeat_instance = 1)
          
          if (is.na(first_name_physician5)==FALSE) {
            first_name_physician<<-first_name_physician5
            last_name_physician<<- last_name_physician5
            credentials<<- credentials5
            street_address_physician<<- street_address_physician5
            city_physician<<- city_physician5
            state_physician<<- state_physician5
            zip_physician<<- zip_physician5
            
            output <- paste0(out_path,"MAP_",input,"_",ep,"_blood_phys5_incidental.docx")
            renderInlineCode(phys_temp, output)
            
            importFiles(rcon = pdb, file = output, record = record, field = "feedback_incidental_stat_blood_physician5_letter",event = events[e],
                        overwrite = TRUE, repeat_instance = 1)
          }}}}}
  
  return(err)
}
