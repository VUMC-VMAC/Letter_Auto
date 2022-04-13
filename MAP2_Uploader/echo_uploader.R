echo_uploader <- function(epoch,vmac) {
  
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
  
  pdb_datas <- exportReports(pdb, 305656)
  echo_datas <- exportReports(EDC,281651)
  
  ptp_path <- paste0(main_path,"resources/Templates/Incidentals/MAP_echo_temp_ptp.docx")
  phys_path <- paste0(main_path,"resources/Templates/Incidentals/MAP_echo_temp_phys.docx")
  
  events <- c("eligibility_arm_1","enrollmentbaseline_arm_1","18month_followup_arm_1","3year_followup_arm_1","5year_followup_arm_1","7year_followup_arm_1")
  pdb_datas <- pdb_datas[which(pdb_datas[,"redcap_event_name"]== events[epoch+1]),]
  ii <- which(pdb_datas["vmac_id"]==as.integer(vmac)) #need to find i for map id
  pdb_data <- pdb_datas[ii,]
  map_id <- pdb_data$map_id
  
  echo_datas <- echo_datas[which(echo_datas$map_id==as.integer(map_id)),]
  echo_datas <- echo_datas[which(echo_datas[,"redcap_event_name"]== events[epoch+1]),]
  echo_data <- echo_datas[which(is.na(echo_datas$redcap_repeat_instrument)),]
  brain_data <- echo_datas[which(is.na(echo_datas$redcap_repeat_instrument)==FALSE),]
  
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
  
  # Participant Data
  first_name <<- pdb_data[i,"preferred_name"]
  if (is.na(first_name)) {first_name<<-pdb_data[i,"first_name"]}
  last_name <<- pdb_data[i, "last_name"]
  street_address <<- pdb_data[i, "street_address"]
  city <<- pdb_data[i, "city"]
  state <<- pdb_data[i, "state"]
  zipp <<- pdb_data[i, "zip"]
  dob <<- format(as.Date(pdb_data[i, "dob"]), "%m/%d/%Y")
  sex <<- as.character(pdb_data[i, "sex"])
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
  salutation<<- as.character(pdb_data[i, "salutation"])
  fb_date1 <- pdb_data[i, "feedback_date"]
  if (is.na(fb_date1)) {feedback_date1 <<- "UNKNOWN"} else {feedback_date1 <<- format(as.Date(fb_date1), "%m/%d/%Y")}
  feedback_location <<- as.character(pdb_data[i, "feedback_location"])
  
  
  echo_date_time <<- format(as.Date(echo_data$echo_date_time), "%m/%d/%Y")
  
  #extracardiac_incidental_describe <<- echo_data$extracardiac_incidental_describe
  echo_ptp <<- echo_data$echo_incidental_letter_participant
  echo_phys <<- echo_data$echo_incidental_letter_physican
  
  
  output <- paste0(out_path,"MAP_",input,"_",ep,"_echo_letter.docx")
  renderInlineCode(ptp_path, output)
  
  importFiles(rcon = pdb, file = output, record = record, field = "feedback_incidental_stat_letter_echo", event = pdb_data[,"redcap_event_name"],
              overwrite = TRUE, repeat_instance = 1)
  
  # Compiling Physician Data
  first_name_physician1<<- pdb_data[i, "feedback_physician1_first_name"]
  last_name_physician1<<- pdb_data[i, "feedback_physician1_last_name"]
  credentials1<<- pdb_data[i,"feedback_physician1_credentials"]
  street_address_physician1<<- pdb_data[i, "feedback_physician1_street_address"]
  city_physician1<<- pdb_data[i, "feedback_physician1_city"]
  state_physician1<<- pdb_data[i, "feedback_physician1_state"]
  zip_physician1<<- pdb_data[i, "feedback_physician1_zip"]
  first_name_physician2<<- pdb_data[i, "feedback_physician2_first_name"]
  last_name_physician2<<- pdb_data[i, "feedback_physician2_last_name"]
  credentials2<<- pdb_data[i,"feedback_physician2_credentials"]
  street_address_physician2<<- pdb_data[i, "feedback_physician2_street_address"]
  city_physician2<<- pdb_data[i, "feedback_physician2_city"]
  state_physician2<<- pdb_data[i, "feedback_physician2_state"]
  zip_physician2<<- pdb_data[i, "feedback_physician2_zip"]
  first_name_physician3<<- pdb_data[i, "feedback_physician3_first_name"]
  last_name_physician3<<- pdb_data[i, "feedback_physician3_last_name"]
  credentials3<<- pdb_data[i,"feedback_physician3_credentials"]
  street_address_physician3<<- pdb_data[i, "feedback_physician3_street_address"]
  city_physician3<<- pdb_data[i, "feedback_physician3_city"]
  state_physician3<<- pdb_data[i, "feedback_physician3_state"]
  zip_physician3<<- pdb_data[i, "feedback_physician3_zip"]
  first_name_physician4<<- pdb_data[i, "feedback_physician4_first_name"]
  last_name_physician4<<- pdb_data[i, "feedback_physician4_last_name"]
  credentials4<<- pdb_data[i,"feedback_physician4_credentials"]
  street_address_physician4<<- pdb_data[i, "feedback_physician4_street_address"]
  city_physician4<<- pdb_data[i, "feedback_physician4_city"]
  state_physician4<<- pdb_data[i, "feedback_physician4_state"]
  zip_physician4<<- pdb_data[i, "feedback_physician4_zip"]
  first_name_physician5<<- pdb_data[i, "feedback_physician5_first_name"]
  last_name_physician5<<- pdb_data[i, "feedback_physician5_last_name"]
  street_address_physician5<<- pdb_data[i, "feedback_physician5_street_address"]
  credentials5<<- pdb_data[i,"feedback_physician5_credentials"]
  city_physician5<<- pdb_data[i, "feedback_physician5_city"]
  state_physician5<<- pdb_data[i, "feedback_physician5_state"]
  zip_physician5<<- pdb_data[i, "feedback_physician5_zip"]
  
  if (is.na(first_name_physician1)) {first_name_physician1<<- ""} else {first_name_physician1<<- paste0("     1.  ",pdb_data[i, "feedback_physician1_first_name"])}
  if (is.na(last_name_physician1)) {last_name_physician1<<- ""} else {last_name_physician1<<- paste0(pdb_data[i, "feedback_physician1_last_name"])}
  if (is.na(street_address_physician1)) {street_address_physician1<<- ""} else {street_address_physician1<<- paste0("          ",pdb_data[i, "feedback_physician1_street_address"])}
  if (is.na(city_physician1)) {city_physician1<<- ""} else {city_physician1<<- paste0("          ",pdb_data[i, "feedback_physician1_city"],",")}
  state_physician1<<- pdb_data[i, "feedback_physician1_state"]
  if (is.na(state_physician1)) {state_physician1<<- ""}
  zip_physician1<<- pdb_data[i, "feedback_physician1_zip"]
  if (is.na(zip_physician1)) {zip_physician1<<- ""}
  if (is.na(first_name_physician2)) {first_name_physician2<<- ""} else {first_name_physician2<<- paste0("     2.  ",pdb_data[i, "feedback_physician2_first_name"])}
  if (is.na(last_name_physician2)) {last_name_physician2<<- ""} else {last_name_physician2<<- paste0(pdb_data[i, "feedback_physician2_last_name"])}
  if (is.na(street_address_physician2)) {street_address_physician2<<- ""} else {street_address_physician2<<- paste0("          ",pdb_data[i, "feedback_physician2_street_address"])}
  if (is.na(city_physician2)) {city_physician2<<- ""} else {city_physician2<<- paste0("          ",pdb_data[i, "feedback_physician2_city"],",")}
  state_physician2<<- pdb_data[i, "feedback_physician2_state"]
  if (is.na(state_physician2)) {state_physician2<<- ""}
  zip_physician2<<- pdb_data[i, "feedback_physician2_zip"]
  if (is.na(zip_physician2)) {zip_physician2<<- ""}
  if (is.na(first_name_physician3)) {first_name_physician3<<- ""} else {first_name_physician3<<- paste0("     3.  ",pdb_data[i, "feedback_physician3_first_name"])}
  if (is.na(last_name_physician3)) {last_name_physician3<<- ""} else {last_name_physician3<<- paste0(pdb_data[i, "feedback_physician3_last_name"])}
  if (is.na(street_address_physician3)) {street_address_physician3<<- ""} else {street_address_physician3<<- paste0("          ",pdb_data[i, "feedback_physician3_street_address"])}
  if (is.na(city_physician3)) {city_physician3<<- ""} else {city_physician3<<- paste0("          ",pdb_data[i, "feedback_physician3_city"],",")}
  state_physician3<<- pdb_data[i, "feedback_physician3_state"]
  if (is.na(state_physician3)) {state_physician3<<- ""}
  zip_physician3<<- pdb_data[i, "feedback_physician3_zip"]
  if (is.na(zip_physician3)) {zip_physician3<<- ""}
  if (is.na(first_name_physician4)) {first_name_physician4<<- ""} else {first_name_physician4<<- paste0("     3.  ",pdb_data[i, "feedback_physician4_first_name"])}
  if (is.na(last_name_physician4)) {last_name_physician4<<- ""} else {last_name_physician4<<- paste0(pdb_data[i, "feedback_physician4_last_name"])}
  if (is.na(street_address_physician4)) {street_address_physician4<<- ""} else {street_address_physician4<<- paste0("          ",pdb_data[i, "feedback_physician4_street_address"])}
  if (is.na(city_physician4)) {city_physician4<<- ""} else {city_physician4<<- paste0("          ",pdb_data[i, "feedback_physician4_city"],",")}
  state_physician4<<- pdb_data[i, "feedback_physician4_state"]
  if (is.na(state_physician4)) {state_physician4<<- ""}
  zip_physician4<<- pdb_data[i, "feedback_physician4_zip"]
  if (is.na(zip_physician4)) {zip_physician4<<- ""}
  if (is.na(first_name_physician5)) {first_name_physician5<<- ""} else {first_name_physician5<<- paste0("     3.  ",pdb_data[i, "feedback_physician5_first_name"])}
  if (is.na(last_name_physician5)) {last_name_physician5<<- ""} else {last_name_physician5<<- paste0(pdb_data[i, "feedback_physician5_last_name"])}
  if (is.na(street_address_physician5)) {street_address_physician5<<- ""} else {street_address_physician5<<- paste0("          ",pdb_data[i, "feedback_physician5_street_address"])}
  if (is.na(city_physician5)) {city_physician5<<- ""} else {city_physician5<<- paste0("          ",pdb_data[i, "feedback_physician5_city"],",")}
  state_physician5<<- pdb_data[i, "feedback_physician5_state"]
  if (is.na(state_physician5)) {state_physician5<<- ""}
  zip_physician5<<- pdb_data[i, "feedback_physician5_zip"]
  if (is.na(zip_physician5)) {zip_physician5<<- ""}
  
  num_phys <- as.integer(pdb_data$feedback_number_letter)
  
  if (num_phys > 0) {
    first_name_physician<<-first_name_physician1
    last_name_physician<<- last_name_physician1
    credentials<<- credentials1
    street_address_physician<<- street_address_physician1
    city_physician<<- city_physician1
    state_physician<<- state_physician1
    zip_physician<<- zip_physician1
    
    output <- paste0(out_path,"MAP_",input,"_",ep,"_echo_phys_incidental.docx")
    renderInlineCode(phys_path, output)
    
    importFiles(rcon = pdb, file = output, record = record, field = "feedback_incidental_stat_echo_physician1_letter", event = events[e+1],
                overwrite = TRUE, repeat_instance = 1)
    
    if (num_phys > 1) {
      first_name_physician<<-first_name_physician2
      last_name_physician<<- last_name_physician2
      credentials<<- credentials2
      street_address_physician<<- street_address_physician2
      city_physician<<- city_physician2
      state_physician<<- state_physician2
      zip_physician<<- zip_physician2
      
      output <- paste0(out_path,"MAP_",input,"_",ep,"_echo_phys2_incidental.docx")
      renderInlineCode(phys_path, output)
      
      importFiles(rcon = pdb, file = output, record = record, field = "feedback_incidental_stat_echo_physician2_letter", event = events[e+1],
                  overwrite = TRUE, repeat_instance = 1)
      
      if (num_phys > 2) {
        first_name_physician<<-first_name_physician3
        last_name_physician<<- last_name_physician3
        credentials<<- credentials3
        street_address_physician<<- street_address_physician3
        city_physician<<- city_physician3
        state_physician<<- state_physician3
        zip_physician<<- zip_physician3
        
        output <- paste0(out_path,"MAP_",input,"_",ep,"_echo_phys3_incidental.docx")
        renderInlineCode(phys_path, output)
        
        importFiles(rcon = pdb, file = output, record = record, field = "feedback_incidental_stat_echo_physician3_letter",event = events[e+1],
                    overwrite = TRUE, repeat_instance = 1)
        
        if (num_phys > 3) {
          first_name_physician<<-first_name_physician4
          last_name_physician<<- last_name_physician4
          credentials<<- credentials4
          street_address_physician<<- street_address_physician4
          city_physician<<- city_physician4
          state_physician<<- state_physician4
          zip_physician<<- zip_physician4
          
          output <- paste0(out_path,"MAP_",input,"_",ep,"_echo_phys4_incidental.docx")
          renderInlineCode(phys_path, output)
          
          importFiles(rcon = pdb, file = output, record = record, field = "feedback_incidental_stat_echo_physician4_letter",event = events[e+1],
                      overwrite = TRUE, repeat_instance = 1)
          
          if (is.na(first_name_physician5)==FALSE) {
            first_name_physician<<-first_name_physician5
            last_name_physician<<- last_name_physician5
            credentials<<- credentials5
            street_address_physician<<- street_address_physician5
            city_physician<<- city_physician5
            state_physician<<- state_physician5
            zip_physician<<- zip_physician5
            
            output <- paste0(out_path,"MAP_",input,"_",ep,"_echo_phys5_incidental.docx")
            renderInlineCode(phys_path, output)
            
            importFiles(rcon = pdb, file = output, record = record, field = "feedback_incidental_stat_echo_physician5_letter",event = events[e+1],
                        overwrite = TRUE, repeat_instance = 1)
          }}}}}
  
  
  return(err)
}
