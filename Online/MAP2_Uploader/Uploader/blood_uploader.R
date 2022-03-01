blood_uploader <- function(epoch,vmac) {
  
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
  
  tm7yr <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                            token = "1769F48432D599795BAA88493EFFCF3F",conn,project = 117460)
  
  tm60 <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                           token = "54A5137E46FFC775D9BC1117DA32B30A",conn,project = 80145)
  
  tm36 <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                           token = "3CCD5E3C8A09BE5F6E940B291A9BC6B4",conn,project = 73222)
  
  tm18 <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                           token = "04661B3930D4F892AB1F194C287EE98B",conn,project = 56822)
  
  tme <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "1A5DACDD0A80C92D60FCCC27A501286E",conn,project = 39332)
  
  pdb <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "496ED1BD518B29CB96B5CFD9C48844FE", conn, project = 136242)
  
  pdb_datas <- exportReports(pdb, 267451)
  
  
  # NP Norm Scores
  dde <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "0676EDF59CA88377654227BB56028EEE", conn, project = 124389)
  dde2 <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                           token = "F009A86ED43B42332AC26C21411F37BC", conn, project = 140071)
  
  EDC <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "09B0A6B7F207F51C6F656BAE567FA390", conn, project = 119047)
  dde_datas <- exportReports(dde, 275632)
  dde2_datas <- exportReports(dde2, 277386)
  edc_datas <- exportReports(EDC, 275637)
  echo_datas <- exportReports(EDC,281651)
  cond_datas <- exportReports(EDC,284613)
  dep_datas <- exportReports(EDC,294830)
  
  events <- c("eligibility_arm_1","enrollmentbaseline_arm_1","18month_followup_arm_1","3year_followup_arm_1","5year_followup_arm_1","7year_followup_arm_1","9year_followup_arm_1","11year_followup_arm_1")
  
  map_data <- pdb_datas[which(pdb_datas$vmac_id==as.integer(vmac)),]
  rev_data_frame <- apply(map_data, 2, rev)
  map_data <- as.data.frame(rev_data_frame)
  
  pdb_data <- map_data[which(map_data[,"redcap_event_name"]== events[epochh+1]),]
  
  map_id <- pdb_data$map_id
  if (nchar(map_id)==1) {inp <- paste0("00",map_id)} else if (nchar(map_id)==2) {inp <- paste0("0",map_id)} else {inp <- map_id}
  
  fail <- 0
  dde_data <- dde2_datas[grep(inp,dde2_datas$record_id),]
  if (length(dde_data$record_id)==0) {fail <- 1}
  ij <- grep("--1",dde_data$record_id)
  if(length(ij)==0) {ij <- 1}
  
  edc_data <- edc_datas[which(edc_datas$map_id==as.integer(map_id)),]
  if (length(edc_data$map_id)==0) {fail <- 1}
  edc_data <- edc_data[which(edc_data[,"redcap_event_name"]== events[epochh+1]),]
  
  dep_data <- dep_datas[which(dep_datas$map_id==as.integer(map_id)),]
  if (length(dep_data$map_id)==0) {fail <- 1}
  dep_data <- dep_data[which(dep_data[,"redcap_event_name"]== events[epochh]),]
  
  echo_datas <- echo_datas[which(echo_datas$map_id==as.integer(map_id)),]
  echo_data <- echo_datas[which(echo_datas[,"redcap_event_name"]== events[epochh+1]),]
  
  #if (length(echo_data$map_id)==0) {fail <- 1}
  
  #cond_datas <- exportReports(EDC,284613)
  #cond_datas <- cond_datas[which(cond_datas[,"redcap_event_name"]== events[epochh+1]),]
  #cond_data <- cond_datas[which(cond_datas["map_id"]==as.integer(pdb_data$map_id)),]
  #cond_data <- cond_data[1,]
  
  err <- ""
  
  # Epoch Selector
  e <- epochh
  Epoch_conv <- c("Enrollment","18-Month","3-Year","5-Year","7-Year","9-Year","11-Year","13-Year")
  Epoc_conv <- c("enrollment","18-month","3-year","5-year","7-year","9-year","11-year","13-year")
  Epoch <<- Epoch_conv[e]; Epoc <<- Epoc_conv[e]
  Epoch2 <<- Epoch_conv[e-2]; Epoc2 <<- Epoc_conv[e-2]
  Epoch1 <<- Epoch_conv[e-1]; Epoc1 <<- Epoc_conv[e-1]
  epoch_conv <- c("18mos","36mos","60mos","7yr","9yr","11yr","13yr")
  epoch2 <<- epoch_conv[e-3]
  epoch1 <<- epoch_conv[e-2]
  epoch <<- epoch_conv[e-1]
  epoch_conv2 <- c("","3yr_","5yr_","7yr_","9yr_","11yr_","13yr_")
  ep_next <<- epoch_conv2[e]
  ep <- epoch
  date_next <<- paste0("visit_estimate_",ep_next,"date")
  date_ty <<- format(as.Date(pdb_data[, date_next]), "%B %Y")
  
  
  map_id <- as.character(pdb_data[,"map_id"])
  
  i<-1
  
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
  bld_date_time <<- format(as.Date(echo_data$bld_date_time), "%m/%d/%Y")
  
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
  
  if (e > 3) {
    if (e > 4) {
      # 7 Year Data
      tm7yr_datas <- exportReports(tm7yr, 248514)
      
      ind <- as.integer(map_id)
      inddd <- c()
      if (exists("indd")==TRUE) remove("indd")
      try(indd <- find.matches(tm7yr_datas[, "map_id"],ind),silent = TRUE)
      if (exists("indd")==FALSE) stop("Not Enough Data")
      for (i in (1:length(indd$matches))){if (indd$matches[i]>0){inddd<-c(inddd,i)}}
      tm7yr_datas[inddd,which(is.na(tm7yr_datas[inddd,1:17]))]<- "Missing"
      tm7yr_data <- tm7yr_datas[inddd,]
      #if (nrow(tm7yr_data)==FALSE) {stop("Not Enough Data")}
      if (nrow(tm7yr_data)==FALSE) {tm7yr_data <- c(rep(NA,38))}
    }
    
    if (e > 3) {
      # 5 Year Data 
      tm60_datas <- exportReports(tm60, 248512)
      
      inddd <- c()
      if (exists("indd")==TRUE) remove("indd")
      try(indd <- find.matches(tm60_datas[, "map_id"],ind),silent = TRUE)
      if (exists("indd")==FALSE) stop("Not Enough Data")
      for (i in (1:length(indd$matches))){if (indd$matches[i]>0){inddd<-c(inddd,i)}}
      tm60_datas[inddd,which(is.na(tm60_datas[inddd,]))]<- "Missing"
      tm60_data <- tm60_datas[inddd,]
      #if (nrow(tm60_data)==FALSE) {stop("Not Enough Data")}
    }
    
    if (e > 2) {
      #3 Year Data
      tm36_datas <- exportReports(tm36, 248500)
      
      inddd <- c()
      if (exists("indd")==TRUE) remove("indd")
      try(indd <- find.matches(tm36_datas[, "map_id"],ind),silent = TRUE)
      if (exists("indd")==FALSE) stop("Not Enough Data")
      for (i in (1:length(indd$matches))){if (indd$matches[i]>0){inddd<-c(inddd,i)}}
      tm36_datas[inddd,which(is.na(tm36_datas[inddd,]))]<- "Missing"
      tm36_data <- tm36_datas[inddd,]
      #if (nrow(tm36_data)==FALSE) {stop("Not Enough Data")}
    }
    
    # Enrollment Data
    tme_datas <- exportReports(tme, 248431)
    inddd <- c()
    if (exists("indd")==TRUE) remove("indd")
    try(indd <- find.matches(tme_datas[, "map_id"],ind),silent = TRUE)
    if (exists("indd")==FALSE) stop("Not Enough Data")
    for (i in (1:length(indd$matches))){if (indd$matches[i]>0){inddd<-c(inddd,i)}}
    tme_datas[inddd,which(is.na(tme_datas[inddd,]))]<- "Missing"
    tme_data <- tme_datas[inddd,]
    if (nrow(tme_data)==FALSE) {stop("Not Enough Data")}
    
    # Follow up dates
    enroll_date <<- format(as.Date(echo_datas[1, "consent_date"]), "%m/%d/%Y")
    if (is.na(enroll_date)) {enroll_date <<- format(as.Date(map_data[1, "visit1_date"]), "%m/%d/%Y")}
    fu_date_prev2 <<- format(as.Date(echo_datas[2, "consent_date"]), "%m/%d/%Y")
    if (is.na(fu_date_prev2)) {fu_date_prev2 <<- format(as.Date(map_data[5, "visit1_date"]), "%m/%d/%Y")}
    fu_date_prev <<- format(as.Date(echo_datas[3, "consent_date"]), "%m/%d/%Y")
    if (is.na(fu_date_prev)) {fu_date_prev <<- format(as.Date(map_data[4, "visit1_date"]), "%m/%d/%Y")}
    fu_date_c <<- format(as.Date(echo_data$consent_date), "%m/%d/%Y")
    if (is.na(fu_date_c)) {fu_date_c <<- format(as.Date(map_data[3, "visit1_date"]), "%m/%d/%Y")}
    
    print("Creating Data Tables")
    
    i <- 1
    
    df2 <- data.frame(
      Test1 = c("Cholesterol", "Cholesterol", "Cholesterol", "Cholesterol", "Blood Sugar", "Blood Sugar", "Blood Sugar", "Thyroid", "Inflammation"),
      Test2 = c("Total","HDL", "LDL", "Triglycerides", "Hemoglobin A1C", "Fasting Insulin", "Fasting Glucose", "Thyroid Stimulating Hormone (TSH)", "High Sensitivity C-Reactive Protein"),
      ER = c(tme_data[i, 5], tme_data[i, 6], tme_data[i,7], tme_data[i,8], tme_data[i,9], tme_data[i,10], tme_data[i,11], tme_data[i,12], tme_data[i,13]),
      MR_36 = c(tm36_data[i, 5], tm36_data[i, 6], tm36_data[i,7], tm36_data[i,8], tm36_data[i,9], tm36_data[i,10], tm36_data[i,11], tm36_data[i,12], tm36_data[i,13]),
      MR_60 = c(tm60_data[i, 5], tm60_data[i, 6], tm60_data[i,7], tm60_data[i,8], tm60_data[i,9], tm60_data[i,10], tm60_data[i,11], tm60_data[i,12], tm60_data[i,13]),
      CR = as.character(c(echo_data$bld_c_chol, echo_data$bld_c_hdlc, echo_data$bld_c_ldlc, echo_data$bld_c_trig, echo_data$bld_c_hgba1c, echo_data$bld_c_insulin, echo_data$bld_c_glucose, echo_data$bld_c_tsh, echo_data$bld_c_crp)),  
      NR = c("<200", "men >40, women >50", "<100", "<150", "<5.7", "<17", "70-99", "0.35-3.6", "0-2.9")
    )
    
    
    df2[df2 == "-9999" | is.na(df2)] <-"-"
    
    if(any(which(df2=="-")==37)) {df2 <- df2[-c(5)]}
    if(any(which(df2=="-")==28)) {df2 <- df2[-c(4)]}
    
    ii <- 6; r <- list()
    if(as.integer(df2[1,ii]) > 200) {r <- c(r,1)}
    if (sex == "Female" & as.integer(df2[2,ii]) < 50) {r <- c(r,2)}
    if (sex == "Male" & as.integer(df2[2,ii]) < 40) {r <- c(r,2)}
    if(as.integer(df2[3,ii]) > 100){r <- c(r,3)}
    if(as.integer(df2[4,ii]) > 150){r <- c(r,4)}
    if(as.integer(df2[5,ii]) > 5.7){r <- c(r,5)}
    if(as.integer(df2[6,ii]) > 17){r <- c(r,6)}
    if(as.integer(df2[7,ii]) > 99 | as.integer(df2[7,ii]) < 70){r <- c(r,7)}
    if(as.integer(df2[8,ii]) > 3.6 | as.integer(df2[8,ii]) < 0.35){r <- c(r,8)}
    if ((as.double(df2[9,ii]) > 2.9) | (as.double(df2[9,ii]) < 0)) {r <- c(r,9)}
    ir <- as.double(r)
    ft2 <- flextable(df2[ir,])
    
    
    ft2 <- set_header_labels(ft2, Test1 = "Test", Test2 = "Test", ER = paste0("Enrollment Results ",enroll_date), 
                             MR_36 = paste0(Epoch2," Results ", fu_date_prev2),
                             MR_60 = paste0(Epoch1," Results ", fu_date_prev), CR = paste0("Current Results ", fu_date_c), NR = "Normal Range/\nCut-off*" )
    ft2 <- bg(ft2, bg="grey",part = "header")
    ft2 <- font(ft2,fontname = "Arial",part = "header")
    ft2 <- font(ft2,fontname = "Arial",part = "body")
    ft2 <- theme_box(ft2)
    
    len <- nrow(df2[ir,])
    if(length(df2)==7){
      ft2 <- width(ft2, j = 1:7, width=.9)
      ft2 <- merge_at(ft2, i = 1, j = 1:2, part = "header")
      ft2 <- fontsize(ft2, j=1, size = 9, part="body")
      ft2 <- fontsize(ft2, j=1:7, size = 10, part="header")
      ft2 <- fontsize(ft2, j=2:7, size = 10, part="body")
      ft2 <- align(ft2, align = "center", part="header")
      ft2 <- align(ft2, align = "center", part="body")
      ft2 <- align(ft2, j=2, align="left",part="body")
      ft2 <- valign(ft2, j=3:7, valign="center", part="body")
      ft2 <- height(ft2, height = .4, part = "header")
      #ft2 <- width(ft2, j = 1, width = .85)
      ft2 <- width(ft2, j = 2, width = 1.25)
    } else {
      ft2 <- width(ft2, j = 1:6, width=.9)
      ft2 <- merge_at(ft2, i = 1, j = 1:2, part = "header")
      ft2 <- fontsize(ft2, j=1, size = 9, part="body")
      ft2 <- fontsize(ft2, j=1:6, size = 10, part="header")
      ft2 <- fontsize(ft2, j=2:6, size = 10, part="body")
      ft2 <- align(ft2, align = "center", part="header")
      ft2 <- align(ft2, align = "center", part="body")
      ft2 <- align(ft2, j=2, align="left",part="body")
      ft2 <- valign(ft2, j=3:6, valign="center", part="body")
      ft2 <- height(ft2, height = .4, part = "header")
      #ft2 <- width(ft2, j = 1, width = .85)
      ft2 <- width(ft2, j = 2, width = 1.25)
    }
    
    ft2 <- bold(ft2, j = 6, bold = TRUE, part = "body")
    
    ptp_path<<- paste0(main_path,"resources/Templates/Incidentals/MAP_blood_temp_ptp_7year.docx")
    phys_path<<- paste0(main_path,"resources/Templates/Incidentals/MAP_blood_temp_phys_7year.docx")
    
    ptp_temp <- paste0(out_path,"ptp_temp.docx")
    phys_temp <- paste0(out_path,"phys_temp.docx")
    
    FT<<- list(ft = ft2)
    body_add_flextables(ptp_path,ptp_temp, FT)
    body_add_flextables(phys_path,phys_temp, FT)
  }
  
  if (e == 1) {
    # Follow up dates
    enroll_date <<- format(as.Date(echo_datas[1, "consent_date"]), "%m/%d/%Y")
    if (is.na(enroll_date)) {enroll_date <<- format(as.Date(map_data[1, "visit1_date"]), "%m/%d/%Y")}
    
    df2 <- data.frame(
      Test1 = c("Cholesterol", "Cholesterol", "Cholesterol", "Cholesterol", "Blood Sugar", "Blood Sugar", "Blood Sugar", "Thyroid", "Inflammation"),
      Test2 = c("Total","HDL", "LDL", "Triglycerides", "Hemoglobin A1C", "Fasting Insulin", "Fasting Glucose", "Thyroid Stimulating Hormone (TSH)", "High Sensitivity C-Reactive Protein"),
      CR = as.character(c(echo_data$bld_c_chol, echo_data$bld_c_hdlc, echo_data$bld_c_ldlc, echo_data$bld_c_trig, echo_data$bld_c_hgba1c, echo_data$bld_c_insulin, echo_data$bld_c_glucose, echo_data$bld_c_tsh, echo_data$bld_c_crp)),  
      NR = c("<200", "men >40, women >50", "<100", "<150", "<5.7", "<17", "70-99", "0.35-3.6", "0-2.9")
    )
    
    
    df2[df2 == "-9999" | is.na(df2)] <-"-"
    
    if(any(which(df2=="-")==37)) {df2 <- df2[-c(5)]}
    if(any(which(df2=="-")==28)) {df2 <- df2[-c(4)]}
    
    ii <- 3; r <- list()
    if(as.integer(df2[1,ii]) > 200) {r <- c(r,1)}
    if (sex == "Female" & as.integer(df2[2,ii]) < 50) {r <- c(r,2)}
    if (sex == "Male" & as.integer(df2[2,ii]) < 40) {r <- c(r,2)}
    if(as.integer(df2[3,ii]) > 100){r <- c(r,3)}
    if(as.integer(df2[4,ii]) > 150){r <- c(r,4)}
    if(as.integer(df2[5,ii]) > 5.7){r <- c(r,5)}
    if(as.integer(df2[6,ii]) > 17){r <- c(r,6)}
    if(as.integer(df2[7,ii]) > 99 | as.integer(df2[7,ii]) < 70){r <- c(r,7)}
    if(as.integer(df2[8,ii]) > 3.6 | as.integer(df2[8,ii]) < 0.35){r <- c(r,8)}
    if ((as.double(df2[9,ii]) > 2.9) | (as.double(df2[9,ii]) < 0)) {r <- c(r,9)}
    ir <- as.double(r)
    ft2 <- flextable(df2[ir,])
    
    ft2 <- set_header_labels(ft2, Test1 = "Test", Test2 = "Test", CR = paste0("Current Results ", enroll_date), NR = "Normal Range/\nCut-off*" )
    ft2 <- bg(ft2, bg="grey",part = "header")
    ft2 <- font(ft2,fontname = "Arial",part = "header")
    ft2 <- font(ft2,fontname = "Arial",part = "body")
    ft2 <- theme_box(ft2)
    
    
    ft2 <- width(ft2, j = 1:4, width=.9)
    ft2 <- merge_at(ft2, i = 1, j = 1:2, part = "header")
    #ft2 <- merge_at(ft2, i = 1:4, j = 1, part = "body")
    #ft2 <- merge_at(ft2, i = 5:7, j = 1, part = "body")
    ft2 <- fontsize(ft2, j=1, size = 9, part="body")
    ft2 <- fontsize(ft2, j=1:4, size = 10, part="header")
    ft2 <- fontsize(ft2, j=2:4, size = 10, part="body")
    ft2 <- align(ft2, align = "center", part="header")
    ft2 <- align(ft2, align = "center", part="body")
    ft2 <- align(ft2, j=2, align="left",part="body")
    ft2 <- valign(ft2, j=3:4, valign="center", part="body")
    ft2 <- height(ft2, height = .4, part = "header")
    #ft2 <- width(ft2, j = 1, width = .85)
    ft2 <- width(ft2, j = 2, width = 1.25)
    
    ft2 <- bold(ft2, j = 3, bold = TRUE, part = "body")
    
    ptp_path <<- paste0(main_path,"resources/Templates/Incidentals/MAP_blood_temp_ptp_e.docx")
    phys_path <<- paste0(main_path,"resources/Templates/Incidentals/MAP_blood_temp_phys_e.docx")
    
    ptp_temp <- paste0(out_path,"ptp_temp.docx")
    phys_temp <- paste0(out_path,"phys_temp.docx")
    
    FT<<- list(ft = ft)
    body_add_flextables(ptp_path,ptp_temp, FT)
    body_add_flextables(phys_path,phys_temp, FT)
  }
  
  if (nchar(map_id)==1) {input<<- paste0("00",map_id)} else if (nchar(map_id)==2) {input<<- paste0("0",map_id)} else {input<<- map_id}
  vmac_id<<- as.character(pdb_data[i,"vmac_id"])
  if (nchar(vmac_id)==1) {record<<- paste0("0000",vmac_id)} else if (nchar(vmac_id)==2) {record<<- paste0("000",vmac_id)} else if (nchar(vmac_id)==3) {record<<- paste0("00",vmac_id)} else if (nchar(vmac_id)==4) {record<<- paste0("0",vmac_id)} else {record<<- vmac_id}
  output <- paste0(out_path,"MAP_",input,"_",ep,"_blood_incidental.docx")
  renderInlineCode(ptp_temp, output)
  
  importFiles(rcon = pdb, file = output, record = record, field = "feedback_incidental_blood_letter", event = pdb_data[,"redcap_event_name"],
              overwrite = TRUE, repeat_instance = 1)
  
  num_phys <- pdb_data$feedback_number_letter
  
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
    
    importFiles(rcon = pdb, file = output, record = record, field = "feedback_incidental_blood_physician1_letter", event = events[e+1],
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
      
      importFiles(rcon = pdb, file = output, record = record, field = "feedback_incidental_blood_physician2_letter", event = events[e+1],
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
        
        importFiles(rcon = pdb, file = output, record = record, field = "feedback_incidental_blood_physician3_letter",event = events[e+1],
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
          
          importFiles(rcon = pdb, file = output, record = record, field = "feedback_incidental_blood_physician4_letter",event = events[e+1],
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
            
            importFiles(rcon = pdb, file = output, record = record, field = "feedback_incidental_blood_physician5_letter",event = events[e+1],
                        overwrite = TRUE, repeat_instance = 1)
          }}}}}
  
  return(err)
}
