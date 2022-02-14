fb_uploader<<- function(epochh,vmac) {
  
  library(ggplot2)
  library(plotly)
  library(redcapAPI)
  library(WordR)
  library(officer)
  library(Hmisc)
  library(tidyverse)
  library(flextable)
  #library(ggpattern)
  #library(patternplot)
  library(png)
  library(readxl)
  
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
  
  ex_path <- paste0(main_path,"epoch5dde_lookup.xlsx")
  
  FII <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "489C53D4DAAE99F87EF37A9D77563BB0",conn,project = 42471)
  
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
  
  if (length(echo_data$map_id)==0) {fail <- 1}
  
  cond_datas <- exportReports(EDC,284613)
  cond_datas <- cond_datas[which(cond_datas[,"redcap_event_name"]== events[epochh+1]),]
  cond_data <- cond_datas[which(cond_datas["map_id"]==as.integer(pdb_data$map_id)),]
  cond_data <- cond_data[1,]
  
  bc <- cond_data$blood_complete; if (is.na(bc)) {bc <- "No"}
  ec <- cond_data$echo_complete; if (is.na(ec)) {ec <- "No"}
  nc <- cond_data$np_complete; if (is.na(nc)) {nc <- "No"}
  
  #if (length(visit_sched)==0) {vs <- "No"} else {vs <- visit_sched}
  
  if (bc == "No" & ec =="No" & nc == "No" | fail) {err <<- "Echo and/or Blood Work not complete; insufficient data"} else {
    err <<- ""
    
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
        fii7yrs <- exportReports(FII, 252309)
        fii7yrs[which(is.na(fii7yrs["brain_incidental"])),"brain_incidental"]<- "No"
        
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
        
        inddd <- c()
        if (exists("indd")==TRUE) remove("indd")
        try(indd <- find.matches(fii7yrs[, "map_id"],ind),silent = TRUE)
        if (exists("indd")==FALSE) stop("Not Enough Data")
        for (i in (1:length(indd$matches))){if (indd$matches[i]>0){inddd<-c(inddd,i)}}
        #fii7yrs[inddd,which(is.na(fii7yrs[inddd,]))] <- "Missing"
        fii7yr <- fii7yrs[inddd,]
        fii7yr[which(is.na(fii7yr))] <- "Missing"
        #if (nrow(fii7yr)==FALSE) {stop("Not Enough Data")}
      }
      
      if (e > 3) {
        # 5 Year Data 
        
        tm60_datas <- exportReports(tm60, 248512)
        fii60s <- exportReports(FII, 252308)
        fii60s[which(is.na(fii60s["extracardiac_incidental"])),"extracardiac_incidental"]<- "No"
        fii60s[which(is.na(fii60s["brain_incidental"])),"brain_incidental"]<- "No"
        
        inddd <- c()
        if (exists("indd")==TRUE) remove("indd")
        try(indd <- find.matches(tm60_datas[, "map_id"],ind),silent = TRUE)
        if (exists("indd")==FALSE) stop("Not Enough Data")
        for (i in (1:length(indd$matches))){if (indd$matches[i]>0){inddd<-c(inddd,i)}}
        tm60_datas[inddd,which(is.na(tm60_datas[inddd,]))]<- "Missing"
        tm60_data <- tm60_datas[inddd,]
        #if (nrow(tm60_data)==FALSE) {stop("Not Enough Data")}
        
        inddd <- c()
        if (exists("indd")==TRUE) remove("indd")
        try(indd <- find.matches(fii60s[, "map_id"],ind),silent = TRUE)
        if (exists("indd")==FALSE) stop("Not Enough Data")
        for (i in (1:length(indd$matches))){if (indd$matches[i]>0){inddd<-c(inddd,i)}}
        fii60s[inddd,which(is.na(fii60s[inddd,]))]<- "Missing"
        fii60 <- fii60s[inddd,]
        #if (nrow(fii60)==FALSE) {stop("Not Enough Data")}
      }
      
      if (e > 2) {
        #3 Year Data
        
        tm36_datas <- exportReports(tm36, 248500)
        fii36s <- exportReports(FII, 252306)
        fii36s[which(is.na(fii36s["extracardiac_incidental"])),"extracardiac_incidental"]<- "No"
        fii36s[which(is.na(fii36s["brain_incidental"])),"brain_incidental"]<- "No"
        
        inddd <- c()
        if (exists("indd")==TRUE) remove("indd")
        try(indd <- find.matches(tm36_datas[, "map_id"],ind),silent = TRUE)
        if (exists("indd")==FALSE) stop("Not Enough Data")
        for (i in (1:length(indd$matches))){if (indd$matches[i]>0){inddd<-c(inddd,i)}}
        tm36_datas[inddd,which(is.na(tm36_datas[inddd,]))]<- "Missing"
        tm36_data <- tm36_datas[inddd,]
        #if (nrow(tm36_data)==FALSE) {stop("Not Enough Data")}
        
        inddd <- c()
        if (exists("indd")==TRUE) remove("indd")
        try(indd <- find.matches(fii36s[, "map_id"],ind),silent = TRUE)
        if (exists("indd")==FALSE) stop("Not Enough Data")
        for (i in (1:length(indd$matches))){if (indd$matches[i]>0){inddd<-c(inddd,i)}}
        fii36s[inddd,which(is.na(fii36s[inddd,]))]<- "Missing"
        fii36 <- fii36s[inddd,]
        #if (nrow(fii36)==FALSE) {stop("Not Enough Data")}
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
      enroll_date <<- format(as.Date(map_data[1, "visit1_date"]), "%m/%d/%Y")
      fu_date_prev2 <<- format(as.Date(map_data[5, "visit1_date"]), "%m/%d/%Y")
      fu_date_prev <<- format(as.Date(map_data[4, "visit1_date"]), "%m/%d/%Y")
      fu_date_c <<- format(as.Date(echo_data$consent_date), "%m/%d/%Y")
      if (is.na(fu_date_c)) {fu_date_c <<- format(as.Date(map_data[3, "visit1_date"]), "%m/%d/%Y")}
      
      print("Creating Data Tables")
      
      i <- 1
      
      df <- data.frame(
        Test = c("Heart rate", "Blood pressure", "Height", "Weight", "Body Mass Index"),
        Test.1 = c("Heart rate", "Blood pressure", "Height", "Weight", "Body Mass Index"),
        ER = c(tme_data[i, 2], tme_data[i, 3], paste0(round(tme_data[i, "height"]*0.393701)," inches"), paste0(round(tme_data[i, "weight"]*2.205)," lbs"), round(((tme_data[i, "weight"])/((tme_data[i, "height"])/100)^2), digits=1)),
        ER = c(tme_data[i, 2], tme_data[i, 4], paste0(round(tme_data[i, "height"]*0.393701)," inches"), paste0(round(tme_data[i, "weight"]*2.205)," lbs"), round(((tme_data[i, "weight"])/((tme_data[i, "height"])/100)^2), digits=1)),
        MR_36 = c(tm36_data[i, 2], fii36[i, 4], paste0(round(tm36_data[i, "height"]*0.393701), " inches"), paste0(round(tm36_data[i, "weight"]*2.205), " lbs"), round((tm36_data[i, "weight"]/(tm36_data[i, "height"]/100)^2), digits=1)),
        MR_36 = c(tm36_data[i, 2], fii36[i, 5], paste0(round(tm36_data[i, "height"]*0.393701), " inches"), paste0(round(tm36_data[i, "weight"]*2.205), " lbs"), round((tm36_data[i, "weight"]/(tm36_data[i, "height"]/100)^2), digits=1)),
        MR_60 = c(tm60_data[i, 2], fii60[i, 4], paste0(round(as.integer(tm60_data[i, "height"])*0.393701), " inches"), paste0(round(as.integer(tm60_data[i, "weight"])*2.205), " lbs"), round((as.integer(tm60_data[i, "weight"])/(as.integer(tm60_data[i, "height"])/100)^2), digits=1)),
        MR_60 = c(tm60_data[i, 2], fii60[i, 5], paste0(round(as.integer(tm60_data[i, "height"])*0.393701), " inches"), paste0(round(as.integer(tm60_data[i, "weight"])*2.205), " lbs"), round((as.integer(tm60_data[i, "weight"])/(as.integer(tm60_data[i, "height"])/100)^2), digits=1)),
        CR = c(echo_data$echo_hrate, echo_data$echo_read_sbp, paste0(round(echo_data$height*0.393701)," inches"), paste0(round(echo_data$weight*2.205)," lbs"), round((echo_data$weight/(echo_data$height/100)^2), digits = 1)),
        CR = c(echo_data$echo_hrate, echo_data$echo_read_dbp, paste0(round(echo_data$height*0.393701)," inches"), paste0(round(echo_data$weight*2.205)," lbs"), round((echo_data$weight/(echo_data$height/100)^2), digits = 1)),
        NR=c("60-100", "<120 / <80", "-", "-", "18.5-24.9")
      )
      
      df[df == "-9999" | df == "-3937 inches" | df == "-22048 lbs" | df == "-1" | df == "Missing" | is.na(df)] <-"-"
      
      
      if(any(which(df=="-")==31)) {df <- df[-c(7,8)]}
      if(any(which(df=="-")==21)) {df <- df[-c(5,6)]}
      
      ft <- flextable(df)
      
      ft <- set_header_labels(ft, Test = "Test", Test.1="Test",
                              ER = paste0("Enrollment Results ",enroll_date),ER.1 = paste0("Enrollment Results ",enroll_date), 
                              MR_36 = paste0(Epoch2," Results ", fu_date_prev2),MR_36.1 = paste0(Epoch2," Results ", fu_date_prev2),
                              MR_60 = paste0(Epoch1," Results ", fu_date_prev),MR_60.1 = paste0(Epoch1," Results ", fu_date_prev), 
                              CR = paste0("Current Results ", fu_date_c),CR.1 = paste0("Current Results ", fu_date_c),
                              NR = "Normal Range*" )
      ft <- bg(ft, bg="grey",part = "header")
      ft <- font(ft,fontname = "Arial",part = "header")
      ft <- font(ft,fontname = "Arial",part = "body")
      ft <- align(ft, align = "center", part="header")
      ft <- align(ft, align = "center", part="body")
      ft <- theme_box(ft)
      
      
      if (length(df)==11) {
        ft <- fontsize(ft, j=1:11, size = 10, part="header")
        ft <- fontsize(ft, j=1:11, size = 10, part="body")
        ft <- width(ft,j = 11, width = 1)
        
        ft <- merge_h(ft,part = "header")
        ft <- merge_h_range(ft,i = 1:5,j1=1, j2=2 ,part = "body")
        ft <- merge_h_range(ft,i = 1,j1=3, j2=4 ,part = "body")
        ft <- merge_h_range(ft,i = 1,j1=5, j2=6 ,part = "body")
        ft <- merge_h_range(ft,i = 1,j1=7, j2=8 ,part = "body")
        ft <- merge_h_range(ft,i = 1,j1=9, j2=10 ,part = "body")
        ft <- merge_h_range(ft,i = 3:5,j1=3, j2=4 ,part = "body")
        ft <- merge_h_range(ft,i = 3:5,j1=5, j2=6 ,part = "body")
        ft <- merge_h_range(ft,i = 3:5,j1=7, j2=8 ,part = "body")
        ft <- merge_h_range(ft,i = 3:5,j1=9, j2=10 ,part = "body")
        ft <- width(ft,j = 3:10, width = .5)
        ft <- width(ft, j=1:2, width = .75)
        ft <- align(ft, i = 1:5, j = 1:2, align="left",part="body")
        evens<-c(4,6,8,10)
      } else {
        ft <- fontsize(ft, j=1:9, size = 10, part="header")
        ft <- fontsize(ft, j=1:9, size = 10, part="body")
        ft <- width(ft,j = 9, width = 1)
        
        ft <- merge_h(ft,part = "header")
        ft <- merge_h_range(ft,i = 1:5,j1=1, j2=2 ,part = "body")
        ft <- merge_h_range(ft,i = 1,j1=3, j2=4 ,part = "body")
        ft <- merge_h_range(ft,i = 1,j1=5, j2=6 ,part = "body")
        ft <- merge_h_range(ft,i = 1,j1=7, j2=8 ,part = "body")
        ft <- merge_h_range(ft,i = 3:5,j1=3, j2=4 ,part = "body")
        ft <- merge_h_range(ft,i = 3:5,j1=5, j2=6 ,part = "body")
        ft <- merge_h_range(ft,i = 3:5,j1=7, j2=8 ,part = "body")
        ft <- width(ft,j = 3:8, width = .5)
        ft <- width(ft, j=1:2, width = .75)
        ft <- align(ft, i = 1:5, j = 1:2, align="left",part="body")
        evens<-c(4,6,8)
      }
      
      
      
      for (ii in 3:(ncol(df)-1)) {
        #Heart rate
        if (df[1,ii]=="-"){
          #print(paste0(ii,":",df[1,ii]))
          ft <- bold(ft, i = 1, j = ii, bold = FALSE, part = "body")}
        else {
          if ((as.integer(df[1,ii]) < 60) | (as.integer(df[1,ii]) >100)){
            ft <- bold(ft, i = 1, j = ii, bold = TRUE, part = "body")}
        }
        #Blood pressure - systolic
        if (df[2,ii]=="-"){
          print(paste0(ii,":",df[2,ii]))
          ft <- bold(ft, i = 2, j = ii, bold = FALSE, part = "body")}
        else {
          if (as.integer(df[2,ii]) > 120){
            ft <- bold(ft, i = 2, j = ii, bold = TRUE, part = "body")}
        }
        
        #BMI
        if (df[5,ii]=="-"){
          print(paste0(ii,":",df[5,ii]))
          ft <- bold(ft, i = 5, j = ii, bold = FALSE, part = "body")}
        else {
          if ((as.double(df[5,ii]) < 18.5 | as.double(df[5,ii]) > 24.9)){
            ft <- bold(ft, i = 5, j = ii, bold = TRUE, part = "body")}
        }
      }
      #Blood pressure -- diastolic
      for (ii in evens) {
        #Blood pressure
        if (df[2,ii]=="-"){
          #print(paste0(ii,":",df[2,ii]))
          ft <- bold(ft, i = 2, j = ii, bold = FALSE, part = "body")}
        else {
          if ((as.integer(df[2,ii]) > 80)){
            ft <- bold(ft, i = 2, j = ii, bold = TRUE, part = "body")}
        }
      }
      
      
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
      
      ft2 <- flextable(df2)
      ft2 <- set_header_labels(ft2, Test1 = "Test", Test2 = "Test", ER = paste0("Enrollment Results ",enroll_date), 
                               MR_36 = paste0(Epoch2," Results ", fu_date_prev2),
                               MR_60 = paste0(Epoch1," Results ", fu_date_prev), CR = paste0("Current Results ", fu_date_c), NR = "Normal Range/\nCut-off*" )
      ft2 <- bg(ft2, bg="grey",part = "header")
      ft2 <- font(ft2,fontname = "Arial",part = "header")
      ft2 <- font(ft2,fontname = "Arial",part = "body")
      ft2 <- theme_box(ft2)
      
      
      if(length(df2)==7){
        ft2 <- width(ft2, j = 1:7, width=.9)
        ft2 <- merge_at(ft2, i = 1, j = 1:2, part = "header")
        ft2 <- merge_at(ft2, i = 1:4, j = 1, part = "body")
        ft2 <- merge_at(ft2, i = 5:7, j = 1, part = "body")
        ft2 <- fontsize(ft2, j=1, size = 9, part="body")
        ft2 <- fontsize(ft2, j=1:7, size = 10, part="header")
        ft2 <- fontsize(ft2, j=2:7, size = 10, part="body")
        ft2 <- align(ft2, align = "center", part="header")
        ft2 <- align(ft2, align = "center", part="body")
        ft2 <- align(ft2, i=1:9, j=2, align="left",part="body")
        ft2 <- valign(ft2, i=1:9, j=3:7, valign="center", part="body")
        ft2 <- height(ft2, height = .4, part = "header")
        #ft2 <- width(ft2, j = 1, width = .85)
        ft2 <- width(ft2, j = 2, width = 1.25)
      } else {
        ft2 <- width(ft2, j = 1:6, width=.9)
        ft2 <- merge_at(ft2, i = 1, j = 1:2, part = "header")
        ft2 <- merge_at(ft2, i = 1:4, j = 1, part = "body")
        ft2 <- merge_at(ft2, i = 5:7, j = 1, part = "body")
        ft2 <- fontsize(ft2, j=1, size = 9, part="body")
        ft2 <- fontsize(ft2, j=1:6, size = 10, part="header")
        ft2 <- fontsize(ft2, j=2:6, size = 10, part="body")
        ft2 <- align(ft2, align = "center", part="header")
        ft2 <- align(ft2, align = "center", part="body")
        ft2 <- align(ft2, i=1:9, j=2, align="left",part="body")
        ft2 <- valign(ft2, i=1:9, j=3:6, valign="center", part="body")
        ft2 <- height(ft2, height = .4, part = "header")
        #ft2 <- width(ft2, j = 1, width = .85)
        ft2 <- width(ft2, j = 2, width = 1.25)
      }
      
      for (ii in 3:(ncol(df2)-1)) {
        if (df2[1,ii]=="-"){ft2<-bold(ft2, i = 1, j = ii, bold = FALSE, part = "body")}else{if(as.integer(df2[1,ii]) > 200){ft2<-bold(ft2, i = 1, j = ii, bold = TRUE, part = "body")}}
        if (df2[2,ii]=="-"){
          ft2<-bold(ft2, i = 2, j = ii, bold = FALSE, part = "body")}
        else{
          if (sex == "Female" & as.integer(df2[2,ii]) < 50){ft2 <- bold(ft2, i = 2, j = ii, bold = TRUE, part = "body")}
          if (sex == "Male" & as.integer(df2[2,ii]) < 40){ft2 <- bold(ft2, i = 2, j = ii, bold = TRUE, part = "body")}
        }
        
        if (df2[3,ii]=="-"){ft2<-bold(ft2, i = 3, j = ii, bold = FALSE, part = "body")}else{if(as.integer(df2[3,ii]) > 100){ft2<-bold(ft2, i = 3, j = ii, bold = TRUE, part = "body")}}
        if (df2[4,ii]=="-"){ft2<-bold(ft2, i = 4, j = ii, bold = FALSE, part = "body")}else{if(as.integer(df2[4,ii]) > 150){ft2<-bold(ft2, i = 4, j = ii, bold = TRUE, part = "body")}}
        if (df2[5,ii]=="-"){ft2<-bold(ft2, i = 5, j = ii, bold = FALSE, part = "body")}else{if(as.integer(df2[5,ii]) > 5.7){ft2<-bold(ft2, i = 5, j = ii, bold = TRUE, part = "body")}}
        if(class(df2[6,ii])=="character") {df2[6,ii] <- NA} else {
          if (is.na(as.double(df2[6,ii]))) {ft2 <- bold(ft2, i = 6, j = ii, bold = TRUE, part = "body")} else {
            if (df2[6,ii]=="-"){ft2<-bold(ft2, i = 6, j = ii, bold = FALSE, part = "body")}else{if(as.integer(df2[6,ii]) > 17){ft2<-bold(ft2, i = 6, j = ii, bold = TRUE, part = "body")}}
          }
        }      
        if (df2[7,ii]=="-"){ft2<-bold(ft2, i = 7, j = ii, bold = FALSE, part = "body")}else{if(as.integer(df2[7,ii]) > 99 | as.integer(df2[7,ii]) < 70){ft2<-bold(ft2, i = 7, j = ii, bold = TRUE, part = "body")}}
        if (df2[8,ii]=="-"){ft2<-bold(ft2, i = 8, j = ii, bold = FALSE, part = "body")}else{if(as.integer(df2[8,ii]) > 3.6 | as.integer(df2[8,ii]) < 0.35){ft2<-bold(ft2, i = 8, j = ii, bold = TRUE, part = "body")}}
        
        if(class(df2[9,ii])=="character") {df2[9,ii] <- NA} else {
          if (is.na(as.double(df2[9,ii]))) {ft2 <- bold(ft2, i = 9, j = ii, bold = TRUE, part = "body")} else {
            if ((as.double(df2[9,ii]) > 2.9) | (as.double(df2[9,ii]) < 0)) {ft2 <- bold(ft2, i = 9, j = ii, bold = TRUE, part = "body")}}
        }
      }
      
      
   
      print("Compiling Memory results")
      
      decline <<- ""
      decline_phys <<- ""
      
      missing <- data.frame(Missing=c("Missing","Data"),Data=c(1,2))
      remove <- missing[,1]
      chart <- missing[,2]
      
      if (nc == "No") {err <<- "Some NP item is missing; Letter generated with Memory Chart; Remove temporary chart that was added."; barr <- ggplot(missing) + geom_bar(stat="identity",position="dodge",aes(x=chart,y=remove))} else {
        
        
        
        np <- rbind(tme_data[-(1:17)],tm36_data[-(1:17)],tm60_data[-(1:17)],tm7yr_data[-(1:17)][-19])
        
        nnp <- list()
        
        nnp$np_cvlt1to5_tscore <- np$np_cvlt1to5_tscore
        nnp$np_cvlt_sdfr_z <- np$np_cvlt_sdfr_zscore
        nnp$np_cvlt_ldfr_z <- np$np_cvlt_ldfr_zscore  
        nnp$np_cvltrecog_discrim_z <- np$np_cvltrecog_discrim_zscore
        nnp$np_biber_t1to5_z <- np$np_biber_t1to5_zscore
        nnp$np_biber_sd_z <- np$np_biber_sd_zscore
        nnp$np_biber_ld_z <- np$np_biber_ld_zscore
        nnp$np_anim_tscore <- np$np_anim_tscore
        nnp$np_bnt_z <- np$np_bnt_zscore
        nnp$np_tower_ss <- np$np_tower_sscore
        nnp$np_inhibit_ss <- np$np_inhibit_sscore
        nnp$np_fas_tscore <- np$np_fas_tscore
        nnp$np_tmtb_ss <- np$np_tmtb_sscore
        nnp$np_hvot_tscore <- np$np_hvot_tscore
        nnp$np_digsymb_ss <- np$np_digsymb_sscore
        nnp$np_color_ss <- np$np_color_sscore
        nnp$np_word_ss <- np$np_word_sscore
        nnp$np_tmta_ss <- np$np_tmta_sscore
        
        # transform variables
        nnp$np_cvlt1to5_z <- (nnp$np_cvlt1to5_tscore - 50)/10
        nnp$np_anim_z <- (nnp$np_anim_tscore-50)/10
        nnp$np_tower_z <- (nnp$np_tower_ss-10)/3
        nnp$np_inhibit_z <- (nnp$np_inhibit_ss-10)/3
        nnp$np_fas_z <- (nnp$np_fas_tscore-50)/10
        nnp$np_tmtb_z <- (nnp$np_tmtb_ss-10)/3
        nnp$np_hvot_z <- -(nnp$np_hvot_tscore-50)/10
        nnp$np_digsymb_z <- (nnp$np_digsymb_ss-10)/3
        nnp$np_color_z <- (nnp$np_color_ss-10)/3
        nnp$np_word_z <- (nnp$np_word_ss-10)/3
        nnp$np_tmta_z <- (nnp$np_tmta_ss-10)/3
        
        np_cvlt1to5_tscore <- np$np_cvlt1to5_tscore
        np_cvlt_sdfr_z <- np$np_cvlt_sdfr_zscore
        np_cvlt_ldfr_z <- np$np_cvlt_ldfr_zscore  
        np_cvltrecog_discrim_z <- np$np_cvltrecog_discrim_zscore
        np_biber_t1to5_z <- np$np_biber_t1to5_zscore
        np_biber_sd_z <- np$np_biber_sd_zscore
        np_biber_ld_z <- np$np_biber_ld_zscore
        np_anim_tscore <- np$np_anim_tscore
        np_bnt_z <- np$np_bnt_zscore
        np_tower_ss <- np$np_tower_sscore
        np_inhibit_ss <- np$np_inhibit_sscore
        np_fas_tscore <- np$np_fas_tscore
        np_tmtb_ss <- np$np_tmtb_sscore
        np_hvot_tscore <- np$np_hvot_tscore
        np_digsymb_ss <- np$np_digsymb_sscore
        np_color_ss <- np$np_color_sscore
        np_word_ss <- np$np_word_sscore
        np_tmta_ss <- np$np_tmta_sscore
        
        # transform variables
        np_cvlt1to5_z <- (nnp$np_cvlt1to5_tscore - 50)/10
        np_anim_z <- (nnp$np_anim_tscore-50)/10
        np_tower_z <- (nnp$np_tower_ss-10)/3
        np_inhibit_z <- (nnp$np_inhibit_ss-10)/3
        np_fas_z <- (nnp$np_fas_tscore-50)/10
        np_tmtb_z <- (nnp$np_tmtb_ss-10)/3
        np_hvot_z <- -(nnp$np_hvot_tscore-50)/10
        np_digsymb_z <- (nnp$np_digsymb_ss-10)/3
        np_color_z <- (nnp$np_color_ss-10)/3
        np_word_z <- (nnp$np_word_ss-10)/3
        np_tmta_z <- (nnp$np_tmta_ss-10)/3
        
        ggfp <- data.frame(nnp)
        
        if (any(is.na(ggfp))) {
        
        #### If Norm Scores are not available ####
          incomplete <- any(dde_data[ij,]==-7777); if (is.na(incomplete)) {incomplete <- 0}
          dnf <- any(dde_data[ij,]==-9999); if (is.na(dnf)) {dnf <- 0}
          miss <- any(is.na(dde_data[ij,3:28])); if(is.na(miss)) {miss <- 0}
          if (incomplete | dnf | miss) {err <- "Some NP data is missing from the DDE; Letter generated without Memory Chart; Remove temporary chart that was added."; barr <- ggplot(missing) + geom_bar(stat="identity",position="dodge",aes(x=chart,y=remove))} else {
            
            age <- pdb_data$age
            edu <- pdb_data$education
            race <- pdb_data$np_norm_race
            
            if (age < 55) {age_r <- "50-54"}; if (age < 60 & age >= 55) {age_r <- "55-59"}; if (age < 65 & age >= 60) {age_r <- "60-64"}
            if (age < 70 & age >= 65) {age_r <- "65-69"}; if (age < 75 & age >= 70) {age_r <- "70-74"} 
            if (age < 80 & age >= 75) {age_r <- "75-79"}; if (age >= 80) {age_r <- "80-85"}
            
            if (edu < 9) {edu_r <- "7-8"}; if (edu >= 9 & edu < 12 ) {edu_r <- "9-11"}; if (edu == 12 ) {edu_r <- "12"}
            if (edu >= 13 & edu < 16 ) {edu_r <- "13-15"}; if (edu >= 16 & edu < 18 ) {edu_r <- "16-17"}
            if (edu >= 18) {edu_r <- "18-20"}
            
            sex_conv <- c("Male" = "M","Female"="F")
            sex_r <- sex_conv[sex]
            
            race_conv <- c("White/Caucasian"="C","African American/Black"="AA")
            race_r <- race_conv[race]
            
            # CVLT
            if (is.na(np_cvlt1to5_z[4])) {
              np_cvlt1to5_tscore <- edc_data["np_cvlt1to5_tscore"]
              np_cvlt1to5_z[4] <- (np_cvlt1to5_tscore - 50)/10
            }
            if (is.na(np_cvlt_sdfr_z[4])) {np_cvlt_sdfr_z[4] <- edc_data["np_cvlt_sdfr_zscore"]}
            if (is.na(np_cvlt_ldfr_z[4])) {np_cvlt_ldfr_z[4] <- edc_data["np_cvlt_ldfr_zscore"]}
            if (is.na(np_cvltrecog_discrim_z[4])) {np_cvltrecog_discrim_z[4] <- edc_data["np_cvltrecog_discrim_zscore"]}
            
            # Biber
            if (is.na(np_biber_t1to5_z[4])) {np_biber_t1to5 <- dde_data[ij,"np_biber_t1to5"];np_biber_t1to5_z[4] <- (np_biber_t1to5 - 114.5) / 34.7}
            if (is.na(np_biber_sd_z[4])) {np_biber_sd <- dde_data[ij,"np_biber_sd"];np_biber_sd_z[4] <- (np_biber_sd - 26.4) / 7}
            if (is.na(np_biber_ld_z[4])) {np_biber_ld <- dde_data[ij,"np_biber_ld"];np_biber_ld_z[4] <- (np_biber_ld - 28) / 7}
            
            #Tower
            if (is.na(np_tower_z[4])) {
              np_tower1 <- as.integer(dde_data[ij,"np_tower1"])-1
              np_tower2 <- as.integer(dde_data[ij,"np_tower2"])-1
              np_tower3 <- as.integer(dde_data[ij,"np_tower3"])-1
              np_tower4 <- as.integer(dde_data[ij,"np_tower4"])-1
              np_tower5 <- as.integer(dde_data[ij,"np_tower5"])-1
              np_tower6 <- as.integer(dde_data[ij,"np_tower6"])-1
              np_tower7 <- as.integer(dde_data[ij,"np_tower7"])-1
              np_tower8 <- as.integer(dde_data[ij,"np_tower8"])-1
              np_tower9 <- as.integer(dde_data[ij,"np_tower9"])-1
              np_tower <- sum(np_tower1,np_tower2,np_tower3,np_tower4,np_tower5,np_tower6,np_tower7,np_tower8,np_tower9)
              tower_ex <- read_excel(ex_path, sheet = 1)
              np_tower_ss <- tower_ex[as.integer(ceiling(np_tower))+1,as.character(age)]
              np_tower_z[4] <- (as.integer(np_tower_ss)-10)/3
            }
            
            # Animal
            if (is.na(np_anim_z[4])) {
              
              np_anim_q1 <- dde_data[ij,"np_anim_q1"]
              np_anim_q2 <- dde_data[ij,"np_anim_q2"]
              np_anim_q3 <- dde_data[ij,"np_anim_q3"]
              np_anim_q4 <- dde_data[ij,"np_anim_q4"]
              np_anim <- sum(c(np_anim_q1, np_anim_q2, np_anim_q3, np_anim_q4))
              anim_ex <- read_excel(ex_path, sheet = "heaton_scaled")
              np_anim_sscore <- anim_ex[as.integer(ceiling(np_anim))+1,4]
              anim_ex <- read_excel(ex_path, sheet = "heaton_animals")
              var_anim <- paste0(sex_r,"/",edu_r,"/",age_r,"/",race_r)
              np_anim_tscore <- anim_ex[as.integer(ceiling(20-np_anim_sscore)),var_anim]
              np_anim_z[4] <- (as.integer(np_anim_tscore) - 50) / 10
            }
            
            if (is.na(np_bnt_z[4])) {
              np_bnt <- dde_data[ij,"np_bnt"]
              np_bnt_z[4] <- (np_bnt - 26) / 3.4
            }
            
            if (is.na(np_inhibit_z[4])) {
              np_inhibit <- dde_data[ij,"np_inhibit"]
              inhibit_ex <- read_excel(ex_path, sheet = 12)
              np_inhibit_ss <- inhibit_ex[as.integer(ceiling(np_inhibit))+1,as.character(age)]
              np_inhibit_z[4] <- (as.integer(np_inhibit_ss)-10)/3
            }
            
            if (is.na(np_fas_z[4])) {
              np_fas <- dde_data[ij,"np_fas"]
              fas_ex <- read_excel(ex_path, sheet = "heaton_scaled")
              np_fas_sscore <- fas_ex[as.integer(ceiling(np_fas))+1,5]
              fas_ex <- read_excel(ex_path, sheet = "heaton_fas")
              var_fas <- paste0(sex_r,"/",edu_r,"/",age_r,"/",race_r)
              np_fas_tscore <- fas_ex[as.integer(ceiling(20-np_fas_sscore)),var_fas]
              np_fas_z[4] <- (as.integer(np_fas_tscore) - 50) / 10 
            }
            
            if (is.na(np_tmtb_z[4])) {
              np_tmtb <- dde_data[ij,"np_tmtb"]
              if (np_tmtb>599) {np_tmtb <- 599}
              tmtb_ex <- read_excel(ex_path, sheet = 6)
              np_tmtb_ss <- tmtb_ex[as.integer(ceiling(np_tmtb))+1,as.character(age)]
              np_tmtb_z[4] <- (as.integer(np_tmtb_ss) - 10)/3
            }
            
            if (is.na(np_tmta_z[4])) {
              np_tmta <- dde_data[ij,"np_tmta"]
              if (np_tmta>599) {np_tmta <- 599}
              tmta_ex <- read_excel(ex_path, sheet = 3)
              np_tmta_ss <- tmta_ex[as.integer(ceiling(np_tmta))+1,as.character(age)]
              np_tmta_z[4] <- (as.integer(np_tmta_ss) - 10)/3
            }
            
            if (is.na(np_hvot_z[4])) {
              np_hvot <- dde_data[ij,"np_hvot"]
              if (age < 55) {hvot_ex <- read_excel(ex_path, sheet = 22)}
              if (55 <= age | age < 60) {hvot_ex <- read_excel(ex_path, sheet = 23)}
              if (60 <= age | age < 65) {hvot_ex <- read_excel(ex_path, sheet = 24)}
              if (65 <= age) {hvot_ex <- read_excel(ex_path, sheet = 25)}
              np_hvot_corrected <- hvot_ex[as.integer(ceiling(np_hvot))+1,as.integer(edu)+2]
              hvot_t_ex <- read_excel(ex_path, sheet = 26)
              np_hvot_tscore <- hvot_t_ex[as.integer(np_hvot_corrected)+1,2]
              np_hvot_z[4] <- -(as.integer(np_hvot_tscore) - 50) / 10
            }
            
            if (is.na(np_digsymb_z[4])) {
              np_digsymb <- dde_data[ij,"np_digsymb"]
              digsymb_ex <- read_excel(ex_path, sheet = 21)
              np_digsymb_ss <- digsymb_ex[as.integer(ceiling(np_digsymb))+1,as.character(age)]
              np_digsymb_z[4] <- (as.integer(np_digsymb_ss)-10)/3
            }
            
            if (is.na(np_color_z[4])) {
              np_color <- dde_data[ij,"np_color"]
              color_ex <- read_excel(ex_path, sheet = 10)
              np_color_ss <- color_ex[as.integer(ceiling(np_color))+1,as.character(age)]
              np_color_z[4] <- (as.integer(np_color_ss)-10)/3
            }
            
            if (is.na(np_word_z[4])) {
              np_word <- dde_data[ij,"np_word"]
              word_ex <- read_excel(ex_path, sheet = 11)
              np_word_ss <- word_ex[as.integer(ceiling(np_word))+1,as.character(age)]
              np_word_z[4] <- (as.integer(np_word_ss)-10)/3
            }
            
            # composite scores
            mem_w<<- cbind(as.numeric(np_cvlt1to5_z),as.numeric(np_cvlt_sdfr_z),as.numeric(np_cvlt_ldfr_z),as.numeric(np_cvltrecog_discrim_z))
            memory_words<<- rowMeans(mem_w)
            mem_s<<- cbind(np_biber_t1to5_z, np_biber_sd_z, np_biber_ld_z)
            memory_shapes<<- rowMeans(mem_s)
            lang<<- cbind(np_anim_z,np_bnt_z)
            language<- rowMeans(lang)
            multitasking<<- rowMeans(cbind(np_tower_z, np_inhibit_z, np_fas_z, np_tmtb_z))
            visuospatial<<- np_hvot_z
            attention<- rowMeans(cbind(np_digsymb_z, np_color_z, np_word_z, np_tmta_z))
            
            counts<- c(memory_words[1:4],memory_shapes[1:4],language[1:4],multitasking[1:4],visuospatial[1:4],attention[1:4]) # Why 1 thru 4?
            
            specie<- c(rep("Memory for\n Words",4),rep("Memory for\n Shapes",4),rep("Language",4),rep("Multi-tasking\n and planning",4),rep("Visuospatial\nskills",4),rep("Attention",4))
            condition<- rep(c(enroll_date, fu_date_prev2, fu_date_prev, fu_date_c),6)
            level_order<- c("Memory for\n Words","Memory for\n Shapes","Language","Multi-tasking\n and planning","Visuospatial\nskills","Attention")
            cond_order<- c(enroll_date, fu_date_prev2, fu_date_prev, fu_date_c)
            spec<- factor(specie,levels = level_order)
            cond<- factor(condition,levels = cond_order)
            data<- data.frame(specie,condition,counts)
            countss<- counts+3
            patterns<- rep(c('crosshatch','circle','none','crosshatch'),6)
            pattern_factor<- c('crosshatch','stripe','circle','none')
            patternss<- factor(patterns,levels = pattern_factor)
            for (ii in 1:length(countss)) {if (countss[ii]<0.25) {countss[ii]<-0.25}; if (countss[ii] > 5.9) {countss[ii]<-5.9}}
            
            barr <- ggplot(data) + theme_bw(14) + 
              scale_fill_manual(values = c("black","white","black","white"))+
              ggpattern::geom_bar_pattern(width = .75,position="dodge", stat="identity",aes(fill=cond, y=countss, x=spec,pattern=patterns),pattern_density=rep(c(0.075,0.075,0.2,0.2),6),pattern_spacing=rep(c(0.015,0.02,0.02,0.02),6),pattern_colour=rep(c("white","black","black","black"),6),pattern_fill = rep(c("black","white","black","black"),6),color="black") +
              scale_y_continuous(name=NULL,breaks=c(1.5,3,4.5),labels = c("Below\nNormal","Normal","Above\nNormal"),limits = c(0,6),expand = c(0, 0))+
              scale_x_discrete(name=NULL) + theme(panel.spacing.x = unit(1,"lines"),plot.background = element_rect(fill = "white",colour = "black",size = 1),plot.margin = unit(c(.3, .8, .3, .3), "cm"),legend.key.size = unit(.21, 'cm'),panel.grid.major.y = element_line(c(1.5,3,4.5),color=c("black","black","black")),panel.grid.minor.y=element_blank(),panel.grid.major.x=element_blank(),axis.text = element_text(colour = "black"),plot.title = element_text(size = 14,face = "bold",hjust = 0.5),axis.ticks = element_blank(),legend.position = "bottom", legend.title = element_blank(),legend.box.margin=margin(-15,0,0,0))+
              coord_fixed(ratio = .4) + labs(title = "Memory Testing Results")
            
            if (any(countss < 1.5)) {
              decline_phys<<- paste0("Based upon the cognitive scores, we suggested that ",first_name," follow-up with a clinical memory work-up for more detailed memory testing. This evaluation can be completed by our colleagues in the Cognitive & Behavioral Neurology Division at Vanderbilt University Medical Center, with a doctor\'s referral (615-936-0060). ")
              decline<<- paste0("As we discussed on ",feedback_date1,", we recommend that you make an appointment for a clinical memory workup for more detailed cognitive testing. You can request a referral from your primary care doctor. We would recommend our colleagues in the Cognitive & Behavioral Neurology Division at Vanderbilt University Medical Center. With a doctor's referral, you can schedule an appointment by calling: 615-936-0060.")
            } else {decline<<- ""; decline_phys<<- ""}
            
          }
        } 
        else {
          # composite scores
          mem_w<<- cbind(as.numeric(np_cvlt1to5_z),as.numeric(np_cvlt_sdfr_z),as.numeric(np_cvlt_ldfr_z),as.numeric(np_cvltrecog_discrim_z))
          memory_words<<- rowMeans(mem_w)
          mem_s<<- cbind(np_biber_t1to5_z, np_biber_sd_z, np_biber_ld_z)
          memory_shapes<<- rowMeans(mem_s)
          lang<<- cbind(np_anim_z,np_bnt_z)
          language<- rowMeans(lang)
          multitasking<<- rowMeans(cbind(np_tower_z, np_inhibit_z, np_fas_z, np_tmtb_z))
          visuospatial<<- np_hvot_z
          attention<- rowMeans(cbind(np_digsymb_z, np_color_z, np_word_z, np_tmta_z))
          
          counts<- c(memory_words[1:4],memory_shapes[1:4],language[1:4],multitasking[1:4],visuospatial[1:4],attention[1:4]) # Why 1 thru 4?
          
          specie<- c(rep("Memory for\n Words",4),rep("Memory for\n Shapes",4),rep("Language",4),rep("Multi-tasking\n and planning",4),rep("Visuospatial\nskills",4),rep("Attention",4))
          condition<- rep(c(enroll_date, fu_date_prev2, fu_date_prev, fu_date_c),6)
          level_order<- c("Memory for\n Words","Memory for\n Shapes","Language","Multi-tasking\n and planning","Visuospatial\nskills","Attention")
          cond_order<- c(enroll_date, fu_date_prev2, fu_date_prev, fu_date_c)
          spec<- factor(specie,levels = level_order)
          cond<- factor(condition,levels = cond_order)
          data<- data.frame(specie,condition,counts)
          countss<- counts+3
          patterns<- rep(c('crosshatch','circle','none','crosshatch'),6)
          pattern_factor<- c('crosshatch','stripe','circle','none')
          patternss<- factor(patterns,levels = pattern_factor)
          for (ii in 1:length(countss)) {if (countss[ii]<0.25) {countss[ii]<-0.25}; if (countss[ii] > 5.9) {countss[ii]<-5.9}}
          
          barr <- ggplot(data) + theme_bw(14) + 
            scale_fill_manual(values = c("black","white","black","white"))+
            ggpattern::geom_bar_pattern(width = .75,position="dodge", stat="identity",aes(fill=cond, y=countss, x=spec,pattern=patterns),pattern_density=rep(c(0.075,0.075,0.2,0.2),6),pattern_spacing=rep(c(0.015,0.02,0.02,0.02),6),pattern_colour=rep(c("white","black","black","black"),6),pattern_fill = rep(c("black","white","black","black"),6),color="black") +
            scale_y_continuous(name=NULL,breaks=c(1.5,3,4.5),labels = c("Below\nNormal","Normal","Above\nNormal"),limits = c(0,6),expand = c(0, 0))+
            scale_x_discrete(name=NULL) + theme(panel.spacing.x = unit(1,"lines"),plot.background = element_rect(fill = "white",colour = "black",size = 1),plot.margin = unit(c(.3, .8, .3, .3), "cm"),legend.key.size = unit(.21, 'cm'),panel.grid.major.y = element_line(c(1.5,3,4.5),color=c("black","black","black")),panel.grid.minor.y=element_blank(),panel.grid.major.x=element_blank(),axis.text = element_text(colour = "black"),plot.title = element_text(size = 14,face = "bold",hjust = 0.5),axis.ticks = element_blank(),legend.position = "bottom", legend.title = element_blank(),legend.box.margin=margin(-15,0,0,0))+
            coord_fixed(ratio = .4) + labs(title = "Memory Testing Results")
          
          if (any(countss < 1.5)) {
            decline_phys<<- paste0("Based upon the cognitive scores, we suggested that ",first_name," follow-up with a clinical memory work-up for more detailed memory testing. This evaluation can be completed by our colleagues in the Cognitive & Behavioral Neurology Division at Vanderbilt University Medical Center, with a doctor\'s referral (615-936-0060). ")
            decline<<- paste0("As we discussed on ",feedback_date1,", we recommend that you make an appointment for a clinical memory workup for more detailed cognitive testing. You can request a referral from your primary care doctor. We would recommend our colleagues in the Cognitive & Behavioral Neurology Division at Vanderbilt University Medical Center. With a doctor's referral, you can schedule an appointment by calling: 615-936-0060.")
          } else {decline<<- ""; decline_phys<<- ""}
          
        }
      } # End of memory testing
  
      print("Compiling Epoch 5 Heart Results")
      
      echo_p2 <- echo_datas[which(echo_datas[,"redcap_event_name"]== events[epochh-1]),]
      echo_p1 <- echo_datas[which(echo_datas[,"redcap_event_name"]== events[epochh]),]
      echo_c <- echo_datas[which(echo_datas[,"redcap_event_name"]== events[epochh+1]),]
      
      lv_p2 <<- paste0("     1.  ",as.character(echo_p2$echo_find1_lv_dys_fx))
      val_p2<<- as.character(echo_p2$echo_find2_valve_fx)
      if (val_p2=="Normal"){val_p2<<-"     3.  No significant"} else {val_p2<<-"     3.  Significant"}
      lv_p<<- paste0("     1.  ",as.character(echo_p1$echo_find1_lv_dys_fx))
      val_p<<- as.character(echo_p1$echo_find2_valve_fx)
      if (val_p=="Normal"){val_p<<-"     2.  No significant"} else {val_p<<-"     2.  Significant"}
      lv_c <<- paste0("     1.  ",as.character(echo_c$echo_find1_lv_dys_fx))
      val_c<<- as.character(echo_c$echo_find2_valve_fx)
      if (val_c=="Normal"){val_c<<-"     2.  No significant"} else {val_c<<-"     2.  Significant"}
      
      qids <- edc_data$qids
      gds <- edc_data$gds_total_score
      #if (is.null(row.names(visit_depress))==FALSE) {visit_depress <<- 0}
      #if (is.na(visit_depress)) {visit_depress <<- 0}
      if (qids > 5 | gds > 4) {
        int <- "mild"
        if (gds > 8 | qids > 10) {int <- "moderate"}
        if (gds > 11 | qids > 15) {int <- "severe"}
        gds_phys <<- paste0("On a measure assessing depressive symptoms, ",first_name," scored in a range suggesting ",int," symptoms of depression. Based upon this score, we recommended that ",first_name," make an appointment for a more detailed clinical assessment of these symptoms.")
        gds <<- paste0("As discussed on ",feedback_date1,", your scores on a measure assessing depressive symptoms fell in a range suggesting ",int," symptoms of depression.  We recommend you make an appointment for a more detailed clinical assessment of these symptoms.  You can request a referral from your primary care doctor.  We would recommend our colleagues who offer clinical services in the Department of Psychiatry at Vanderbilt University.  You can schedule an appointment by calling: 615-936-3555.")
      } else {gds<<- ""; gds_phys<<- ""}
      
      ei_p2 <- echo_p2$extracardiac_incidental; if (is.na(ei_p2)) {ei_p2 <- "No"}
      ei_p1 <- echo_p1$extracardiac_incidental; if (is.na(ei_p1)) {ei_p1 <- "No"}
      ei_c <- echo_c$extracardiac_incidental; if (is.na(ei_c)) {ei_c <- "No"}
      if (ei_p2=="Yes") {lung_p2<<-paste("     4.  ",echo_p2$extracardiac_incidental_describe)} else {lung_p2<<-""}
      if (ei_p1=="Yes") {lung_p<<-paste("     3.  ",echo_p1$extracardiac_incidental_describe)} else {lung_p<<-""}
      if (ei_c=="Yes") {lung_c<<-paste("     3.  ",echo_c$extracardiac_incidental_describe)} else {lung_c<<-""}
      
      bi_p2 <- echo_p2$brain_incidental; if (is.na(bi_p2)) {bi_p2 <- "No"}
      bi_p1 <- echo_p1$brain_incidental; if (is.na(bi_p1)) {bi_p1 <- "No"}
      bi_c <- echo_c$brain_incidental; if (is.na(bi_c)) {bi_c <- "No"}
      if (bi_p2=="Yes" | bi_p1=="Yes" | bi_c=="Yes") {
        brain_intro1<<-"Brain Test Results"
        brain_intro2<<-"You underwent brain testing, which was read by board-certified neuroradiologists."
      } else {brain_intro1<<-""; brain_intro2<<-""}
      
      if (bi_p2=="Yes") {brain_ip2 <<-paste0("Your _ results on ",fu_date_prev2," were as follows:"); brain_p2 <<-echo_p2$brain_incidental_davis} else {brain_ip2 <<-""; brain_p2 <<-""}
      
      if (bi_p1=="Yes") {brain_ip <<-paste0("Your _ results on ",fu_date_prev," were as follows:"); brain_p <<-echo_p1$brain_incidental_davis} else {brain_ip <<-""; brain_p <<-""}
      
      if (bi_c=="Yes") {brain_ic <<-paste0("Your _ results on ",fu_date_c," were as follows:"); brain_c <<-echo_c$brain_incidental_davis} else {brain_ic <<-""; brain_c <<-""}
      
      ptp_path<<- paste0(main_path,"resources/Templates/Feedback/MAP2_fb_temp.docx")
      phys_path<<- paste0(main_path,"resources/Templates/Feedback/MAP2_phys_temp.docx")
    }
    
    if (e == 1) {
      
      enroll_date <<- format(as.Date(echo_data$consent_date), "%m/%d/%Y")
      if (is.na(enroll_date)) {enroll_date <<- format(as.Date(map_data[1, "visit1_date"]), "%m/%d/%Y")}
      
      age <- pdb_data$age
      edu <- pdb_data$education
      race <- pdb_data$np_norm_race
      
      if (age < 55) {age_r <- "50-54"}; if (age < 60 & age >= 55) {age_r <- "55-59"}; if (age < 65 & age >= 60) {age_r <- "60-64"}
      if (age < 70 & age >= 65) {age_r <- "65-69"}; if (age < 75 & age >= 70) {age_r <- "70-74"} 
      if (age < 80 & age >= 75) {age_r <- "75-79"}; if (age >= 80) {age_r <- "80-85"}
      
      if (edu < 9) {edu_r <- "7-8"}; if (edu >= 9 & edu < 12 ) {edu_r <- "9-11"}; if (edu == 12 ) {edu_r <- "12"}
      if (edu >= 13 & edu < 16 ) {edu_r <- "13-15"}; if (edu >= 16 & edu < 18 ) {edu_r <- "16-17"}
      if (edu >= 18) {edu_r <- "18-20"}
      
      sex_conv <- c("Male" = "M","Female"="F")
      sex_r <- sex_conv[sex]
      
      race_conv <- c("White/Caucasian"="C","African American/Black"="AA")
      race_r <- race_conv[race]
      
      print("Creating Data Tables")
      
      df <- data.frame(
        Test = c("Heart rate", "Blood pressure", "Height", "Weight", "Body Mass Index"),
        Test.1 = c("Heart rate", "Blood pressure", "Height", "Weight", "Body Mass Index"),
        CR = c(echo_data$echo_hrate, echo_data$echo_read_sbp, paste0(round(echo_data$height*0.393701)," inches"), paste0(round(echo_data$weight*2.205)," lbs"), round((echo_data$weight/(echo_data$height/100)^2), digits = 1)),
        CR = c(echo_data$echo_hrate, echo_data$echo_read_dbp, paste0(round(echo_data$height*0.393701)," inches"), paste0(round(echo_data$weight*2.205)," lbs"), round((echo_data$weight/(echo_data$height/100)^2), digits = 1)),
        NR=c("60-100", "<120 / <80", "-", "-", "18.5-24.9")
      )
      
      df[df == "-9999" | df == "-3937 inches" | df == "-22048 lbs" | df == "-1" | df == "Missing" | is.na(df)] <-"-"
      
      
      if(any(which(df=="-")==31)) {df <- df[-c(7,8)]}
      if(any(which(df=="-")==21)) {df <- df[-c(5,6)]}
      
      ft <- flextable(df)
      
      ft <- set_header_labels(ft, Test = "Test", Test.1="Test",
                              CR = paste0("Current Results ", enroll_date),CR.1 = paste0("Current Results ", enroll_date),
                              NR = "Normal Range*" )
      ft <- bg(ft, bg="grey",part = "header")
      ft <- font(ft,fontname = "Arial",part = "header")
      ft <- font(ft,fontname = "Arial",part = "body")
      ft <- align(ft, align = "center", part="header")
      ft <- align(ft, align = "center", part="body")
      ft <- theme_box(ft)
      
      
    
      ft <- fontsize(ft, j=1:5, size = 10, part="header")
      ft <- fontsize(ft, j=1:5, size = 10, part="body")
      ft <- width(ft,j = 5, width = 1)
      
      ft <- merge_h(ft,part = "header")
      ft <- merge_h_range(ft,i = 1:5,j1=1, j2=2 ,part = "body")
      ft <- merge_h_range(ft,i = 1,j1=3, j2=4 ,part = "body")
      #ft <- merge_h_range(ft,i = 1,j1=5, j2=6 ,part = "body")
      #ft <- merge_h_range(ft,i = 1,j1=7, j2=8 ,part = "body")
      ft <- merge_h_range(ft,i = 3:5,j1=3, j2=4 ,part = "body")
      #ft <- merge_h_range(ft,i = 3:5,j1=5, j2=6 ,part = "body")
      #ft <- merge_h_range(ft,i = 3:5,j1=7, j2=8 ,part = "body")
      ft <- width(ft,j = 3:4, width = .5)
      ft <- width(ft, j=1:2, width = .75)
      ft <- align(ft, i = 1:3, j = 1:2, align="left",part="body")
      evens<-c(4)
    
      
      
      
      for (ii in 3:(ncol(df)-1)) {
        #Heart rate
        if (df[1,ii]=="-"){
          #print(paste0(ii,":",df[1,ii]))
          ft <- bold(ft, i = 1, j = ii, bold = FALSE, part = "body")}
        else {
          if ((as.integer(df[1,ii]) < 60) | (as.integer(df[1,ii]) >100)){
            ft <- bold(ft, i = 1, j = ii, bold = TRUE, part = "body")}
        }
        #Blood pressure - systolic
        if (df[2,ii]=="-"){
          print(paste0(ii,":",df[2,ii]))
          ft <- bold(ft, i = 2, j = ii, bold = FALSE, part = "body")}
        else {
          if (as.integer(df[2,ii]) > 120){
            ft <- bold(ft, i = 2, j = ii, bold = TRUE, part = "body")}
        }
        
        #BMI
        if (df[5,ii]=="-"){
          print(paste0(ii,":",df[5,ii]))
          ft <- bold(ft, i = 5, j = ii, bold = FALSE, part = "body")}
        else {
          if ((as.double(df[5,ii]) < 18.5 | as.double(df[5,ii]) > 24.9)){
            ft <- bold(ft, i = 5, j = ii, bold = TRUE, part = "body")}
        }
      }
      #Blood pressure -- diastolic
      for (ii in evens) {
        #Blood pressure
        if (df[2,ii]=="-"){
          #print(paste0(ii,":",df[2,ii]))
          ft <- bold(ft, i = 2, j = ii, bold = FALSE, part = "body")}
        else {
          if ((as.integer(df[2,ii]) > 80)){
            ft <- bold(ft, i = 2, j = ii, bold = TRUE, part = "body")}
        }
      }
      
      
      df2 <- data.frame(
        Test1 = c("Cholesterol", "Cholesterol", "Cholesterol", "Cholesterol", "Blood Sugar", "Blood Sugar", "Blood Sugar", "Thyroid", "Inflammation"),
        Test2 = c("Total","HDL", "LDL", "Triglycerides", "Hemoglobin A1C", "Fasting Insulin", "Fasting Glucose", "Thyroid Stimulating Hormone (TSH)", "High Sensitivity C-Reactive Protein"),
        CR = as.character(c(echo_data$bld_c_chol, echo_data$bld_c_hdlc, echo_data$bld_c_ldlc, echo_data$bld_c_trig, echo_data$bld_c_hgba1c, echo_data$bld_c_insulin, echo_data$bld_c_glucose, echo_data$bld_c_tsh, echo_data$bld_c_crp)),  
        NR = c("<200", "men >40, women >50", "<100", "<150", "<5.7", "<17", "70-99", "0.35-3.6", "0-2.9")
      )
      
      
      df2[df2 == "-9999" | is.na(df2)] <-"-"
      
      if(any(which(df2=="-")==37)) {df2 <- df2[-c(5)]}
      if(any(which(df2=="-")==28)) {df2 <- df2[-c(4)]}
      
      ft2 <- flextable(df2)
      ft2 <- set_header_labels(ft2, Test1 = "Test", Test2 = "Test", CR = paste0("Current Results ", enroll_date), NR = "Normal Range/\nCut-off*" )
      ft2 <- bg(ft2, bg="grey",part = "header")
      ft2 <- font(ft2,fontname = "Arial",part = "header")
      ft2 <- font(ft2,fontname = "Arial",part = "body")
      ft2 <- theme_box(ft2)
      
    
      ft2 <- width(ft2, j = 1:4, width=.9)
      ft2 <- merge_at(ft2, i = 1, j = 1:2, part = "header")
      ft2 <- merge_at(ft2, i = 1:4, j = 1, part = "body")
      ft2 <- merge_at(ft2, i = 5:7, j = 1, part = "body")
      ft2 <- fontsize(ft2, j=1, size = 9, part="body")
      ft2 <- fontsize(ft2, j=1:4, size = 10, part="header")
      ft2 <- fontsize(ft2, j=2:4, size = 10, part="body")
      ft2 <- align(ft2, align = "center", part="header")
      ft2 <- align(ft2, align = "center", part="body")
      ft2 <- align(ft2, i=1:9, j=2, align="left",part="body")
      ft2 <- valign(ft2, i=1:9, j=3:4, valign="center", part="body")
      ft2 <- height(ft2, height = .4, part = "header")
      #ft2 <- width(ft2, j = 1, width = .85)
      ft2 <- width(ft2, j = 2, width = 1.25)
    
      
      for (ii in 3:(ncol(df2)-1)) {
        if (df2[1,ii]=="-"){ft2<-bold(ft2, i = 1, j = ii, bold = FALSE, part = "body")}else{if(as.integer(df2[1,ii]) > 200){ft2<-bold(ft2, i = 1, j = ii, bold = TRUE, part = "body")}}
        if (df2[2,ii]=="-"){
          ft2<-bold(ft2, i = 2, j = ii, bold = FALSE, part = "body")}
        else{
          if (sex == "Female" & as.integer(df2[2,ii]) < 50){ft2 <- bold(ft2, i = 2, j = ii, bold = TRUE, part = "body")}
          if (sex == "Male" & as.integer(df2[2,ii]) < 40){ft2 <- bold(ft2, i = 2, j = ii, bold = TRUE, part = "body")}
        }
        
        if (df2[3,ii]=="-"){ft2<-bold(ft2, i = 3, j = ii, bold = FALSE, part = "body")}else{if(as.integer(df2[3,ii]) > 100){ft2<-bold(ft2, i = 3, j = ii, bold = TRUE, part = "body")}}
        if (df2[4,ii]=="-"){ft2<-bold(ft2, i = 4, j = ii, bold = FALSE, part = "body")}else{if(as.integer(df2[4,ii]) > 150){ft2<-bold(ft2, i = 4, j = ii, bold = TRUE, part = "body")}}
        if (df2[5,ii]=="-"){ft2<-bold(ft2, i = 5, j = ii, bold = FALSE, part = "body")}else{if(as.integer(df2[5,ii]) > 5.7){ft2<-bold(ft2, i = 5, j = ii, bold = TRUE, part = "body")}}
        if(class(df2[6,ii])=="character") {df2[6,ii] <- NA} else {
          if (is.na(as.double(df2[6,ii]))) {ft2 <- bold(ft2, i = 6, j = ii, bold = TRUE, part = "body")} else {
            if (df2[6,ii]=="-"){ft2<-bold(ft2, i = 6, j = ii, bold = FALSE, part = "body")}else{if(as.integer(df2[6,ii]) > 17){ft2<-bold(ft2, i = 6, j = ii, bold = TRUE, part = "body")}}
          }
        }      
        if (df2[7,ii]=="-"){ft2<-bold(ft2, i = 7, j = ii, bold = FALSE, part = "body")}else{if(as.integer(df2[7,ii]) > 99 | as.integer(df2[7,ii]) < 70){ft2<-bold(ft2, i = 7, j = ii, bold = TRUE, part = "body")}}
        if (df2[8,ii]=="-"){ft2<-bold(ft2, i = 8, j = ii, bold = FALSE, part = "body")}else{if(as.integer(df2[8,ii]) > 3.6 | as.integer(df2[8,ii]) < 0.35){ft2<-bold(ft2, i = 8, j = ii, bold = TRUE, part = "body")}}
        
        if(class(df2[9,ii])=="character") {df2[9,ii] <- NA} else {
          if (is.na(as.double(df2[9,ii]))) {ft2 <- bold(ft2, i = 9, j = ii, bold = TRUE, part = "body")} else {
            if ((as.double(df2[9,ii]) > 2.9) | (as.double(df2[9,ii]) < 0)) {ft2 <- bold(ft2, i = 9, j = ii, bold = TRUE, part = "body")}}
        }
      }
      
      print("Enrollment Memory Results")
      incomplete <- any(dde_data==-7777); if (is.na(incomplete)) {incomplete <- 0}
      dnf <- any(dde_data==-9999); if (is.na(dnf)) {dnf <- 0}
      miss <- any(is.na(dde_data[ij,3:28])); if(is.na(miss)) {miss <- 0}
      if (incomplete | dnf | miss) {err <- "Some NP data is missing from the DDE; Leter generated without Memory Chart"; barr <- "Not enough data to create barchart"} else {
        
        # CVLT
        np_cvlt1to5_tscore <- edc_data["np_cvlt1to5_tscore"]
        np_cvlt1to5_z <- (np_cvlt1to5_tscore - 50)/10
        
        np_cvlt_sdfr_z <- edc_data["np_cvlt_sdfr_zscore"]
        np_cvlt_ldfr_z <- edc_data["np_cvlt_ldfr_zscore"]
        np_cvltrecog_discrim_z <- edc_data["np_cvltrecog_discrim_zscore"]
        
        # Biber
        np_biber_t1to5 <- dde_data[ij,"np_biber_t1to5"];np_biber_t1to5_z <- (np_biber_t1to5 - 114.5) / 34.7
        np_biber_sd <- dde_data[ij,"np_biber_sd"];np_biber_sd_z <- (np_biber_sd - 26.4) / 7
        np_biber_ld <- dde_data[ij,"np_biber_ld"];np_biber_ld_z <- (np_biber_ld - 28) / 7
        
        #Tower
        np_tower1 <- as.integer(dde_data[ij,"np_tower1"])-1
        np_tower2 <- as.integer(dde_data[ij,"np_tower2"])-1
        np_tower3 <- as.integer(dde_data[ij,"np_tower3"])-1
        np_tower4 <- as.integer(dde_data[ij,"np_tower4"])-1
        np_tower5 <- as.integer(dde_data[ij,"np_tower5"])-1
        np_tower6 <- as.integer(dde_data[ij,"np_tower6"])-1
        np_tower7 <- as.integer(dde_data[ij,"np_tower7"])-1
        np_tower8 <- as.integer(dde_data[ij,"np_tower8"])-1
        np_tower9 <- as.integer(dde_data[ij,"np_tower9"])-1
        np_tower <- sum(np_tower1,np_tower2,np_tower3,np_tower4,np_tower5,np_tower6,np_tower7,np_tower8,np_tower9)
        tower_ex <- read_excel(ex_path, sheet = 1)
        np_tower_ss <- tower_ex[as.integer(ceiling(np_tower))+1,as.character(age)]
        np_tower_z <- (as.integer(np_tower_ss)-10)/3
        
        # Animal
        np_anim_q1 <- dde_data[ij,"np_anim_q1"]
        np_anim_q2 <- dde_data[ij,"np_anim_q2"]
        np_anim_q3 <- dde_data[ij,"np_anim_q3"]
        np_anim_q4 <- dde_data[ij,"np_anim_q4"]
        np_anim <- sum(c(np_anim_q1, np_anim_q2, np_anim_q3, np_anim_q4))
        anim_ex <- read_excel(ex_path, sheet = "heaton_scaled")
        np_anim_sscore <- anim_ex[as.integer(ceiling(np_anim))+1,4]
        anim_ex <- read_excel(ex_path, sheet = "heaton_animals")
        var_anim <- paste0(sex_r,"/",edu_r,"/",age_r,"/",race_r)
        np_anim_tscore <- anim_ex[as.integer(ceiling(20-np_anim_sscore)),var_anim]
        np_anim_z <- (as.integer(np_anim_tscore) - 50) / 10
        
        np_bnt <- dde_data[ij,"np_bnt"]
        np_bnt_z <- (np_bnt - 26) / 3.4
        
        np_inhibit <- dde_data[ij,"np_inhibit"]
        inhibit_ex <- read_excel(ex_path, sheet = 12)
        np_inhibit_ss <- inhibit_ex[as.integer(ceiling(np_inhibit))+1,as.character(age)]
        np_inhibit_z <- (as.integer(np_inhibit_ss)-10)/3
        
        np_fas <- dde_data[ij,"np_fas"]
        fas_ex <- read_excel(ex_path, sheet = "heaton_scaled")
        np_fas_sscore <- fas_ex[as.integer(ceiling(np_fas))+1,5]
        fas_ex <- read_excel(ex_path, sheet = "heaton_fas")
        var_fas <- paste0(sex_r,"/",edu_r,"/",age_r,"/",race_r)
        np_fas_tscore <- fas_ex[as.integer(ceiling(20-np_fas_sscore)),var_fas]
        np_fas_z <- (as.integer(np_fas_tscore) - 50) / 10 
        
        np_tmtb <- dde_data[ij,"np_tmtb"]
        if (np_tmtb>599) {np_tmtb <- 599}
        tmtb_ex <- read_excel(ex_path, sheet = 6)
        np_tmtb_ss <- tmtb_ex[as.integer(ceiling(np_tmtb))+1,as.character(age)]
        np_tmtb_z <- (as.integer(np_tmtb_ss) - 10)/3
        
        np_tmta <- dde_data[ij,"np_tmta"]
        if (np_tmta>599) {np_tmta <- 599}
        tmta_ex <- read_excel(ex_path, sheet = 3)
        np_tmta_ss <- tmta_ex[as.integer(ceiling(np_tmta))+1,as.character(age)]
        np_tmta_z <- (as.integer(np_tmta_ss) - 10)/3
        
        np_hvot <- dde_data[ij,"np_hvot"]
        if (age < 55) {hvot_ex <- read_excel(ex_path, sheet = 22)}
        if (55 <= age | age < 60) {hvot_ex <- read_excel(ex_path, sheet = 23)}
        if (60 <= age | age < 65) {hvot_ex <- read_excel(ex_path, sheet = 24)}
        if (65 <= age) {hvot_ex <- read_excel(ex_path, sheet = 25)}
        np_hvot_corrected <- hvot_ex[as.integer(ceiling(np_hvot))+1,as.integer(edu)+2]
        hvot_t_ex <- read_excel(ex_path, sheet = 26)
        np_hvot_tscore <- hvot_t_ex[as.integer(np_hvot_corrected)+1,2]
        np_hvot_z <- -(as.integer(np_hvot_tscore) - 50) / 10
        
        np_digsymb <- dde_data[ij,"np_digsymb"]
        digsymb_ex <- read_excel(ex_path, sheet = 21)
        np_digsymb_ss <- digsymb_ex[as.integer(ceiling(np_digsymb))+1,as.character(age)]
        np_digsymb_z <- (as.integer(np_digsymb_ss)-10)/3
        
        np_color <- dde_data[ij,"np_color"]
        color_ex <- read_excel(ex_path, sheet = 10)
        np_color_ss <- color_ex[as.integer(ceiling(np_color))+1,as.character(age)]
        np_color_z <- (as.integer(np_color_ss)-10)/3
        
        np_word <- dde_data[ij,"np_word"]
        word_ex <- read_excel(ex_path, sheet = 11)
        np_word_ss <- word_ex[as.integer(ceiling(np_word))+1,as.character(age)]
        np_word_z <- (as.integer(np_word_ss)-10)/3
        
        
        # composite scores
        mem_w<<- cbind(as.numeric(np_cvlt1to5_z),as.numeric(np_cvlt_sdfr_z),as.numeric(np_cvlt_ldfr_z),as.numeric(np_cvltrecog_discrim_z))
        memory_words<<- rowMeans(mem_w)
        mem_s<<- cbind(np_biber_t1to5_z, np_biber_sd_z, np_biber_ld_z)
        memory_shapes<<- rowMeans(mem_s)
        lang<<- cbind(np_anim_z,np_bnt_z)
        language<- rowMeans(lang)
        multitasking<<- rowMeans(cbind(np_tower_z, np_inhibit_z, np_fas_z, np_tmtb_z))
        visuospatial<<- np_hvot_z
        attention<- rowMeans(cbind(np_digsymb_z, np_color_z, np_word_z, np_tmta_z))
        
        counts<- c(memory_words,memory_shapes,language,multitasking,visuospatial,attention) 
        
        specie<- c("Memory for\n Words","Memory for\n Shapes","Language","Multi-tasking\n and planning","Visuospatial\nskills","Attention")
        condition<- rep(c(enroll_date),6)
        level_order<- c("Memory for\n Words","Memory for\n Shapes","Language","Multi-tasking\n and planning","Visuospatial\nskills","Attention")
        cond_order<- c(enroll_date)
        spec<- factor(specie,levels = level_order)
        cond<- factor(condition,levels = cond_order)
        data<- data.frame(specie,condition,counts)
        countss<- counts+3
        patterns<- rep(c('crosshatch'),6)
        pattern_factor<- c('crosshatch')
        patternss<- factor(patterns,levels = pattern_factor)
        for (ii in 1:length(countss)) {if (countss[ii]<0.25) {countss[ii]<-0.25}; if (countss[ii] > 5.9) {countss[ii]<-5.9}}
        
        barr <- ggplot(data) + theme_bw(14)+scale_fill_manual(values = c("black"))+
          ggpattern::geom_bar_pattern(width = .75,position="dodge", stat="identity",aes(fill=cond, y=countss, x=spec,pattern=patterns),color=rep(c("black"),6),pattern_density=rep(c(0.5),6),pattern_colour=rep(c("black"),6),pattern_fill = rep(c("white"),6),pattern_spacing=.02) +
          scale_y_continuous(name=NULL,breaks=c(1.5,3,4.5),labels = c("Below\nNormal","Normal","Above\nNormal"),limits = c(0,6),expand = c(0, 0))+
          scale_x_discrete(name=NULL) + theme(panel.spacing.x = unit(1,"lines"),plot.background = element_rect(fill = "white",colour = "black",size = 1),plot.margin = unit(c(.3, .8, .3, .3), "cm"),legend.key.size = unit(.21, 'cm'),panel.grid.major.y = element_line(c(1.5,3,4.5),color=c("black","black","black")),panel.grid.minor.y=element_blank(),panel.grid.major.x=element_blank(),axis.text = element_text(colour = "black"),plot.title = element_text(size = 14,face = "bold",hjust = 0.5),axis.ticks = element_blank(),legend.position = "bottom", legend.title = element_blank(),legend.box.margin=margin(-15,0,0,0))+
          coord_fixed(ratio = .4) + labs(title = "Memory Testing Results")
        
        if (any(countss < 1.5)) {
          decline_phys<<- paste0("Based upon the cognitive scores, we suggested that ",first_name," follow-up with a clinical memory work-up for more detailed memory testing. This evaluation can be completed by our colleagues in the Cognitive & Behavioral Neurology Division at Vanderbilt University Medical Center, with a doctor\'s referral (615-936-0060). ")
          decline<<- paste0("As we discussed on ",feedback_date1,", we recommend that you make an appointment for a clinical memory workup for more detailed cognitive testing. You can request a referral from your primary care doctor. We would recommend our colleagues in the Cognitive & Behavioral Neurology Division at Vanderbilt University Medical Center. With a doctor's referral, you can schedule an appointment by calling: 615-936-0060.")
        } else {decline<<- ""; decline_phys<<- ""}
        
      }
      
      
      print("Compiling Heart Results")
      
      echo_c <- echo_datas[which(echo_datas[,"redcap_event_name"]== events[epochh+1]),]
      
      lv_c <<- paste0("     1.  ",as.character(echo_c$echo_find1_lv_dys_fx))
      val_c<<- as.character(echo_c$echo_find2_valve_fx)
      if (val_c=="Normal"){val_c<<-"     2.  No significant"} else {val_c<<-"     2.  Significant"}
      
      qids <- dep_data$qids
      gds <- dep_data$gds_total_score
      #if (is.null(row.names(visit_depress))==FALSE) {visit_depress <<- 0}
      #if (is.na(visit_depress)) {visit_depress <<- 0}
      if (qids > 5 | gds > 4) {
        int <- "mild"
        if (gds > 8 | qids > 10) {int <- "moderate"}
        if (gds > 11 | qids > 15) {int <- "severe"}
        gds_phys <<- paste0("On a measure assessing depressive symptoms, ",first_name," scored in a range suggesting ",int," symptoms of depression. Based upon this score, we recommended that ",first_name," make an appointment for a more detailed clinical assessment of these symptoms.")
        gds <<- paste0("As discussed on ",feedback_date1,", your scores on a measure assessing depressive symptoms fell in a range suggesting ",int," symptoms of depression.  We recommend you make an appointment for a more detailed clinical assessment of these symptoms.  You can request a referral from your primary care doctor.  We would recommend our colleagues who offer clinical services in the Department of Psychiatry at Vanderbilt University.  You can schedule an appointment by calling: 615-936-3555.")
      } else {gds<<- ""; gds_phys<<- ""}
      
      ei_c <- echo_c$extracardiac_incidental; if (is.na(ei_c)) {ei_c <- "No"}
      if (ei_c=="Yes") {lung_c<<-paste("     3.  ",echo_c$extracardiac_incidental_describe)} else {lung_c<<-""}
      
      bi_c <- echo_c$brain_incidental; if (is.na(bi_c)) {bi_c <- "No"}
      if (bi_c=="Yes") {
        brain_intro1<<-"Brain Test Results"
        brain_intro2<<-"You underwent brain testing, which was read by board-certified neuroradiologists."
      } else {brain_intro1<<-""; brain_intro2<<-""}
      
      if (bi_c=="Yes") {brain_ic <<-paste0("Your _ results on ",enroll_date," were as follows:"); brain_c <<-echo_c$brain_incidental_davis} else {brain_ic <<-""; brain_c <<-""}
      
      ptp_path<<- paste0(main_path,"resources/Templates/Feedback/MAP2_fb_temp_e.docx")
      phys_path<<- paste0(main_path,"resources/Templates/Feedback/MAP2_phys_temp_e.docx")
    }
    
    ptp_temp<<- paste0(out_path,"ptp_temp.docx")
    phys_temp<<- paste0(out_path,"phys_temp.docx")
    Plots<<- list(membar = function() print(barr))
    addPlots(ptp_path,ptp_temp,Plots,width = 7.3,height = 3.6)
    addPlots(phys_path,phys_temp,Plots,width = 7.3,height = 3.6)
    FT<<- list(ft = ft, ft2 = ft2)
    body_add_flextables(ptp_temp,ptp_temp, FT)
    body_add_flextables(phys_temp,phys_temp, FT)
    
    if (nchar(map_id)==1) {input<<- paste0("00",map_id)} else if (nchar(map_id)==2) {input<<- paste0("0",map_id)} else {input<<- map_id}
    vmac_id<<- as.character(pdb_data[i,"vmac_id"])
    if (nchar(vmac_id)==1) {record<<- paste0("0000",vmac_id)} else if (nchar(vmac_id)==2) {record<<- paste0("000",vmac_id)} else if (nchar(vmac_id)==3) {record<<- paste0("00",vmac_id)} else if (nchar(vmac_id)==4) {record<<- paste0("0",vmac_id)} else {record<<- vmac_id}
    output<- paste0(out_path,"feedback_MAP",input,"_",epoch,".docx")
    renderInlineCode(ptp_temp, output)
    
    importFiles(rcon = pdb, file = output, record = record, field = "feedback_letter", event = events[e+1],
                overwrite = TRUE, repeat_instance = 1)
    
    print("Imported File")
    
    num_phys <- pdb_data$feedback_number_letter
    
    if (num_phys > 0) {
      first_name_physician<<-first_name_physician1
      last_name_physician<<- last_name_physician1
      credentials<<- credentials1
      street_address_physician<<- street_address_physician1
      city_physician<<- city_physician1
      state_physician<<- state_physician1
      zip_physician<<- zip_physician1
      
      output<- paste0(out_path,"physician_letter_MAP_",input,"_",epoch,".docx")
      renderInlineCode(phys_temp, output)
      
      importFiles(rcon = pdb, file = output, record = record, field = "feedback_physician1_letter", event = events[e+1],
                  overwrite = TRUE, repeat_instance = 1)
    
      if (num_phys > 1) {
        first_name_physician<<-first_name_physician2
        last_name_physician<<- last_name_physician2
        credentials<<- credentials2
        street_address_physician<<- street_address_physician2
        city_physician<<- city_physician2
        state_physician<<- state_physician2
        zip_physician<<- zip_physician2
        
        output<- paste0(out_path,"physician2_letter_MAP_",input,"_",epoch,".docx")
        renderInlineCode(phys_temp, output)
        
        importFiles(rcon = pdb, file = output, record = record, field = "feedback_physician2_letter", event = events[e+1],
                    overwrite = TRUE, repeat_instance = 1)
    
        if (num_phys > 2) {
          first_name_physician<<-first_name_physician3
          last_name_physician<<- last_name_physician3
          credentials<<- credentials3
          street_address_physician<<- street_address_physician3
          city_physician<<- city_physician3
          state_physician<<- state_physician3
          zip_physician<<- zip_physician3
          
          output<- paste0(out_path,"physician3_letter_MAP_",input,"_",epoch,".docx")
          renderInlineCode(phys_temp, output)
          
          importFiles(rcon = pdb, file = output, record = record, field = "feedback_physician3_letter",event = events[e+1],
                      overwrite = TRUE, repeat_instance = 1)
    
          if (num_phys > 3) {
            first_name_physician<<-first_name_physician4
            last_name_physician<<- last_name_physician4
            credentials<<- credentials4
            street_address_physician<<- street_address_physician4
            city_physician<<- city_physician4
            state_physician<<- state_physician4
            zip_physician<<- zip_physician4
            
            output<- paste0(out_path,"physician4_letter_MAP_",input,"_",epoch,".docx")
            renderInlineCode(phys_temp, output)
            
            importFiles(rcon = pdb, file = output, record = record, field = "feedback_physician4_letter",event = events[e+1],
                        overwrite = TRUE, repeat_instance = 1)
            
            if (is.na(first_name_physician5)==FALSE) {
              first_name_physician<<-first_name_physician5
              last_name_physician<<- last_name_physician5
              credentials<<- credentials5
              street_address_physician<<- street_address_physician5
              city_physician<<- city_physician5
              state_physician<<- state_physician5
              zip_physician<<- zip_physician5
              
              output<- paste0(out_path,"physician5_letter_MAP_",input,"_",epoch,".docx")
              renderInlineCode(phys_temp, output)
              
              importFiles(rcon = pdb, file = output, record = record, field = "feedback_physician5_letter",event = events[e+1],
                          overwrite = TRUE, repeat_instance = 1)
    }}}}}
        
  }
  return(err)
}

