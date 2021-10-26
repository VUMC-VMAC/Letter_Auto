np_uploader<<- function(epochh,vmac) {
  
  library(ggplot2)
  library(plotly)
  library(redcapAPI)
  library(WordR)
  library(officer)
  library(Hmisc)
  library(tidyverse)
  library(flextable)
  library(ggpattern)
  library(patternplot)
  library(png)
  library(readxl)
  
  
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
                          token = "7E1DC8A562246EC4F7043579B863706C", conn, project = 136242)
  
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
  
  dde_data <- dde2_datas[which(dde2_datas["vmac_id"]==as.integer(vmac)),]
  record_id <- dde_data[1,"record_id"]
  map_id <- substr(record_id,1,3)
  
  edc_data <- edc_datas[which(edc_datas["map_id"]==as.integer(map_id)),]
  
  
  events <- c("eligibility_arm_1","enrollmentbaseline_arm_1","18month_followup_arm_1","3year_followup_arm_1","5year_followup_arm_1","7year_followup_arm_1")
  
  map_data <- pdb_datas[which(pdb_datas["vmac_id"]==as.integer(vmac)),]
  rev_data_frame <- apply(map_data, 2, rev)
  map_data <- as.data.frame(rev_data_frame)
  
  pdb_data <- map_data[which(map_data[,"redcap_event_name"]== events[epochh+1]),]
  
  map_data[,which(is.na(map_data[,"feedback_location"]))]<-"other"
  
  pdb_data <- map_data[which(map_data[,"redcap_event_name"]== events[epochh+1]),]
  
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
  if (nrow(tm7yr_data)==FALSE) {stop("Not Enough Data")}
  
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
  
  i<-1
  
 
  print("Compiling Memory results")
  
  np <- rbind(tme_data[-(1:17)],tm36_data[-(1:17)],tm60_data[-(1:17)],tm7yr_data[-(1:17)][-19])
  
  np_cvlt1to5_tscore <- np$np_cvlt1to5_tscore
  np_cvlt_sdfr_z <<- np$np_cvlt_sdfr_zscore
  np_cvlt_ldfr_z <<- np$np_cvlt_ldfr_zscore  
  np_cvltrecog_discrim_z <<- np$np_cvltrecog_discrim_zscore
  np_biber_t1to5_z <<- np$np_biber_t1to5_zscore
  np_biber_sd_z <<- np$np_biber_sd_zscore
  np_biber_ld_z <<- np$np_biber_ld_zscore
  np_anim_tscore <- np$np_anim_tscore
  np_bnt_z <<- np$np_bnt_zscore
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
  np_cvlt1to5_z <<- (np_cvlt1to5_tscore - 50)/10
  np_anim_z <<- (np_anim_tscore-50)/10
  np_tower_z <<- (np_tower_ss-10)/3
  np_inhibit_z <<- (np_inhibit_ss-10)/3
  np_fas_z <<- (np_fas_tscore-50)/10
  np_tmtb_z <<- (np_tmtb_ss-10)/3
  np_hvot_z <<- -(np_hvot_tscore-50)/10
  np_digsymb_z <<- (np_digsymb_ss-10)/3
  np_color_z <<- (np_color_ss-10)/3
  np_word_z <<- (np_word_ss-10)/3
  np_tmta_z <<- (np_tmta_ss-10)/3
  
  #### If Norm Scores are not available ####
  
  # CVLT
  if (any(is.na(np_cvlt1to5_z))) {
    np_cvlt1to5_tscore <- edc_data["np_cvlt1to5_tscore"]
    np_cvlt1to5_z[4] <- (np_cvlt1to5_tscore[[1]] - 50)/10
  }
  if (any(is.na(np_cvlt_sdfr_z))) {np_cvlt_sdfr_z[4] <- edc_data["np_cvlt_sdfr_zscore"]}
  if (any(is.na(np_cvlt_ldfr_z))) {np_cvlt_ldfr_z[4] <- edc_data["np_cvlt_ldfr_zscore"]}
  if (any(is.na(np_cvltrecog_discrim_z))) {np_cvltrecog_discrim_z[4] <- edc_data["np_cvltrecog_discrim_zscore"]}
  
  # Biber
  if (any(is.na(np_biber_t1to5_z))) {np_biber_t1to5 <- dde_data[2,"np_biber_t1to5"];np_biber_t1to5_z[4] <- (np_biber_t1to5 - 114.5) / 34.7}
  if (any(is.na(np_biber_sd_z))) {np_biber_sd <- dde_data[2,"np_biber_sd"];np_biber_sd_z[4] <- (np_biber_sd - 26.4) / 7}
  if (any(is.na(np_biber_ld_z))) {np_biber_ld <- dde_data[2,"np_biber_ld"];np_biber_ld_z[4] <- (np_biber_ld - 28) / 7}
  
  #Tower
  if (any(is.na(np_tower_z))) {
    age <- dde_data[2,"age"]
    
    np_tower1 <- as.integer(dde_data[2,"np_tower1"])-1
    np_tower2 <- as.integer(dde_data[2,"np_tower2"])-1
    np_tower3 <- as.integer(dde_data[2,"np_tower3"])-1
    np_tower4 <- as.integer(dde_data[2,"np_tower4"])-1
    np_tower5 <- as.integer(dde_data[2,"np_tower5"])-1
    np_tower6 <- as.integer(dde_data[2,"np_tower6"])-1
    np_tower7 <- as.integer(dde_data[2,"np_tower7"])-1
    np_tower8 <- as.integer(dde_data[2,"np_tower8"])-1
    np_tower9 <- as.integer(dde_data[2,"np_tower9"])-1
    np_tower <- sum(np_tower1,np_tower2,np_tower3,np_tower4,np_tower5,np_tower6,np_tower7,np_tower8,np_tower9)
    tower_ex <- read_excel("~/epoch5dde_lookup.xlsx", sheet = 1)
    np_tower_ss <- tower_ex[as.integer(ceiling(np_tower))+1,as.character(age)]
    np_tower_z[4] <- (as.integer(np_tower_ss)-10)/3
  }
  
  # Animal
  if (any(is.na(np_anim_z))) {
    age <- dde_data[2,"age"]
    if (age < 55) {age_r <- "50-54"}; if (age < 60 & age >= 55) {age_r <- "55-59"}; if (age < 65 & age >= 60) {age_r <- "60-64"}
    if (age < 70 & age >= 65) {age_r <- "65-69"}; if (age < 75 & age >= 70) {age_r <- "70-74"} 
    if (age < 80 & age >= 75) {age_r <- "75-79"}; if (age >= 80) {age_r <- "80-85"}
    edu <- dde_data[2,"education"]
    if (edu < 9) {edu_r <- "7-8"}; if (edu >= 9 & edu < 12 ) {edu_r <- "9-11"}; if (edu == 12 ) {edu_r <- "12"}
    if (edu >= 13 & edu < 16 ) {edu_r <- "13-15"}; if (edu >= 16 & edu < 18 ) {edu_r <- "16-17"}
    if (edu >= 18) {edu_r <- "18-20"}
    sex_conv <- c("M","F","missing","N/A")
    sex_r <- sex_conv[dde_data[2,"sex"]]
    race_conv <- c("C","AA","NA","A","O","M","N/A")
    race <- race_conv[dde_data[2,"race"]]
    
    np_anim_q1 <- dde_data[2,"np_anim_q1"]
    np_anim_q2 <- dde_data[2,"np_anim_q2"]
    np_anim_q3 <- dde_data[2,"np_anim_q3"]
    np_anim_q4 <- dde_data[2,"np_anim_q4"]
    np_anim <- sum(c(np_anim_q1, np_anim_q2, np_anim_q3, np_anim_q4))
    anim_ex <- read_excel("~/epoch5dde_lookup.xlsx", sheet = "heaton_scaled")
    np_anim_sscore <- anim_ex[as.integer(ceiling(np_anim))+1,4]
    anim_ex <- read_excel("~/epoch5dde_lookup.xlsx", sheet = "heaton_animals")
    var_anim <- paste0(sex_r,"/",edu_r,"/",age_r,"/",race)
    np_anim_tscore <- anim_ex[as.integer(ceiling(20-np_anim_sscore)),var_anim]
    np_anim_z[4] <- (as.integer(np_anim_tscore) - 50) / 10
  }
  
  if (any(is.na(np_bnt_z))) {
    np_bnt <- dde_data[2,"np_bnt"]
    np_bnt_z[4] <- (np_bnt - 26) / 3.4
  }
  
  if (any(is.na(np_inhibit_z))) {
    age <- dde_data[2,"age"]
    np_inhibit <- dde_data[2,"np_inhibit"]
    inhibit_ex <- read_excel("~/epoch5dde_lookup.xlsx", sheet = 12)
    np_inhibit_ss <- inhibit_ex[as.integer(ceiling(np_inhibit))+1,as.character(age)]
    np_inhibit_z[4] <- (as.integer(np_inhibit_ss)-10)/3
  }
  
  if (any(is.na(np_fas_z))) {
    age <- dde_data[2,"age"]
    if (age < 55) {age_r <- "50-54"}; if (age < 60 & age >= 55) {age_r <- "55-59"}; if (age < 65 & age >= 60) {age_r <- "60-64"}
    if (age < 70 & age >= 65) {age_r <- "65-69"}; if (age < 75 & age >= 70) {age_r <- "70-74"} 
    if (age < 80 & age >= 75) {age_r <- "75-79"}; if (age >= 80) {age_r <- "80-85"}
    edu <- dde_data[2,"education"]
    if (edu < 9) {edu_r <- "7-8"}; if (edu >= 9 & edu < 12 ) {edu_r <- "9-11"}; if (edu == 12 ) {edu_r <- "12"}
    if (edu >= 13 & edu < 16 ) {edu_r <- "13-15"}; if (edu >= 16 & edu < 18 ) {edu_r <- "16-17"}
    if (edu >= 18) {edu_r <- "18-20"}
    sex_conv <- c("M","F","missing","N/A")
    sex_r <- sex_conv[dde_data[2,"sex"]]
    race_conv <- c("C","AA","NA","A","O","M","N/A")
    race <- race_conv[dde_data[2,"race"]]
    
    np_fas <- dde_data[2,"np_fas"]
    fas_ex <- read_excel("~/epoch5dde_lookup.xlsx", sheet = "heaton_scaled")
    np_fas_sscore <- fas_ex[as.integer(ceiling(np_fas))+1,5]
    fas_ex <- read_excel("~/epoch5dde_lookup.xlsx", sheet = "heaton_fas")
    var_fas <- paste0(sex_r,"/",edu_r,"/",age_r,"/",race)
    np_fas_tscore <- fas_ex[as.integer(ceiling(20-np_fas_sscore)),var_fas]
    np_fas_z[4] <- (as.integer(np_fas_tscore) - 50) / 10 
  }
  
  if (any(is.na(np_tmtb_z))) {
    age <- dde_data[2,"age"]
    np_tmtb <- dde_data[2,"np_tmtb"]
    tmtb_ex <- read_excel("~/epoch5dde_lookup.xlsx", sheet = 6)
    np_tmtb_ss <- tmtb_ex[as.integer(ceiling(np_tmtb))+1,as.character(age)]
    np_tmtb_z[4] <- (as.integer(np_tmtb_ss) - 10)/3
  }
  
  if (any(is.na(np_tmta_z))) {
    age <- dde_data[2,"age"]
    
    np_tmta <- dde_data[2,"np_tmta"]
    tmta_ex <- read_excel("~/epoch5dde_lookup.xlsx", sheet = 3)
    np_tmta_ss <- tmta_ex[as.integer(ceiling(np_tmta))+1,as.character(age)]
    np_tmta_z[4] <- (as.integer(np_tmta_ss) - 10)/3
  }
  
  if (any(is.na(np_hvot_z))) {
    age <- dde_data[2,"age"]
    edu <- dde_data[2,"education"]
    np_hvot <- dde_data[2,"np_hvot"]
    if (age < 55) {hvot_ex <- read_excel("~/epoch5dde_lookup.xlsx", sheet = 22)}
    if (55 <= age | age < 60) {hvot_ex <- read_excel("~/epoch5dde_lookup.xlsx", sheet = 23)}
    if (60 <= age | age < 65) {hvot_ex <- read_excel("~/epoch5dde_lookup.xlsx", sheet = 24)}
    if (65 <= age) {hvot_ex <- read_excel("~/epoch5dde_lookup.xlsx", sheet = 25)}
    np_hvot_corrected <- hvot_ex[as.integer(ceiling(np_hvot))+1,as.integer(edu)+2]
    hvot_t_ex <- read_excel("~/epoch5dde_lookup.xlsx", sheet = 26)
    np_hvot_tscore <- hvot_t_ex[as.integer(np_hvot_corrected)+1,2]
    np_hvot_z[4] <- -(as.integer(np_hvot_tscore) - 50) / 10
  }
  
  if (any(is.na(np_digsymb_z))) {
    age <- dde_data[2,"age"]
    np_digsymb <- dde_data[2,"np_digsymb"]
    digsymb_ex <- read_excel("~/epoch5dde_lookup.xlsx", sheet = 21)
    np_digsymb_ss <- digsymb_ex[as.integer(ceiling(np_digsymb))+1,as.character(age)]
    np_digsymb_z[4] <- (as.integer(np_digsymb_ss)-10)/3
  }
  
  if (any(is.na(np_color_z))) {
    age <- dde_data[2,"age"]
    
    np_color <- dde_data[2,"np_color"]
    color_ex <- read_excel("~/epoch5dde_lookup.xlsx", sheet = 10)
    np_color_ss <- color_ex[as.integer(ceiling(np_color))+1,as.character(age)]
    np_color_z[4] <- (as.integer(np_color_ss)-10)/3
  }
  
  if (any(is.na(np_word_z))) {
    age <- dde_data[2,"age"]
    
    np_word <- dde_data[2,"np_word"]
    word_ex <- read_excel("~/epoch5dde_lookup.xlsx", sheet = 11)
    np_word_ss <- word_ex[as.integer(ceiling(np_word))+1,as.character(age)]
    np_word_z[4] <- (as.integer(np_word_ss)-10)/3
  }
  
  dat <<- cbind(np_anim_z,np_biber_ld_z,np_biber_sd_z,np_biber_t1to5_z,np_bnt_z,np_color_z,np_cvlt1to5_z,np_cvlt_ldfr_z,np_cvlt_sdfr_z,np_cvltrecog_discrim_z,np_digsymb_z,np_fas_z,np_hvot_z,np_inhibit_z,np_tmta_z,np_tmtb_z,np_tower_z,np_word_z)

  return(dat)
 
}
