np_uploader<<- function(epochh,vmac) {
  
  library(ggplot2)
  library(redcapAPI)
  library(Hmisc)
  library(tidyverse)
  library(flextable)
  library(readxl)
  
  
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
  
  ex_path <- paste0(main_path,"epoch5dde_lookup.xlsx")
  
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
  
  events <- c("eligibility_arm_1","enrollmentbaseline_arm_1","18month_followup_arm_1","3year_followup_arm_1","5year_followup_arm_1","7year_followup_arm_1")
  
  map_data <- pdb_datas[which(pdb_datas["vmac_id"]==as.integer(vmac)),]
  if (nrow(map_data) > 1) {
    rev_data_frame <- apply(map_data, 2, rev)
    map_data <- as.data.frame(rev_data_frame)
    pdb_data <- map_data[which(map_data[,"redcap_event_name"]== events[epochh+1]),]
  } else {
    pdb_data <- map_data
  }
  
  map_id <- as.character(pdb_data$map_id)
  if (nchar(map_id)==1) {input<<- paste0("00",map_id)} else if (nchar(map_id)==2) {input<<- paste0("0",map_id)} else {input<<- map_id}
  
  dde_datas <- exportReports(dde, 275632)
  dde2_datas <- exportReports(dde2, 277386)
  edc_datas <- exportReports(EDC, 275637)
  
  dde_data <- dde2_datas[grep(input,dde2_datas$record_id),]
  ij <- grep("--1",dde_data$record_id)
  
  dde_data[dde_data == "-9999" | dde_data == "-8888" | dde_data == "-7777" | is.na(dde_data)] <- NA
  
  record_id <- dde_data[1,"record_id"]
  
  edc_datas <- edc_datas[which(edc_datas$map_id==as.integer(map_id)),]
  
  #map_data[,which(is.na(map_data[,"feedback_location"]))]<-"other"
  
  edc_data <- edc_datas[which(edc_datas$redcap_event_name == events[epochh+1]),]
  
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
  
  
  print("Compiling Memory results")
  
  age <- pdb_data$age
  if (age < 55) {age_r <- "50-54"}; if (age < 60 & age >= 55) {age_r <- "55-59"}; if (age < 65 & age >= 60) {age_r <- "60-64"}
  if (age < 70 & age >= 65) {age_r <- "65-69"}; if (age < 75 & age >= 70) {age_r <- "70-74"} 
  if (age < 80 & age >= 75) {age_r <- "75-79"}; if (age >= 80) {age_r <- "80-85"}
  
  ra <- pdb_data$race
  if (ra != "White/Caucasian" & ra != "African American/Black") {ra <- pdb_data$np_norm_race}
  r_conv <- c("White/Caucasian" = "C","African American/Black" = "AA")
  race <- r_conv[ra]
  
  sex <- pdb_data$sex
  s_conv <- c("Male" = "M", "Female" = "F")
  sex_r <- s_conv[sex]
  
  edu <- pdb_data$education
  if (edu < 9) {edu_r <- "7-8"}; if (edu >= 9 & edu < 12 ) {edu_r <- "9-11"}; if (edu == 12 ) {edu_r <- "12"}
  if (edu >= 13 & edu < 16 ) {edu_r <- "13-15"}; if (edu >= 16 & edu < 18 ) {edu_r <- "16-17"}
  if (edu >= 18) {edu_r <- "18-20"}
  
  if (e == 1) {
    edc_elig <- edc_datas[which(edc_datas$redcap_event_name == events[epochh]),]
    gds <- edc_elig$gds_total_score
  } else {gds <- edc_data$gds_total_score}
  
  if (e == 1) {
    # CVLT
    np_cvlt1to5_tscore <- edc_data["np_cvlt1to5_tscore"]
    np_cvlt1to5_z <- (np_cvlt1to5_tscore[[1]] - 50)/10
    
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
    var_anim <- paste0(sex_r,"/",edu_r,"/",age_r,"/",race)
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
    var_fas <- paste0(sex_r,"/",edu_r,"/",age_r,"/",race)
    np_fas_tscore <- fas_ex[as.integer(ceiling(20-np_fas_sscore)),var_fas]
    np_fas_z <- (as.integer(np_fas_tscore) - 50) / 10 
    
    
    np_tmtb <- dde_data[ij,"np_tmtb"]
    tmtb_ex <- read_excel(ex_path, sheet = 6)
    np_tmtb_ss <- tmtb_ex[as.integer(ceiling(np_tmtb))+1,as.character(age)]
    np_tmtb_z <- (as.integer(np_tmtb_ss) - 10)/3
    
    
    np_tmta <- dde_data[ij,"np_tmta"]
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
    
    
    dat1 <<- cbind(ra,gds,np_anim_z,np_biber_ld_z,np_biber_sd_z,np_biber_t1to5_z,np_bnt_z,np_color_z,np_cvlt1to5_z,np_cvlt_ldfr_z,np_cvlt_sdfr_z,np_cvltrecog_discrim_z,np_digsymb_z,np_fas_z,np_hvot_z,np_inhibit_z,np_tmta_z,np_tmtb_z,np_tower_z,np_word_z)
    dat2 <<- cbind("NA","NA",np_anim,np_biber_ld,np_biber_sd,np_biber_t1to5,np_bnt,np_color,"np_cvlt1to5","np_cvlt_ldfr","np_cvlt_sdfr","np_cvltrecog_discrim",np_digsymb,np_fas,np_hvot,np_inhibit,np_tmta,np_tmtb,np_tower,np_word)
    dat <<- rbind(dat1,as.character(dat2))
  }
  
  if (e > 1) {
    
    e1 <- which(echo_datas$redcap_event_name == events[2]); e3 <- which(map_data$redcap_event_name == events[2])
    p21 <- which(echo_datas$redcap_event_name == events[e-1]); p23 <- which(map_data$redcap_event_name == events[e-1])
    p1 <- which(echo_datas$redcap_event_name == events[e]); p3 <- which(map_data$redcap_event_name == events[e])
    
    dog <- cbind(pdb_data,edc_data)
    dog_e <- cbind(map_data[e3,],edc_datas[e2,])
    dog_p2 <- cbind(map_data[p23,],edc_datas[p22,])
    dog_p <- cbind(map_data[p3,],edc_datas[p2,])
    dog_m <- rbind(dog_e,dog_p2,dog_p,dog)
    
    i<-1
    
    print("Compiling Memory results")
    
    np_cvlt1to5_tscore <- dog_m$np_cvlt1to5_tscore
    np_cvlt_sdfr_z <- dog_m$np_cvlt_sdfr_zscore
    np_cvlt_ldfr_z <- dog_m$np_cvlt_ldfr_zscore  
    np_cvltrecog_discrim_z <- dog_m$np_cvltrecog_discrim_zscore
    np_biber_t1to5_z <- dog_m$np_biber_t1to5_zscore
    np_biber_sd_z <- dog_m$np_biber_sd_zscore
    np_biber_ld_z <- dog_m$np_biber_ld_zscore
    np_anim_tscore <- dog_m$np_anim_tscore
    np_bnt_z <- dog_m$np_bnt_zscore
    np_tower_ss <- dog_m$np_tower_sscore
    np_inhibit_ss <- dog_m$np_inhibit_sscore
    np_fas_tscore <- dog_m$np_fas_tscore
    np_tmtb_ss <- dog_m$np_tmtb_sscore
    np_hvot_tscore <- dog_m$np_hvot_tscore
    np_digsymb_ss <- dog_m$np_digsymb_sscore
    np_color_ss <- dog_m$np_color_sscore
    np_word_ss <- dog_m$np_word_sscore
    np_tmta_ss <- dog_m$np_tmta_sscore
    
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
    
    #### If Norm Scores are not available ####
    
    # CVLT
    np_cvlt1to5_tscore <- edc_data["np_cvlt1to5_tscore"]
    if (any(is.na(np_cvlt1to5_z))) {
      np_cvlt1to5_tscore <- edc_data["np_cvlt1to5_tscore"]
      np_cvlt1to5_z[4] <- (np_cvlt1to5_tscore[[1]] - 50)/10
    }
    if (any(is.na(np_cvlt_sdfr_z))) {np_cvlt_sdfr_z[4] <- edc_data["np_cvlt_sdfr_zscore"]}
    if (any(is.na(np_cvlt_ldfr_z))) {np_cvlt_ldfr_z[4] <- edc_data["np_cvlt_ldfr_zscore"]}
    if (any(is.na(np_cvltrecog_discrim_z))) {np_cvltrecog_discrim_z[4] <- edc_data["np_cvltrecog_discrim_zscore"]}
    
    # Biber
    np_biber_t1to5 <- dde_data[ij,"np_biber_t1to5"]
    if (any(is.na(np_biber_t1to5_z))) {np_biber_t1to5 <- dde_data[ij,"np_biber_t1to5"];np_biber_t1to5_z[4] <- (np_biber_t1to5 - 114.5) / 34.7}
    np_biber_sd <- dde_data[ij,"np_biber_sd"]
    if (any(is.na(np_biber_sd_z))) {np_biber_sd <- dde_data[ij,"np_biber_sd"];np_biber_sd_z[4] <- (np_biber_sd - 26.4) / 7}
    np_biber_ld <- dde_data[ij,"np_biber_ld"]
    if (any(is.na(np_biber_ld_z))) {np_biber_ld <- dde_data[ij,"np_biber_ld"];np_biber_ld_z[4] <- (np_biber_ld - 28) / 7}
    
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
    if (any(is.na(np_tower_z))) {
      tower_ex <- read_excel(ex_path, sheet = 1)
      np_tower_ss <- tower_ex[as.integer(ceiling(np_tower))+1,as.character(age)]
      np_tower_z[4] <- (as.integer(np_tower_ss)-10)/3
    }
    
    # Animal
    np_anim_q1 <- dde_data[ij,"np_anim_q1"]
    np_anim_q2 <- dde_data[ij,"np_anim_q2"]
    np_anim_q3 <- dde_data[ij,"np_anim_q3"]
    np_anim_q4 <- dde_data[ij,"np_anim_q4"]
    np_anim <- sum(c(np_anim_q1, np_anim_q2, np_anim_q3, np_anim_q4))
    if (any(is.na(np_anim_z))) {
      anim_ex <- read_excel(ex_path, sheet = "heaton_scaled")
      np_anim_sscore <- anim_ex[as.integer(ceiling(np_anim))+1,4]
      anim_ex <- read_excel(ex_path, sheet = "heaton_animals")
      var_anim <- paste0(sex_r,"/",edu_r,"/",age_r,"/",race)
      np_anim_tscore <- anim_ex[as.integer(ceiling(20-np_anim_sscore)),var_anim]
      np_anim_z[4] <- (as.integer(np_anim_tscore) - 50) / 10
    }
    
    np_bnt <- dde_data[ij,"np_bnt"]
    if (any(is.na(np_bnt_z))) {
      np_bnt_z[4] <- (np_bnt - 26) / 3.4
    }
    
    np_inhibit <- dde_data[ij,"np_inhibit"]
    if (any(is.na(np_inhibit_z))) {
      inhibit_ex <- read_excel(ex_path, sheet = 12)
      np_inhibit_ss <- inhibit_ex[as.integer(ceiling(np_inhibit))+1,as.character(age)]
      np_inhibit_z[4] <- (as.integer(np_inhibit_ss)-10)/3
    }
    
    np_fas <- dde_data[ij,"np_fas"]
    if (any(is.na(np_fas_z))) {
      fas_ex <- read_excel(ex_path, sheet = "heaton_scaled")
      np_fas_sscore <- fas_ex[as.integer(ceiling(np_fas))+1,5]
      fas_ex <- read_excel(ex_path, sheet = "heaton_fas")
      var_fas <- paste0(sex_r,"/",edu_r,"/",age_r,"/",race)
      np_fas_tscore <- fas_ex[as.integer(ceiling(20-np_fas_sscore)),var_fas]
      np_fas_z[4] <- (as.integer(np_fas_tscore) - 50) / 10 
    }
    
    np_tmtb <- dde_data[ij,"np_tmtb"]
    if (any(is.na(np_tmtb_z))) {
      tmtb_ex <- read_excel(ex_path, sheet = 6)
      np_tmtb_ss <- tmtb_ex[as.integer(ceiling(np_tmtb))+1,as.character(age)]
      np_tmtb_z[4] <- (as.integer(np_tmtb_ss) - 10)/3
    }
    
    np_tmta <- dde_data[ij,"np_tmta"]
    if (any(is.na(np_tmta_z))) {
      tmta_ex <- read_excel(ex_path, sheet = 3)
      np_tmta_ss <- tmta_ex[as.integer(ceiling(np_tmta))+1,as.character(age)]
      np_tmta_z[4] <- (as.integer(np_tmta_ss) - 10)/3
    }
    
    np_hvot <- dde_data[ij,"np_hvot"]
    if (any(is.na(np_hvot_z))) {
      if (age < 55) {hvot_ex <- read_excel(ex_path, sheet = 22)}
      if (55 <= age | age < 60) {hvot_ex <- read_excel(ex_path, sheet = 23)}
      if (60 <= age | age < 65) {hvot_ex <- read_excel(ex_path, sheet = 24)}
      if (65 <= age) {hvot_ex <- read_excel(ex_path, sheet = 25)}
      np_hvot_corrected <- hvot_ex[as.integer(ceiling(np_hvot))+1,as.integer(edu)+2]
      hvot_t_ex <- read_excel(ex_path, sheet = 26)
      np_hvot_tscore <- hvot_t_ex[as.integer(np_hvot_corrected)+1,2]
      np_hvot_z[4] <- -(as.integer(np_hvot_tscore) - 50) / 10
    }
    
    np_digsymb <- dde_data[ij,"np_digsymb"]
    if (any(is.na(np_digsymb_z))) {
      digsymb_ex <- read_excel(ex_path, sheet = 21)
      np_digsymb_ss <- digsymb_ex[as.integer(ceiling(np_digsymb))+1,as.character(age)]
      np_digsymb_z[4] <- (as.integer(np_digsymb_ss)-10)/3
    }
    
    np_color <- dde_data[ij,"np_color"]
    if (any(is.na(np_color_z))) {
      color_ex <- read_excel(ex_path, sheet = 10)
      np_color_ss <- color_ex[as.integer(ceiling(np_color))+1,as.character(age)]
      np_color_z[4] <- (as.integer(np_color_ss)-10)/3
    }
    
    np_word <- dde_data[ij,"np_word"]
    if (any(is.na(np_word_z))) {
      word_ex <- read_excel(ex_path, sheet = 11)
      np_word_ss <- word_ex[as.integer(ceiling(np_word))+1,as.character(age)]
      np_word_z[4] <- (as.integer(np_word_ss)-10)/3
    }
    
    
    dat1 <<- cbind(ra,dog_m$gds_total_score,np_anim_z,np_biber_ld_z,np_biber_sd_z,np_biber_t1to5_z,np_bnt_z,np_color_z,np_cvlt1to5_z,np_cvlt_ldfr_z,np_cvlt_sdfr_z,np_cvltrecog_discrim_z,np_digsymb_z,np_fas_z,np_hvot_z,np_inhibit_z,np_tmta_z,np_tmtb_z,np_tower_z,np_word_z)
    dat2 <<- cbind("NA","NA",np_anim,np_biber_ld,np_biber_sd,np_biber_t1to5,np_bnt,np_color,"np_cvlt1to5","np_cvlt_ldfr","np_cvlt_sdfr","np_cvltrecog_discrim",np_digsymb,np_fas,np_hvot,np_inhibit,np_tmta,np_tmtb,np_tower,np_word)
    dat <<- rbind(dat1,dat2)
    colnames(dat)[1:2] <- c("Race","GDS")
    
  }
  
  return(dat)
  
}

