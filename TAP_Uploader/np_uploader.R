np_uploader<<- function(epochh,vmac) {
  
  library(ggplot2)
  library(plotly)
  library(redcapAPI)
  library(WordR)
  library(officer)
  library(Hmisc)
  library(tidyverse)
  library(flextable)
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
  tap_ex_path <- paste0(main_path,"TAP_np_lookup.xlsx")
  
  np <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                         token = "79321D56C26DD34A7F03A318096D77F4", conn, project = 136221)
  pdb <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "0E16F65FB0A51C570781384D91AA1A78", conn, project = 137402)
  pdb_datas <- exportReports(pdb, 270431)
  np_datas <- exportReports(np, 280482)
  
  pdb_datas <- pdb_datas[which(as.integer(as.factor(pdb_datas[,"redcap_event_name"]))== epochh),]
  i <- which(pdb_datas["vmac_id"]==as.integer(vmac))
  pdb_data <- pdb_datas[i,]
  
  # NP Norm Scores
  np_datas <- np_datas[grep(vmac,np_datas$record_id),]
  
  np_data <- np_datas[grep("--1",np_datas$record_id),]
 
  print("Compiling Memory results")
  
  age <- pdb_data$age
  if (age<60) {age_r <- 1}; if (age < 70 & age >= 60) {age_r <- 2}; if (age < 80 & age >= 70) {age_r <- 3}; if (age < 90 & age >= 80) {age_r <- 4}; if (age >= 90) {age_r <- 5}
  edu <- pdb_data$education
  if (edu<=12) {edu_r <- 1}; if (edu < 16 & edu >= 13) {edu_r <- 2}; if (edu == 16) {edu_r <- 3}; if (edu >= 17) {edu_r <- 4}
  ind <- age_r + edu_r
  sex <- pdb_data$sex
  if (sex=="Male") {sex_r <- 0} else {sex_r <- 2}
  
  np_moca_total <<- np_data$np_moca_total
  craftvrs <<- np_data$craftvrs
  crafturs <<- np_data$crafturs
  craftdvr <<- np_data$craftdvr
  craftdre <<- np_data$craftdre
  udsbentc <<- np_data$udsbentc
  udsbentd <<- np_data$udsbentd
  digforct <<- np_data$digforct
  digforsl <<- np_data$digforsl
  digbacct <<- np_data$digbacct
  digbacls <<- np_data$digbacls
  minttots <<- np_data$minttots
  udsverfc <<- np_data$udsverfc
  udsverlc <<- np_data$udsverlc
  udsvertn <<- np_data$udsvertn
  animals_c2 <<- np_data$animals_c2
  veg_c2 <<- np_data$veg_c2
  traila_c2 <<- np_data$traila_c2
  trailb_c2 <<- np_data$trailb_c2
  
  # GET MEAN and SD
  moca_ex <- read_excel(tap_ex_path, sheet = "moca")
  moca_mean <- moca_ex[ind,1+sex_r]
  moca_sd <- moca_ex[ind,2+sex_r]
  
  craftvrs_ex <- read_excel(tap_ex_path, sheet = "craftvrs")
  craftvrs_mean <- craftvrs_ex[ind,1+sex_r]
  craftvrs_sd <- craftvrs_ex[ind,2+sex_r]
  
  crafturs_ex <- read_excel(tap_ex_path, sheet = "crafturs")
  crafturs_mean <- crafturs_ex[ind,1+sex_r]
  crafturs_sd <- crafturs_ex[ind,2+sex_r]
  
  craftdvr_ex <- read_excel(tap_ex_path, sheet = "craftdvr")
  craftdvr_mean <- craftdvr_ex[ind,1+sex_r]
  craftdvr_sd <- craftdvr_ex[ind,2+sex_r]
  
  craftdre_ex <- read_excel(tap_ex_path, sheet = "craftdre")
  craftdre_mean <- craftdre_ex[ind,1+sex_r]
  craftdre_sd <- craftdre_ex[ind,2+sex_r]
  
  udsbentc_ex <- read_excel(tap_ex_path, sheet = "udsbentc")
  udsbentc_mean <- udsbentc_ex[ind,1+sex_r]
  udsbentc_sd <- udsbentc_ex[ind,2+sex_r]
  
  udsbentd_ex <- read_excel(tap_ex_path, sheet = "udsbentd")
  udsbentd_mean <- udsbentd_ex[ind,1+sex_r]
  udsbentd_sd <- udsbentd_ex[ind,2+sex_r]
  
  digforct_ex <- read_excel(tap_ex_path, sheet = "digforct")
  digforct_mean <- digforct_ex[ind,1+sex_r]
  digforct_sd <- digforct_ex[ind,2+sex_r]
  
  digforsl_ex <- read_excel(tap_ex_path, sheet = "digforsl")
  digforsl_mean <- digforsl_ex[ind,1+sex_r]
  digforsl_sd <- digforsl_ex[ind,2+sex_r]
  
  digbacct_ex <- read_excel(tap_ex_path, sheet = "digbacct")
  digbacct_mean <- digbacct_ex[ind,1+sex_r]
  digbacct_sd <- digbacct_ex[ind,2+sex_r]
  
  digbacls_ex <- read_excel(tap_ex_path, sheet = "digbacls")
  digbacls_mean <- digbacls_ex[ind,1+sex_r]
  digbacls_sd <- digbacls_ex[ind,2+sex_r]
  
  minttots_ex <- read_excel(tap_ex_path, sheet = "minttots")
  minttots_mean <- minttots_ex[ind,1+sex_r]
  minttots_sd <- minttots_ex[ind,2+sex_r]
  
  udsverfc_ex <- read_excel(tap_ex_path, sheet = "udsverfc")
  udsverfc_mean <- udsverfc_ex[ind,1+sex_r]
  udsverfc_sd <- udsverfc_ex[ind,2+sex_r]
  
  udsverlc_ex <- read_excel(tap_ex_path, sheet = "udsverlc")
  udsverlc_mean <- udsverlc_ex[ind,1+sex_r]
  udsverlc_sd <- udsverlc_ex[ind,2+sex_r]
  
  udsvertn_ex <- read_excel(tap_ex_path, sheet = "udsvertn")
  udsvertn_mean <- udsvertn_ex[ind,1+sex_r]
  udsvertn_sd <- udsvertn_ex[ind,2+sex_r]
  
  animals_c2_ex <- read_excel(tap_ex_path, sheet = "animals_c2")
  animals_c2_mean <- animals_c2_ex[ind,1+sex_r]
  animals_c2_sd <- animals_c2_ex[ind,2+sex_r]
  
  veg_c2_ex <- read_excel(tap_ex_path, sheet = "veg_c2")
  veg_c2_mean <- veg_c2_ex[ind,1+sex_r]
  veg_c2_sd <- veg_c2_ex[ind,2+sex_r]
  
  traila_c2_ex <- read_excel(tap_ex_path, sheet = "traila_c2")
  traila_c2_mean <- traila_c2_ex[ind,1+sex_r]
  traila_c2_sd <- traila_c2_ex[ind,2+sex_r]
  
  trailb_c2_ex <- read_excel(tap_ex_path, sheet = "trailb_c2")
  trailb_c2_mean <- trailb_c2_ex[ind,1+sex_r]
  trailb_c2_sd <- trailb_c2_ex[ind,2+sex_r]
  
  # transform variables
  moca_z <- (as.integer(np_moca_total)-as.integer(moca_mean))/as.integer(moca_sd)
  craftvrs_z <- (as.integer(craftvrs)-as.integer(craftvrs_mean)-1)/as.integer(craftvrs_sd)
  crafturs_z <- (as.integer(crafturs)-as.integer(crafturs_mean)-1)/as.integer(crafturs_sd)
  craftdvr_z <- (as.integer(craftdvr)-as.integer(craftdvr_mean)-1)/as.integer(craftdvr_sd)
  craftdre_z <- (as.integer(craftdre)-as.integer(craftdre_mean)-1)/as.integer(craftdre_sd)
  udsbentc_z <- (as.integer(udsbentc)-as.integer(udsbentc_mean)-1)/as.integer(udsbentc_sd)
  udsbentd_z <- (as.integer(udsbentd)-as.integer(udsbentd_mean)-1)/as.integer(udsbentd_sd)
  digforct_z <- (as.integer(digforct)-as.integer(digforct_mean)-1)/as.integer(digforct_sd)
  digforsl_z <- (as.integer(digforsl)-as.integer(digforsl_mean)+1)/as.integer(digforsl_sd)
  digbacct_z <- (as.integer(digbacct)-as.integer(digbacct_mean)-1)/as.integer(digbacct_sd)
  digbacls_z <- (as.integer(digbacls)-as.integer(digbacls_mean))/as.integer(digbacls_sd)
  minttots_z <- (as.integer(minttots)-as.integer(minttots_mean)-1)/as.integer(minttots_sd)
  udsverfc_z <- (as.integer(udsverfc)-as.integer(udsverfc_mean)-1)/as.integer(udsverfc_sd)
  udsverlc_z <- (as.integer(udsverlc)-as.integer(udsverlc_mean)-1)/as.integer(udsverlc_sd)
  udsvertn_z <- (as.integer(udsvertn)-as.integer(udsvertn_mean)-1)/as.integer(udsvertn_sd)
  animals_c2_z <- (as.integer(animals_c2)-as.integer(animals_c2_mean)-1)/as.integer(animals_c2_sd)
  veg_c2_z <- (as.integer(veg_c2)-as.integer(veg_c2_mean)-1)/as.integer(veg_c2_sd)
  traila_c2_z <- (as.integer(traila_c2)-as.integer(traila_c2_mean)-1)/as.integer(traila_c2_sd)
  trailb_c2_z <- (as.integer(trailb_c2)-as.integer(trailb_c2_mean)-1)/as.integer(trailb_c2_sd)
  
  mem_ver<- cbind(craftvrs_z,crafturs_z,craftdvr_z,craftdre_z)
  mem_vis<- cbind(udsbentc_z, udsbentd_z)
  lang<- cbind(minttots_z,animals_c2_z,veg_c2_z)
  ex_func<- cbind(digbacct_z, digbacls_z, udsverfc_z, udsverlc_z, udsvertn_z, trailb_c2_z)
  attention<- cbind(digforct_z, digforsl_z, traila_c2_z)
  
  #Tower
  np_tower1 <- as.integer(np_data$np_tower1)-1
  np_tower2 <- as.integer(np_data$np_tower2)-1
  np_tower3 <- as.integer(np_data$np_tower3)-1
  np_tower4 <- as.integer(np_data$np_tower4)-1
  np_tower5 <- as.integer(np_data$np_tower5)-1
  np_tower6 <- as.integer(np_data$np_tower6)-1
  np_tower7 <- as.integer(np_data$np_tower7)-1
  np_tower8 <- as.integer(np_data$np_tower8)-1
  np_tower9 <- as.integer(np_data$np_tower9)-1
  np_tower <- sum(np_tower1,np_tower2,np_tower3,np_tower4,np_tower5,np_tower6,np_tower7,np_tower8,np_tower9)
  tower_ex <- read_excel(ex_path, sheet = 1)
  np_tower_ss <- tower_ex[as.integer(ceiling(np_tower))+1,as.character(age)]
  np_tower_z <- (as.integer(np_tower_ss)-10)/3
  
  np_inhibit <- np_data$np_inhibit
  inhibit_ex <- read_excel(ex_path, sheet = 12)
  np_inhibit_ss <- inhibit_ex[as.integer(ceiling(np_inhibit))+1,as.character(age)]
  np_inhibit_z <- (as.integer(np_inhibit_ss)-10)/3
  
  np_digsymb <- np_data$np_digsymb
  digsymb_ex <- read_excel(ex_path, sheet = 21)
  np_digsymb_ss <- digsymb_ex[as.integer(ceiling(np_digsymb))+1,as.character(age)]
  np_digsymb_z <- (as.integer(np_digsymb_ss)-10)/3
  
  np_color <- np_data$np_color
  color_ex <- read_excel(ex_path, sheet = 10)
  np_color_ss <- color_ex[as.integer(ceiling(np_color))+1,as.character(age)]
  np_color_z <- (as.integer(np_color_ss)-10)/3
  
  np_word <- np_data$np_word
  word_ex <- read_excel(ex_path, sheet = 11)
  np_word_ss <- word_ex[as.integer(ceiling(np_word))+1,as.character(age)]
  np_word_z <- (as.integer(np_word_ss)-10)/3
  
  np_cvlt1to5_tscore <- edc_data$np_cvlt1to5_tscore
  np_cvlt1to5_z <- (np_cvlt1to5_tscore[[1]] - 50)/10
  
  np_cvlt_sdfr_z <- edc_data$np_cvlt_sdfr_z
  np_cvlt_ldfr_z <- edc_data$np_cvlt_ldfr_z
  
  
  mem_ver<- cbind(craftvrs_z,crafturs_z,craftdvr_z,craftdre_z)
  mem_vis<- cbind(udsbentc_z, udsbentd_z)
  lang<- cbind(minttots_z,animals_z,veg_z)
  ex_func<- cbind(digbacct_z, digbacls_z, udsverfc_z, udsverlc_z, udsvertn_z, trailb_z)
  attention<- cbind(digforct_z, digforsl_z, traila_z)
  
  dat <<- cbind(mem_ver,mem_vis,lang,ex_func,attention,np_cvlt1to5_z,np_cvlt_ldfr_z,np_cvlt_sdfr_z,np_word_z,np_color_z,np_digsymb_z,np_inhibit_z,np_tower_z)

  return(dat)
 
}

