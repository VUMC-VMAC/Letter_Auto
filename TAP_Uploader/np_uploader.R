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
  
  ex_path <- paste0("~/Letter_Automation - Copy/epoch5dde_lookup.xlsx")
  
  np <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                         token = "79321D56C26DD34A7F03A318096D77F4", conn, project = 136221)
  pdb <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "0E16F65FB0A51C570781384D91AA1A78", conn, project = 137402)
  
  edc <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "A2F023358C81C065E1D98575795DCD5B", conn, project = 135160)
  edc_datas <- exportReports(edc, 280483)
  pdb_datas <- exportReports(pdb, 270431)
  np_datas <- exportReports(np, 280482)
  
  pdb_datas <- pdb_datas[which(as.integer(as.factor(pdb_datas[,"redcap_event_name"]))== epochh),]
  i <- which(pdb_datas["vmac_id"]==as.integer(vmac))
  pdb_data <- pdb_datas[i,]
  
  # NP Norm Scores
  np_datas <- np_datas[grep(vmac,np_datas$record_id),]
  
  np_data <- np_datas[grep("--1",np_datas$record_id),]
  
  edc_data <- edc_datas[which(edc_datas["vmac_id"]==as.integer(vmac)),]
 
  print("Compiling Memory results")
  
  age <- pdb_data$age
  if (age<60) {age_r <- 0}; if (age < 70 & age >= 60) {age_r <- 4}; if (age < 80 & age >= 70) {age_r <- 8}; if (age < 90 & age >= 80) {age_r <- 12}; if (age >= 90) {age_r <- 16}
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
  animals <<- np_data$animals
  veg <<- np_data$veg
  traila <<- np_data$traila
  trailb <<- np_data$trailb
  
  # GET MEAN and SD
  moca_ex <- read_excel("~/Letter_Automation - Copy/TAP_np_lookup.xlsx", sheet = "moca")
  moca_mean <- moca_ex[ind,1+sex_r]
  moca_sd <- moca_ex[ind,2+sex_r]
  
  craftvrs_ex <- read_excel("~/Letter_Automation - Copy/TAP_np_lookup.xlsx", sheet = "craftvrs")
  craftvrs_mean <- craftvrs_ex[ind,1+sex_r]
  craftvrs_sd <- craftvrs_ex[ind,2+sex_r]
  
  crafturs_ex <- read_excel("~/Letter_Automation - Copy/TAP_np_lookup.xlsx", sheet = "crafturs")
  crafturs_mean <- crafturs_ex[ind,1+sex_r]
  crafturs_sd <- crafturs_ex[ind,2+sex_r]
  
  craftdvr_ex <- read_excel("~/Letter_Automation - Copy/TAP_np_lookup.xlsx", sheet = "craftdvr")
  craftdvr_mean <- craftdvr_ex[ind,1+sex_r]
  craftdvr_sd <- craftdvr_ex[ind,2+sex_r]
  
  craftdre_ex <- read_excel("~/Letter_Automation - Copy/TAP_np_lookup.xlsx", sheet = "craftdre")
  craftdre_mean <- craftdre_ex[ind,1+sex_r]
  craftdre_sd <- craftdre_ex[ind,2+sex_r]
  
  udsbentc_ex <- read_excel("~/Letter_Automation - Copy/TAP_np_lookup.xlsx", sheet = "udsbentc")
  udsbentc_mean <- udsbentc_ex[ind,1+sex_r]
  udsbentc_sd <- udsbentc_ex[ind,2+sex_r]
  
  udsbentd_ex <- read_excel("~/Letter_Automation - Copy/TAP_np_lookup.xlsx", sheet = "udsbentd")
  udsbentd_mean <- udsbentd_ex[ind,1+sex_r]
  udsbentd_sd <- udsbentd_ex[ind,2+sex_r]
  
  digforct_ex <- read_excel("~/Letter_Automation - Copy/TAP_np_lookup.xlsx", sheet = "digforct")
  digforct_mean <- digforct_ex[ind,1+sex_r]
  digforct_sd <- digforct_ex[ind,2+sex_r]
  
  digforsl_ex <- read_excel("~/Letter_Automation - Copy/TAP_np_lookup.xlsx", sheet = "digforsl")
  digforsl_mean <- digforsl_ex[ind,1+sex_r]
  digforsl_sd <- digforsl_ex[ind,2+sex_r]
  
  digbacct_ex <- read_excel("~/Letter_Automation - Copy/TAP_np_lookup.xlsx", sheet = "digbacct")
  digbacct_mean <- digbacct_ex[ind,1+sex_r]
  digbacct_sd <- digbacct_ex[ind,2+sex_r]
  
  digbacls_ex <- read_excel("~/Letter_Automation - Copy/TAP_np_lookup.xlsx", sheet = "digbacls")
  digbacls_mean <- digbacls_ex[ind,1+sex_r]
  digbacls_sd <- digbacls_ex[ind,2+sex_r]
  
  minttots_ex <- read_excel("~/Letter_Automation - Copy/TAP_np_lookup.xlsx", sheet = "minttots")
  minttots_mean <- minttots_ex[ind,1+sex_r]
  minttots_sd <- minttots_ex[ind,2+sex_r]
  
  udsverfc_ex <- read_excel("~/Letter_Automation - Copy/TAP_np_lookup.xlsx", sheet = "udsverfc")
  udsverfc_mean <- udsverfc_ex[ind,1+sex_r]
  udsverfc_sd <- udsverfc_ex[ind,2+sex_r]
  
  udsverlc_ex <- read_excel("~/Letter_Automation - Copy/TAP_np_lookup.xlsx", sheet = "udsverlc")
  udsverlc_mean <- udsverlc_ex[ind,1+sex_r]
  udsverlc_sd <- udsverlc_ex[ind,2+sex_r]
  
  udsvertn_ex <- read_excel("~/Letter_Automation - Copy/TAP_np_lookup.xlsx", sheet = "udsvertn")
  udsvertn_mean <- udsvertn_ex[ind,1+sex_r]
  udsvertn_sd <- udsvertn_ex[ind,2+sex_r]
  
  animals_ex <- read_excel("~/Letter_Automation - Copy/TAP_np_lookup.xlsx", sheet = "animals")
  animals_mean <- animals_ex[ind,1+sex_r]
  animals_sd <- animals_ex[ind,2+sex_r]
  
  veg_ex <- read_excel("~/Letter_Automation - Copy/TAP_np_lookup.xlsx", sheet = "veg")
  veg_mean <- veg_ex[ind,1+sex_r]
  veg_sd <- veg_ex[ind,2+sex_r]
  
  traila_ex <- read_excel("~/Letter_Automation - Copy/TAP_np_lookup.xlsx", sheet = "traila")
  traila_mean <- traila_ex[ind,1+sex_r]
  traila_sd <- traila_ex[ind,2+sex_r]
  
  trailb_ex <- read_excel("~/Letter_Automation - Copy/TAP_np_lookup.xlsx", sheet = "trailb")
  trailb_mean <- trailb_ex[ind,1+sex_r]
  trailb_sd <- trailb_ex[ind,2+sex_r]
  
  # transform variables
  moca_z <- as.double((as.integer(np_moca_total)-moca_mean)/moca_sd)
  craftvrs_z <- as.double((as.integer(craftvrs)-craftvrs_mean-1)/craftvrs_sd)
  crafturs_z <- as.double((as.integer(crafturs)-crafturs_mean-1)/crafturs_sd)
  craftdvr_z <- as.double((as.integer(craftdvr)-craftdvr_mean-1)/craftdvr_sd)
  craftdre_z <- as.double((as.integer(craftdre)-craftdre_mean-1)/craftdre_sd)
  udsbentc_z <- as.double((as.integer(udsbentc)-udsbentc_mean-1)/udsbentc_sd)
  udsbentd_z <- as.double((as.integer(udsbentd)-udsbentd_mean-1)/udsbentd_sd)
  digforct_z <- as.double((as.integer(digforct)-digforct_mean-1)/digforct_sd)
  digforsl_z <- as.double((as.integer(digforsl)-digforsl_mean+1)/digforsl_sd)
  digbacct_z <- as.double((as.integer(digbacct)-digbacct_mean-1)/digbacct_sd)
  digbacls_z <- as.double((as.integer(digbacls)-digbacls_mean)/digbacls_sd)
  minttots_z <- as.double((as.integer(minttots)-minttots_mean-1)/minttots_sd)
  udsverfc_z <- as.double((as.integer(udsverfc)-udsverfc_mean-1)/udsverfc_sd)
  udsverlc_z <- as.double((as.integer(udsverlc)-udsverlc_mean-1)/udsverlc_sd)
  udsvertn_z <- as.double((as.integer(udsvertn)-udsvertn_mean-1)/udsvertn_sd)
  animals_z <- as.double((as.integer(animals)-animals_mean-1)/animals_sd)
  veg_z <- as.double((as.integer(veg)-veg_mean-1)/veg_sd)
  traila_z <- as.double((as.integer(traila)-traila_mean-1)/traila_sd)
  trailb_z <- as.double((as.integer(trailb)-trailb_mean-1)/trailb_sd)
  
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
  
  dat1 <<- cbind(mem_ver,mem_vis,lang,ex_func,attention,np_cvlt1to5_z,np_cvlt_ldfr_z,np_cvlt_sdfr_z,np_word_z,np_color_z,np_digsymb_z,np_inhibit_z,np_tower_z)
  dat2 <<- cbind(craftvrs,crafturs,craftdvr,craftdre,udsbentc, udsbentd,minttots,animals,veg,digbacct, digbacls, udsverfc, udsverlc, udsvertn, trailb,digforct, digforsl, traila,"np_cvlt1to5","np_cvlt_ldfr","np_cvlt_sdfr",np_word,np_color,np_digsymb,np_inhibit,np_tower)
  dat <<- rbind(dat1,dat2)

  return(dat)
 
}

