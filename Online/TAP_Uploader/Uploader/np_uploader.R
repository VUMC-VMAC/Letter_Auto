np_uploader<<- function(epochh,vmac) {
  
  library(ggplot2)
  library(plotly)
  library(redcapAPI)
  library(WordR)
  library(officer)
  library(Hmisc)
  library(tidyverse)
  library(flextable)
  library(png)
  library(readxl)
  
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
  moca_ex <- read_excel("/app/TAP_np_lookup.xlsx", sheet = "moca")
  moca_mean <- moca_ex[ind,1+sex_r]
  moca_sd <- moca_ex[ind,2+sex_r]
  
  craftvrs_ex <- read_excel("/app/TAP_np_lookup.xlsx", sheet = "craftvrs")
  craftvrs_mean <- craftvrs_ex[ind,1+sex_r]
  craftvrs_sd <- craftvrs_ex[ind,2+sex_r]
  
  crafturs_ex <- read_excel("/app/TAP_np_lookup.xlsx", sheet = "crafturs")
  crafturs_mean <- crafturs_ex[ind,1+sex_r]
  crafturs_sd <- crafturs_ex[ind,2+sex_r]
  
  craftdvr_ex <- read_excel("/app/TAP_np_lookup.xlsx", sheet = "craftdvr")
  craftdvr_mean <- craftdvr_ex[ind,1+sex_r]
  craftdvr_sd <- craftdvr_ex[ind,2+sex_r]
  
  craftdre_ex <- read_excel("/app/TAP_np_lookup.xlsx", sheet = "craftdre")
  craftdre_mean <- craftdre_ex[ind,1+sex_r]
  craftdre_sd <- craftdre_ex[ind,2+sex_r]
  
  udsbentc_ex <- read_excel("/app/TAP_np_lookup.xlsx", sheet = "udsbentc")
  udsbentc_mean <- udsbentc_ex[ind,1+sex_r]
  udsbentc_sd <- udsbentc_ex[ind,2+sex_r]
  
  udsbentd_ex <- read_excel("/app/TAP_np_lookup.xlsx", sheet = "udsbentd")
  udsbentd_mean <- udsbentd_ex[ind,1+sex_r]
  udsbentd_sd <- udsbentd_ex[ind,2+sex_r]
  
  digforct_ex <- read_excel("/app/TAP_np_lookup.xlsx", sheet = "digforct")
  digforct_mean <- digforct_ex[ind,1+sex_r]
  digforct_sd <- digforct_ex[ind,2+sex_r]
  
  digforsl_ex <- read_excel("/app/TAP_np_lookup.xlsx", sheet = "digforsl")
  digforsl_mean <- digforsl_ex[ind,1+sex_r]
  digforsl_sd <- digforsl_ex[ind,2+sex_r]
  
  digbacct_ex <- read_excel("/app/TAP_np_lookup.xlsx", sheet = "digbacct")
  digbacct_mean <- digbacct_ex[ind,1+sex_r]
  digbacct_sd <- digbacct_ex[ind,2+sex_r]
  
  digbacls_ex <- read_excel("/app/TAP_np_lookup.xlsx", sheet = "digbacls")
  digbacls_mean <- digbacls_ex[ind,1+sex_r]
  digbacls_sd <- digbacls_ex[ind,2+sex_r]
  
  minttots_ex <- read_excel("/app/TAP_np_lookup.xlsx", sheet = "minttots")
  minttots_mean <- minttots_ex[ind,1+sex_r]
  minttots_sd <- minttots_ex[ind,2+sex_r]
  
  udsverfc_ex <- read_excel("/app/TAP_np_lookup.xlsx", sheet = "udsverfc")
  udsverfc_mean <- udsverfc_ex[ind,1+sex_r]
  udsverfc_sd <- udsverfc_ex[ind,2+sex_r]
  
  udsverlc_ex <- read_excel("/app/TAP_np_lookup.xlsx", sheet = "udsverlc")
  udsverlc_mean <- udsverlc_ex[ind,1+sex_r]
  udsverlc_sd <- udsverlc_ex[ind,2+sex_r]
  
  udsvertn_ex <- read_excel("/app/TAP_np_lookup.xlsx", sheet = "udsvertn")
  udsvertn_mean <- udsvertn_ex[ind,1+sex_r]
  udsvertn_sd <- udsvertn_ex[ind,2+sex_r]
  
  animals_ex <- read_excel("/app/TAP_np_lookup.xlsx", sheet = "animals")
  animals_mean <- animals_ex[ind,1+sex_r]
  animals_sd <- animals_ex[ind,2+sex_r]
  
  veg_ex <- read_excel("/app/TAP_np_lookup.xlsx", sheet = "veg")
  veg_mean <- veg_ex[ind,1+sex_r]
  veg_sd <- veg_ex[ind,2+sex_r]
  
  traila_ex <- read_excel("/app/TAP_np_lookup.xlsx", sheet = "traila")
  traila_mean <- traila_ex[ind,1+sex_r]
  traila_sd <- traila_ex[ind,2+sex_r]
  
  trailb_ex <- read_excel("/app/TAP_np_lookup.xlsx", sheet = "trailb")
  trailb_mean <- trailb_ex[ind,1+sex_r]
  trailb_sd <- trailb_ex[ind,2+sex_r]
  
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
  animals_z <- (as.integer(animals)-as.integer(animals_mean)-1)/as.integer(animals_sd)
  veg_z <- (as.integer(veg)-as.integer(veg_mean)-1)/as.integer(veg_sd)
  traila_z <- (as.integer(traila)-as.integer(traila_mean)-1)/as.integer(traila_sd)
  trailb_z <- (as.integer(trailb)-as.integer(trailb_mean)-1)/as.integer(trailb_sd)
  
  mem_ver<- cbind(craftvrs_z,crafturs_z,craftdvr_z,craftdre_z)
  mem_vis<- cbind(udsbentc_z, udsbentd_z)
  lang<- cbind(minttots_z,animals_z,veg_z)
  ex_func<- cbind(digbacct_z, digbacls_z, udsverfc_z, udsverlc_z, udsvertn_z, trailb_z)
  attention<- cbind(digforct_z, digforsl_z, traila_z)
  
  dat <<- cbind(mem_ver,mem_vis,lang,ex_func,attention)

  return(dat)
 
}

