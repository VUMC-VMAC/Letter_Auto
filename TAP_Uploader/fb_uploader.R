fb_uploader<<- function(epochh,vmac) {
  
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
  
  edc <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                         token = "A2F023358C81C065E1D98575795DCD5B", conn, project = 135160)
  np <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "79321D56C26DD34A7F03A318096D77F4", conn, project = 136221)
  pdb <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "0E16F65FB0A51C570781384D91AA1A78", conn, project = 137402)
  pdb_datas <- exportReports(pdb, 270431)
  np_datas <- exportReports(np, 280482)
  edc_datas <- exportReports(edc, 280483)
  
  #pdb_data[which(is.na(pdb_data[,"proxy_diff_address"])),"proxy_diff_address"]<- "No"
  
  events <- c("enrollmentbaseline_arm_1","18month_followup_arm_1","3year_followup_arm_1","5year_followup_arm_1","7year_followup_arm_1")
  
  pdb_datas <- pdb_datas[which(as.integer(as.factor(pdb_datas[,"redcap_event_name"]))== epochh),]
  
  
  i <- which(pdb_datas["vmac_id"]==as.integer(vmac))
  
  pdb_data <- pdb_datas[i,]
  
  #pdb_data[,which(is.na(pdb_data[,"feedback_location"]))]<-"other"
  
  edc_data <- edc_datas[which(edc_datas["vmac_id"]==as.integer(vmac)),]
  
  np_datas <- np_datas[grep(vmac,np_datas$record_id),]
  
  np_data <- np_datas[grep("--1",np_datas$record_id),]
  
  e <- epochh
  Epoch_conv <- c("Enrollment","18-Month","3-Year","5-Year","7-Year","9-Year","11-Year","13-Year")
  Epoc_conv <- c("enrollment","18-month","3-year","5-year","7-year","9-year","11-year","13-year")
  Epoch <<- Epoch_conv[e]; Epoc <<- Epoc_conv[e]
  Epoch2 <<- Epoch_conv[e-2]; Epoc2 <<- Epoc_conv[e-2]
  Epoch1 <<- Epoch_conv[e-1]; Epoc1 <<- Epoc_conv[e-1]
  epoch_conv <- c("enroll","18mos","36mos","60mos","7yr","9yr","11yr","13yr")
  epoch2 <<- epoch_conv[e-2]
  epoch1 <<- epoch_conv[e-1]
  epoch <<- epoch_conv[e]
  ep_next <<- epoch_conv[e+1]
  ep <- epoch
  date_next <<- paste0("visit_estimate_1yr_date")
  date_ty <<- format(as.Date(pdb_data[, date_next]), "%B %Y")
  
  
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
  fb_date <<- pdb_data$feedback_date
  if (is.na(fb_date)) {feedback_date <<- "MISSING"} else {feedback_date <<- format(as.Date(fb_date), "%m/%d/%Y")}
  feedback_location <<- as.character(pdb_data[i, "feedback_location"])
  if (is.na(feedback_location)) {feedback_location <<- "MISSING"}
  
  weight <- as.integer(edc_data$weight)
  df <- data.frame(
    Test = c("Heart rate", "Blood pressure", "Height", "Weight", "Body Mass Index"),
    Test.1 = c("Heart rate", "Blood pressure", "Height", "Weight", "Body Mass Index"),
    CR = c(edc_data$echo_hrate,edc_data$echo_sbp, paste0(edc_data$height," inches"), paste0(weight," lbs"), round((weight/2.205)/((edc_data$height/39.37)^2),digits = 1)),
    CR = c(edc_data$echo_hrate,edc_data$echo_dbp, paste0(edc_data$height," inches"), paste0(weight," lbs"), round((weight/2.205)/((edc_data$height/39.37)^2),digits = 1)),
    NR = c("60-100", "<120 / <80", "n/a", "n/a", "18.5-24.9")
  )
  
  ft <- flextable(df)
  ft <- set_header_labels(ft, Test = "Test", Test.1="Test",
                          CR = paste0("Current ",Epoch," Results ", enroll_date),CR.1 = paste0("Current ",Epoch," Results ", enroll_date),
                          NR = "Normal Range*" )
  ft <- bg(ft, bg="grey",part = "header")
  ft <- fontsize(ft, j=1:5, size = 10, part="header")
  ft <- fontsize(ft, j=1:5, size = 10, part="body")
  ft <- width(ft,j = 5, width = 1)
  
  ft <- merge_h(ft,part = "header")
  ft <- merge_h_range(ft,i = 1:5,j1=1, j2=2 ,part = "body")
  ft <- merge_h_range(ft,i = 1,j1=3, j2=4 ,part = "body")
  #ft <- merge_h_range(ft,i = 1,j1=5, j2=6 ,part = "body")
  #ft <- merge_h_range(ft,i = 1,j1=7, j2=8 ,part = "body")
  #ft <- merge_h_range(ft,i = 1,j1=9, j2=10 ,part = "body")
  ft <- merge_h_range(ft,i = 3:5,j1=3, j2=4 ,part = "body")
  #ft <- merge_h_range(ft,i = 3:5,j1=5, j2=6 ,part = "body")
  #ft <- merge_h_range(ft,i = 3:5,j1=7, j2=8 ,part = "body")
  #ft <- merge_h_range(ft,i = 3:5,j1=9, j2=10 ,part = "body")
  ft <- width(ft,j = 3:4, width = .5)
  ft <- width(ft, j=1:2, width = .75)
  
   ft <- theme_box(ft)
   ft <- align(ft, align = "center", part="header")
   ft <- align(ft, align = "center", part="body")
   ft <- align(ft, i = 1:5, j = 1:2, align="left",part="body")
   for (ii in 3:(ncol(df)-1)) {if ((as.integer(df[1,ii]) < 60) | (as.integer(df[1,ii]) >100)) {ft <- bold(ft, i = 1, j = ii, bold = TRUE, part = "body")}
   if (as.integer(df[2,ii]) > 120) {ft <- bold(ft, i = 2, j = ii, bold = TRUE, part = "body")}
   if ((as.double(df[5,ii]) < 18.5) | (as.double(df[5,ii]) > 24.9)) {ft <- bold(ft, i = 5, j = ii, bold = TRUE, part = "body")}}
   evens <- c(4,6,8,10)
   #for (ii in evens) {if (as.integer(df[2,ii]) > 80) {ft <- bold(ft, i = 2, j = ii, bold = TRUE, part = "body")}}
   
   df2 <- data.frame(
     Test1 = c("Cholesterol", "Cholesterol", "Cholesterol", "Cholesterol", "Blood Sugar", "Blood Sugar", "Blood Sugar", "Thyroid", "Inflammation"),
     Test2 = c("Total","HDL", "LDL", "Triglycerides", "Hemoglobin A1C", "Fasting Insulin", "Fasting Glucose", "Thyroid Stimulating Hormone (TSH)", "High Sensitivity C-Reactive Protein"),
     CR = as.character(c(edc_data$bld_c_chol, edc_data$bld_c_hdlc, edc_data$bld_c_ldlc, edc_data$bld_c_trig,edc_data$bld_c_hgba1c, edc_data$bld_c_insulin, edc_data$bld_c_glucose, edc_data$bld_c_tsh, edc_data$bld_c_crp)),
     NR = c("<200", "men >40, women >50", "<100", "<150", "4-6.5", "<17.2", "70-110", "0.3-5.0", "0.1-3.0")
   )
   
   ft2 <- flextable(df2)
   ft2 <- width(ft2, j = 1:4, width=.9)
   ft2 <- set_header_labels(ft2, Test1 = "Test", Test2 = "Test", CR = paste0("Current ",Epoch," Results ", enroll_date), NR = "Normal Range/\nCut-off*" )
   ft2 <- merge_at(ft2, i = 1, j = 1:2, part = "header")
   ft2 <- merge_at(ft2, i = 1:4, j = 1, part = "body")
   ft2 <- merge_at(ft2, i = 5:7, j = 1, part = "body")
   ft2 <- bg(ft2, bg="grey",part = "header")
   ft2 <- fontsize(ft2, j=1, size = 9, part="body")
   ft2 <- fontsize(ft2, j=1:4, size = 10, part="header")
   ft2 <- fontsize(ft2, j=2:4, size = 10, part="body")
   ft2 <- theme_box(ft2)
   ft2 <- align(ft2, align = "center", part="header")
   ft2 <- align(ft2, align = "center", part="body")
   ft2 <- align(ft2, i=1:9, j=2, align="left",part="body")
   ft2 <- valign(ft2, i=1:9, j=3:4, valign="top", part="body")
   ft2 <- height(ft2, height = .4, part = "header")
   ft2 <- width(ft2, j = 1, width = .85)
   ft2 <- width(ft2, j = 2, width = 1.25)
   for (ii in 3:(ncol(df2)-1)) {if (as.integer(df2[1,ii]) > 200) {ft2 <- bold(ft2, i = 1, j = ii, bold = TRUE, part = "body")}
     if (if (sex == "Female"){as.integer(df2[2,ii]) < 50} else {as.integer(df2[2,ii]) < 40}) {ft2 <- bold(ft2, i = 2, j = ii, bold = TRUE, part = "body")}
     if (as.integer(df2[3,ii]) > 100) {ft2 <- bold(ft2, i = 3, j = ii, bold = TRUE, part = "body")}
     if (as.integer(df2[4,ii]) > 150) {ft2 <- bold(ft2, i = 4, j = ii, bold = TRUE, part = "body")}
     if ((as.integer(df2[5,ii]) > 6.5) | (as.integer(df2[5,ii]) < 4)) {ft2 <- bold(ft2, i = 5, j = ii, bold = TRUE, part = "body")}
     if (as.integer(df2[6,ii]) > 17.2) {ft2 <- bold(ft2, i = 6, j = ii, bold = TRUE, part = "body")}
     if ((as.integer(df2[7,ii]) > 110) | (as.integer(df2[7,ii]) < 70)) {ft2 <- bold(ft2, i = 7, j = ii, bold = TRUE, part = "body")}
     if ((as.double(df2[8,ii]) > 5) | (as.double(df2[8,ii]) < 0.3)) {ft2 <- bold(ft2, i = 8, j = ii, bold = TRUE, part = "body")}
     if ((as.double(df2[9,ii]) > 3) | (as.double(df2[9,ii]) < 0.1)) {ft2 <- bold(ft2, i = 9, j = ii, bold = TRUE, part = "body")}
   }
  
  first_name_physician1<<- pdb_data$feedback_physician1_first_name
  last_name_physician1<<- pdb_data$feedback_physician1_last_name
  credentials1<<- pdb_data$feedback_physician1_credentials
  street_address_physician1<<- pdb_data$feedback_physician1_street_address
  city_physician1<<- pdb_data$feedback_physician1_city
  state_physician1<<- pdb_data$feedback_physician1_state
  zip_physician1<<- pdb_data$feedback_physician1_zip
  first_name_physician2<<- pdb_data$feedback_physician2_first_name
  last_name_physician2<<- pdb_data$feedback_physician2_last_name
  credentials2<<- pdb_data$feedback_physician2_credentials
  street_address_physician2<<- pdb_data$feedback_physician2_street_address
  city_physician2<<- pdb_data$feedback_physician2_city
  state_physician2<<- pdb_data$feedback_physician2_state
  zip_physician2<<- pdb_data$feedback_physician2_zip
  first_name_physician3<<- pdb_data$feedback_physician3_first_name
  last_name_physician3<<- pdb_data$feedback_physician3_last_name
  credentials3<<- pdb_data$feedback_physician3_credentials
  street_address_physician3<<- pdb_data$feedback_physician3_street_address
  city_physician3<<- pdb_data$feedback_physician3_city
  state_physician3<<- pdb_data$feedback_physician3_state
  zip_physician3<<- pdb_data$feedback_physician3_zip
  
  if (is.na(first_name_physician1)) {first_name_physician1<<- ""} else {first_name_physician1 <<- paste0("     1.  ",first_name_physician1)}
  if (is.na(last_name_physician1)) {last_name_physician1<<- ""}
  if (is.na(street_address_physician1)) {street_address_physician1 <<- ""} else {street_address_physician1 <<- paste0("          ",street_address_physician1)}
  if (is.na(city_physician1)) {city_physician1 <<- ""} else {city_physician1 <<- paste0("          ",city_physician1,",")}
  if (is.na(state_physician1)) {state_physician1 <<- ""}
  if (is.na(zip_physician1)) {zip_physician1 <<- ""}

  
  # Initialize memory variables
  age <- pdb_data$age
  if (age<60) {age_r <- 1}; if (age < 70 & age >= 60) {age_r <- 2}; if (age < 80 & age >= 70) {age_r <- 3}; if (age < 90 & age >= 80) {age_r <- 4}; if (age >= 90) {age_r <- 5}
  edu <- pdb_data$education
  if (edu<=12) {edu_r <- 1}; if (edu < 16 & edu >= 13) {edu_r <- 2}; if (edu == 16) {edu_r <- 3}; if (edu >= 17) {edu_r <- 4}
  ind <- age_r + edu_r
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
  moca_ex <- read_excel("~/TAP_np_lookup.xlsx", sheet = "moca")
  moca_mean <- moca_ex[ind,1+sex_r]
  moca_sd <- moca_ex[ind,2+sex_r]
  
  craftvrs_ex <- read_excel("~/TAP_np_lookup.xlsx", sheet = "craftvrs")
  craftvrs_mean <- craftvrs_ex[ind,1+sex_r]
  craftvrs_sd <- craftvrs_ex[ind,2+sex_r]
  
  crafturs_ex <- read_excel("~/TAP_np_lookup.xlsx", sheet = "crafturs")
  crafturs_mean <- crafturs_ex[ind,1+sex_r]
  crafturs_sd <- crafturs_ex[ind,2+sex_r]
  
  craftdvr_ex <- read_excel("~/TAP_np_lookup.xlsx", sheet = "craftdvr")
  craftdvr_mean <- craftdvr_ex[ind,1+sex_r]
  craftdvr_sd <- craftdvr_ex[ind,2+sex_r]
  
  craftdre_ex <- read_excel("~/TAP_np_lookup.xlsx", sheet = "craftdre")
  craftdre_mean <- craftdre_ex[ind,1+sex_r]
  craftdre_sd <- craftdre_ex[ind,2+sex_r]
  
  udsbentc_ex <- read_excel("~/TAP_np_lookup.xlsx", sheet = "udsbentc")
  udsbentc_mean <- udsbentc_ex[ind,1+sex_r]
  udsbentc_sd <- udsbentc_ex[ind,2+sex_r]
  
  udsbentd_ex <- read_excel("~/TAP_np_lookup.xlsx", sheet = "udsbentd")
  udsbentd_mean <- udsbentd_ex[ind,1+sex_r]
  udsbentd_sd <- udsbentd_ex[ind,2+sex_r]
  
  digforct_ex <- read_excel("~/TAP_np_lookup.xlsx", sheet = "digforct")
  digforct_mean <- digforct_ex[ind,1+sex_r]
  digforct_sd <- digforct_ex[ind,2+sex_r]
  
  digforsl_ex <- read_excel("~/TAP_np_lookup.xlsx", sheet = "digforsl")
  digforsl_mean <- digforsl_ex[ind,1+sex_r]
  digforsl_sd <- digforsl_ex[ind,2+sex_r]
  
  digbacct_ex <- read_excel("~/TAP_np_lookup.xlsx", sheet = "digbacct")
  digbacct_mean <- digbacct_ex[ind,1+sex_r]
  digbacct_sd <- digbacct_ex[ind,2+sex_r]
  
  digbacls_ex <- read_excel("~/TAP_np_lookup.xlsx", sheet = "digbacls")
  digbacls_mean <- digbacls_ex[ind,1+sex_r]
  digbacls_sd <- digbacls_ex[ind,2+sex_r]
  
  minttots_ex <- read_excel("~/TAP_np_lookup.xlsx", sheet = "minttots")
  minttots_mean <- minttots_ex[ind,1+sex_r]
  minttots_sd <- minttots_ex[ind,2+sex_r]
  
  udsverfc_ex <- read_excel("~/TAP_np_lookup.xlsx", sheet = "udsverfc")
  udsverfc_mean <- udsverfc_ex[ind,1+sex_r]
  udsverfc_sd <- udsverfc_ex[ind,2+sex_r]
  
  udsverlc_ex <- read_excel("~/TAP_np_lookup.xlsx", sheet = "udsverlc")
  udsverlc_mean <- udsverlc_ex[ind,1+sex_r]
  udsverlc_sd <- udsverlc_ex[ind,2+sex_r]
  
  udsvertn_ex <- read_excel("~/TAP_np_lookup.xlsx", sheet = "udsvertn")
  udsvertn_mean <- udsvertn_ex[ind,1+sex_r]
  udsvertn_sd <- udsvertn_ex[ind,2+sex_r]
  
  animals_ex <- read_excel("~/TAP_np_lookup.xlsx", sheet = "animals")
  animals_mean <- animals_ex[ind,1+sex_r]
  animals_sd <- animals_ex[ind,2+sex_r]
  
  veg_ex <- read_excel("~/TAP_np_lookup.xlsx", sheet = "veg")
  veg_mean <- veg_ex[ind,1+sex_r]
  veg_sd <- veg_ex[ind,2+sex_r]
  
  traila_ex <- read_excel("~/TAP_np_lookup.xlsx", sheet = "traila")
  traila_mean <- traila_ex[ind,1+sex_r]
  traila_sd <- traila_ex[ind,2+sex_r]
  
  trailb_ex <- read_excel("~/TAP_np_lookup.xlsx", sheet = "trailb")
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
  
  # composite scores
  mem_ver<<- cbind(craftvrs_z,crafturs_z,craftdvr_z,craftdre_z)
  memory_verbal<<- rowMeans(mem_ver)
  mem_vis<<- cbind(udsbentc_z, udsbentd_z)
  memory_visual<<- rowMeans(mem_vis)
  lang<<- cbind(minttots_z,animals_z,veg_z)
  language<- rowMeans(lang)
  ex_func<<- rowMeans(cbind(digbacct_z, digbacls_z, udsverfc_z, udsverlc_z, udsvertn_z, trailb_z))
  attention<- rowMeans(cbind(digforct_z, digforsl_z, traila_z))
  
  counts<- c(memory_verbal,memory_visual,language,ex_func,attention)
  
  specie<- c("Memory for\n Words","Memory for\n Shapes","Language","Multi-tasking\n and planning","Attention")
  condition<- rep(c(enroll_date),5)
  level_order<- c("Memory for\n Words","Memory for\n Shapes","Language","Multi-tasking\n and planning","Attention")
  cond_order<- c(enroll_date)
  spec<- factor(specie,levels = level_order)
  cond<- factor(condition,levels = cond_order)
  data<- data.frame(specie,condition,counts)
  countss<- counts+3
  patterns<- rep(c('crosshatch'),5)
  pattern_factor<- c('crosshatch')
  patternss<- factor(patterns,levels = pattern_factor)
  for (ii in 1:length(countss)) {if (countss[ii]<0.25) {countss[ii]<-0.25}}
  
  barr <- ggplot(data,aes(fill=cond, y=countss, x=spec,pattern_key_scale_factor=patterns)) + theme_bw(14)+scale_fill_manual(values = c("black"))+
    geom_bar_pattern(width = .75,color=rep(c("black"),5),aes(pattern=cond),pattern_density=rep(c(0.5),5),pattern_colour=rep(c("black"),5),pattern_fill = rep(c("white"),5),pattern_spacing=.02,position="dodge", stat="identity",pattern_key_scale_factor = 1) +
    scale_y_continuous(name=NULL,breaks=c(1.5,3,4.5),labels = c("Below\nNormal","Normal","Above\nNormal"),limits = c(0,6),expand = c(0, 0))+
    scale_x_discrete(name=NULL) + theme(panel.spacing.x = unit(1,"lines"),plot.background = element_rect(fill = "white",colour = "black",size = 1),plot.margin = unit(c(.3, .8, .3, .3), "cm"),legend.key.size = unit(.21, 'cm'),panel.grid.major.y = element_line(c(1.5,3,4.5),color=c("black","black","black")),panel.grid.minor.y=element_blank(),panel.grid.major.x=element_blank(),axis.text = element_text(colour = "black"),plot.title = element_text(size = 14,face = "bold",hjust = 0.5),axis.ticks = element_blank(),legend.position = "bottom", legend.title = element_blank(),legend.box.margin=margin(-15,0,0,0))+
    coord_fixed(ratio = .4) + labs(title = "Memory Testing Results")
  
  ptp_temp<<- paste0("C:/Users/sweelyb/Documents/resources/output/ptp_temp.docx")
  phys_temp<<- paste0("C:/Users/sweelyb/Documents/resources/output/phys_temp.docx")
  Plots<<- list(membar = function() print(barr))
  ptp_path<<- paste0("C:/Users/sweelyb/Documents/resources/Templates/Feedback/TAP_fb_temp_",epoch,".docx")
  phys_path<<- paste0("C:/Users/sweelyb/Documents/resources/Templates/Feedback/TAP_phys_temp_",epoch,".docx")
  addPlots(ptp_path,ptp_temp,Plots,width = 7.3,height = 3.6)
  addPlots(phys_path,phys_temp,Plots,width = 7.3,height = 3.6)
  FT<<- list(ft = ft, ft2 = ft2)
  body_add_flextables(ptp_temp,ptp_temp, FT)
  body_add_flextables(phys_temp,phys_temp, FT)
  
  lv<<- as.character(edc_data$echo_find1_lv_dys_fx)
  val<<- as.character(edc_data$echo_find2_valve_fx)
  if (val=="Normal"){val <<-"     3.  No significant"} else {val <<-"     3.  Significant"}
  lv <<- paste0("     1.  ", lv)
  
  
  gds <- edc_data$qds_total
  if (gds > 4) {
    int <- "mild"
    if (gds > 8) {int <- "moderate"}
    if (gds > 11) {int <- "severe"}
    gds_phys <<- paste0("On a measure assessing depressive symptoms, ",first_name," scored in a range suggesting ",int," symptoms of depression. Based upon this score, we recommended that ",first_name," make an appointment for a more detailed clinical assessment of these symptoms.")
    gds <<- paste0("As discussed on ",feedback_date1,", your scores on a measure assessing depressive symptoms fell in a range suggesting ",int," symptoms of depression.  We recommend you make an appointment for a more detailed clinical assessment of these symptoms.  You can request a referral from your primary care doctor.  We would recommend our colleagues who offer clinical services in the Department of Psychiatry at Vanderbilt University.  You can schedule an appointment by calling: 615-936-3555.")
  } else {gds<<- ""; gds_phys<<- ""}
  
  if (any(countss < 1.5)) {
    decline_phys<<- paste0("Based upon the cognitive scores, we suggested that ",first_name," follow-up with a clinical memory work-up for more detailed memory testing. This evaluation can be completed by our colleagues in the Cognitive & Behavioral Neurology Division at Vanderbilt University Medical Center, with a doctor\'s referral (615-936-0060). ")
    decline<<- paste0("As we discussed on ",enroll_date,", we recommend that you make an appointment for a clinical memory workup for more detailed cognitive testing. You can request a referral from your primary care doctor. We would recommend our colleagues in the Cognitive & Behavioral Neurology Division at Vanderbilt University Medical Center. With a doctor's referral, you can schedule an appointment by calling: 615-936-0060.")
  } else {decline<<- ""; decline_phys<<- ""}
  
  #if (edc_data["extracardiac_incidental"]=="Yes") {lung36<<-paste("     4.  ",fii36["extracardiac_incidental_describe"])} else {lung36<<-""}
  
  bi <- edc_data["brain_incidental"]
  if (is.na(bi)) {bi <- "No"}
  if (bi=="Yes") {
    brain_intro1<<-"Brain Test Results"
    brain_intro2<<-"You underwent brain testing, which was read by board-certified neuroradiologists."
  } else {brain_intro1<<-""; brain_intro2<<-""}
  
  if (bi=="Yes") {brain_ic<<-paste0("Your _ results on ",enroll_date," were as follows:"); brain_c<<-edc_data$brain_incidental_davis} else {brain_ic<<-""; brain_c<<-""}
  
  #if (nchar(map_id)==1) {input<<- paste0("00",map_id)} else if (nchar(map_id)==2) {input<<- paste0("0",map_id)} else {input<<- map_id}
  vmac_id<<- as.character(pdb_data$vmac_id)
  if (nchar(vmac_id)==1) {record<<- paste0("0000",vmac_id)} else if (nchar(vmac_id)==2) {record<<- paste0("000",vmac_id)} else if (nchar(vmac_id)==3) {record<<- paste0("00",vmac_id)} else if (nchar(vmac_id)==4) {record<<- paste0("0",vmac_id)} else {record<<- vmac_id}
  input <- record
  output<- paste0("C:/Users/sweelyb/Documents/resources/output/feedback_letter_TAP_",input,"_",epoch,".docx")
  renderInlineCode(ptp_temp, output)
  
  #importFiles(rcon = pdb, file = output, record = record, field = "feedback_letter", event = events[e],
              #overwrite = TRUE, repeat_instance = 1)
  print("Imported File")
  
  if (is.na(first_name_physician1)==FALSE) {
    first_name_physician<<-first_name_physician1
    last_name_physician<<- last_name_physician1
    credentials<<- credentials1
    street_address_physician<<- street_address_physician1
    city_physician<<- city_physician1
    state_physician<<- state_physician1
    zip_physician<<- zip_physician1
    
    output<- paste0("C:/Users/sweelyb/Documents/resources/output/physician_letter_MAP_",input,"_",epoch,".docx")
    renderInlineCode(phys_temp, output)
    
    #importFiles(rcon = pdb, file = output, record = record, field = "feedback_physician1_letter", event = events[e],
                #overwrite = TRUE, repeat_instance = 1)
  }
}
